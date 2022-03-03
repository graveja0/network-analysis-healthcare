calculate_concentration_measures <- function(
    area_denom, 
    target_area,
    collapse_to_system_level = TRUE ,
    system_radius = 100,
    minimum_market_share = 0.01,
    minimum_market_size = 25,
    km = km
) {
    
    sf <- load_target_sf(target_area = target_area )
    zips_in_area <- sf_zip_cent$zip_code[!is.na(as.numeric(st_within(sf_zip_cent,sf)))] 
    
    # First we get an edge list that selects on the ZIP codes with centroids in the area.
    
    if (collapse_to_system_level) {
        
        edge_list_geography <- 
            edge_list_geography_ %>% 
            filter(zip_code %in%  zips_in_area) %>% 
            inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(zip_code, geoid), c("zip_code")) %>% 
            group_by(geoid,prvnumgrp) %>% 
            summarise(total_cases = sum(total_cases, na.rm=TRUE)) %>% 
            select(from= geoid, to = prvnumgrp, weight = total_cases) %>% 
            ungroup() %>% 
            mutate(total_hospitalizations = sum(weight)) %>% 
            group_by(to) %>% 
            mutate(market_share = sum(weight) / total_hospitalizations) %>% 
            mutate(market_size = sum(weight))
        
        # First get the hospitals
        hospitals_to_include_ <- 
            edge_list_geography %>% 
            filter(market_share > minimum_market_share | market_size > minimum_market_size) %>% 
            pull(to) %>% 
            unique()
        
        hospitals_to_include <- hospitals_to_include_orig <- 
            df_node_hosp_ %>% 
            filter(id %in% hospitals_to_include_ ) %>% 
            mutate(within_area = as.integer(geoid %in% target_area))
        
        systems_to_include_ <-
            df_node_hosp_ %>%
            filter(id %in% hospitals_to_include$id) %>%
            data.frame() %>% 
            ungroup() %>%
            select(sysname) %>%
            unique()
        
        systems_to_include_ <-  # These are the total number of hospitals within the systems that are included in the target area.
            df_node_hosp_ %>%
            filter(!is.na(id) & id!="") %>% 
            filter(sysname %in% unique(systems_to_include_$sysname)) %>% 
            unique()
        
        # We now limit to outside hospitals within a 100 mile radius of any hospital within the target area. 
        systems_to_include <-
            st_distance(hospitals_to_include, systems_to_include_) %>%
            data.frame() %>%
            mutate(id = hospitals_to_include$id) %>%
            set_names(c("id", systems_to_include_$id)) %>%
            gather(target_id,meters,-id) %>%
            as_tibble() %>%
            mutate(meters=as.numeric(paste0(meters))) %>%
            mutate(miles = meters * 0.000621371) %>%
            filter(miles < system_radius ) %>%
            select(id = target_id,miles) %>%
            group_by(id) %>%
            arrange(id, minimum_miles_to_nearest = miles) %>%
            filter(row_number()==1) %>%
            inner_join(
                df_node_hosp_,"id"
            ) %>%
            group_by(sysname) %>% 
            mutate(within_area = as.numeric(geoid %in% target_area), na.rm=TRUE) %>%
            ungroup()
        
        hospitals_to_include <- 
            systems_to_include 
        
    } else {
        edge_list_geography <- 
            edge_list_geography_ %>% 
            filter(zip_code %in%  zips_in_area) %>% 
            inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(zip_code, geoid), c("zip_code")) %>% 
            group_by(geoid,prvnumgrp) %>% 
            summarise(total_cases = sum(total_cases, na.rm=TRUE))  %>% 
            select(from= geoid, to = prvnumgrp, weight = total_cases) %>% 
            ungroup() %>% 
            mutate(total_hospitalizations = sum(weight)) %>% 
            group_by(to) %>% 
            mutate(market_share = sum(weight) / total_hospitalizations) %>% 
            mutate(market_size = sum(weight))  
        
        # To get a geographic HHI we need a roster of hospitals served by the area.
        # We limit here to hospitals with a 1% or greater market share, or at least 25 patients from the
        # ZIP code. 
        
        hospitals_to_include_ <- 
            edge_list_geography %>% 
            filter(market_share > minimum_market_share | market_size > minimum_market_size) %>% 
            pull(to) %>% 
            unique()
        
        hospitals_to_include <- 
            df_node_hosp_ %>% 
            filter(id %in% hospitals_to_include_ ) %>% 
            mutate(within_area = as.integer(geoid %in% target_area))
        
    }
    
    G_hosp_ <- 
        edge_list_geography %>% 
        filter(to %in% hospitals_to_include$id) %>% 
        graph.data.frame(directed = TRUE) %>% 
        as_tbl_graph() %>% 
        activate(nodes) 

    # nodes <- G_hosp_ %>% activate(nodes) %>% select(name) %>% data.frame()
    # foo <- G_hosp_   %>% activate(nodes) %>% data.frame()
    # 
    G_hosp_ <-
        G_hosp_  %>% 
        activate(nodes) %>% 
        inner_join(
            df_node_hosp_ %>%
                bind_cols(st_coordinates(.) %>% as.tibble() %>% set_names(c("x","y"))) %>%
                rename(name = id) %>%
                bind_rows(sf_zip_cent %>% rename(name = geoid) %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(name) ) %>% 
                unique(),"name"  
        ) %>% 
        mutate(from = row_number(), 
               to = row_number())
    
    G_hosp <- 
        G_hosp_ %>% 
        activate(edges) %>% 
        left_join(G_hosp_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
        left_join(G_hosp_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name,system_id), "to")
    
    ### OUTFLOW HHI
    if (!collapse_to_system_level) {
        outflow_hhi <- G_hosp %>% get_outflow_hhi(overall = FALSE, collapse = FALSE)
    }  else {
        outflow_hhi <- G_hosp %>% get_outflow_hhi(overall = FALSE, collapse = TRUE) 
    }
    outflow_hhi <- 
        outflow_hhi %>% 
        data.frame() %>% 
        set_names(c("outflow")) %>% 
        rownames_to_column(var = "geoid")
    
    ############################################################################################
    # Inflow HHI:  From the perspective of the geographic location of hospitals, i.e., 
    # what is the HHI among hospitals located within a given geographic community c
    #? This measure considers market concentration in terms of patient flows into the geography.
    ############################################################################################
    
    hospitals_within <- 
        hospitals_to_include %>% 
        mutate(within_area = as.integer(denomid %in% target_area)) %>% 
        filter(within_area==1) %>% 
        group_by(geoid) %>% 
        nest() %>% 
        mutate(n_hosps = map_dbl(data,~(.x %>% nrow()))) %>% 
        mutate(n_systems = map_dbl(data,~(.x %>% pull(system_id) %>% unique() %>% length()))) %>% 
        filter(n_hosps>=1) %>% 
        mutate(in_edgelist = map_dbl(data,~(.x %>% pull(id) %in% unique(edge_list_geography_$prvnumgrp) %>% max()))) %>% 
        filter(in_edgelist==1)
    
    inflow_hhi <-
        hospitals_within %>%
        mutate(inflow_hhi = map_dbl(data,~({

            edge_list_hospital <- 
                edge_list_geography_ %>% 
                filter(prvnumgrp %in% .x$id)  %>% 
                inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(zip_code, geoid), c("zip_code")) %>% 
                group_by(geoid,prvnumgrp) %>% 
                summarise(total_cases = sum(total_cases, na.rm=TRUE)) %>% 
                select(from= geoid, to = prvnumgrp, weight = total_cases) %>% 
                ungroup() 

            G_hosp_within_ <- 
                edge_list_hospital %>% 
                unique() %>% 
                graph.data.frame(directed = TRUE) %>% 
                as_tbl_graph() %>% 
                activate(nodes) %>% 
                mutate(from = row_number(), 
                       to = row_number())
            
            G_hosp_within <- 
                G_hosp_within_ %>% 
                activate(edges) %>% 
                left_join(G_hosp_within_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
                left_join(G_hosp_within_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name), "to") %>% 
                left_join(df_node_hosp_ %>% select(to_name=id, sysname,mname),"to_name") %>% 
                activate(nodes) %>% 
                left_join(df_node_hosp_ %>% select(name=id, sysname,mname),"name")
            
            if (collapse_to_system_level) {
                inflow_hhi <- G_hosp_within %>% get_inflow_hhi(collapse = TRUE)
            } else {
                inflow_hhi <- G_hosp_within %>% get_inflow_hhi(collapse = FALSE)
            }
            inflow_hhi
        
    
    })))
    
    areas_to_calculate <- 
        sf_zip_cent %>% 
        filter(zip_code %in% zips_in_area) %>% 
        pull(geoid) %>% 
        unique()

    hhi_km <- 
        km$zip %>% 
        select(zip_code,hhi_star_z) %>% 
            inner_join(
                edge_list_geography_ %>% 
                    filter(zip_code %in% zips_in_area) %>% 
                    inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(zip_code, geoid) %>% unique() , "zip_code") %>% 
                    group_by(zip_code, geoid) %>% 
                    summarise(total_cases = sum(total_cases)) %>% 
                    data.frame()
            ) %>% 
            group_by(geoid) %>% 
            summarise(hhi_km = weighted.mean(hhi_star_z,total_cases))

    tag <- ifelse(collapse_to_system_level,"sys","hosp")
    final <- 
        outflow_hhi %>% 
        rename(outflow_hhi = outflow) %>% 
        left_join(
            inflow_hhi %>% 
                select(geoid,n_hosps,n_systems,inflow_hhi),"geoid"
        ) %>% 
        left_join(   hhi_km,"geoid") %>% 
        select(geoid,contains("hhi"),everything()) %>% 
        as_tibble() %>% 
        rename_at(vars(contains("hhi")),function(x) paste0(x,"_",tag))
    
    return(final )
    
}














# # We can also get a system-level HHI measure since many of these hospitals are in the same system.
# # For this, we want to pull in any hospitals that are within some defined radius (e.g., 100 miles),
# # not necessarily to include EVERY hospital in the system. 
# 
# systems_to_include_ <- 
#     df_node_hosp_ %>% 
#     filter(id %in% hospitals_to_include$id) %>% 
#     ungroup() %>% 
#     select(sysname) %>% 
#     unique()
# 
# systems_to_include_ <- 
#     df_node_hosp_ %>% 
#     filter(sysname %in% systems_to_include_$sysname)
# 
# systems_to_include <- 
#     st_distance(hospitals_to_include, systems_to_include_) %>% 
#     data.frame() %>% 
#     mutate(id = hospitals_to_include$id) %>% 
#     set_names(c("id", systems_to_include_$id)) %>% 
#     gather(target_id,meters,-id) %>% 
#     as_tibble() %>% 
#     mutate(meters=as.numeric(paste0(meters))) %>% 
#     mutate(miles = meters * 0.000621371) %>% 
#     filter(miles < maximum_system_radius ) %>% 
#     select(id = target_id,miles) %>% 
#     group_by(id) %>% 
#     arrange(id, minimum_miles_to_nearest = miles) %>% 
#     filter(row_number()==1) %>% 
#     inner_join(
#         df_node_hosp_,"id"
#     ) %>% 
#     mutate(within_area = as.numeric(geoid == target_area)) %>% 
#     ungroup()
