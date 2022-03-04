get_adjacency_matrix <- function(
    application_area,
    area_type,
    row_type,
    
    # area_denom, 
    # target_area,
    collapse_to_system_level = TRUE ,
    system_radius = 100,
    minimum_market_share = 0.01,
    minimum_market_size = 25,
    km = km
) {
    
    if (row_type=="county") idname = quo("fips_code") 
    if (row_type=="commuting-zone") idname = quo("cz_id")
    
    sf <- load_target_sf(target_area = application_area)
    zips_in_area <- sf_zip_cent$zip_code[!is.na(as.numeric(st_within(sf_zip_cent,sf)))] 
    
    # First we get an edge list that selects on the ZIP codes with centroids in the area.
    
    if (collapse_to_system_level) {
        
        # Roll up to the commuting zone level to identify the hosptial systems that actually serve the area. 
        
        edge_list_geography <- 
            edge_list_geography_ %>% 
            filter(zip_code %in%  zips_in_area) %>% 
            inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(zip_code, geoid = {{idname}}), c("zip_code")) %>% 
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
            mutate(within_area = as.integer(denomid %in% application_area)) %>% 
            st_as_sf(crs = 4326)
        
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
            unique() %>% 
            st_as_sf(crs=4326)
        
        # We now limit to outside hospitals within a 100 mile radius of any hospital within the target area. 
        tmp <- hospitals_to_include %>% filter(within_area==1)
        
        systems_to_include <-
            suppressWarnings({
                st_distance(tmp, systems_to_include_) %>%
                    data.frame() %>%
                    mutate(id = tmp$id) %>%
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
                    mutate(within_area = as.numeric(denomid %in% application_area), na.rm=TRUE) %>%
                    ungroup()
            })
           
        
        orig_hospitals_to_include <- hospitals_to_include
        
        hospitals_in_target_area <- 
            hospitals_to_include %>% 
            filter(denomid %in% application_area) 
        
        hospitals_to_include <- 
            systems_to_include %>% 
            rename(geoid = {{idname}})
        
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
            mutate(within_area = as.integer(geoid %in% target_area)) %>% 
            rename(geoid = {{idname}})
        
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
                st_as_sf(crs = 4326) %>% 
                bind_cols(st_coordinates(.) %>% as.tibble() %>% set_names(c("x","y"))) %>%
                rename(name = id) %>%
                bind_rows(sf_zip_cent %>% rename(name = {{idname}}) %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(name) %>% 
                              mutate(name = paste0(name))) %>% 
                unique(),"name"  
        ) %>% 
        mutate(from = row_number(), 
               to = row_number())
    
    G_hosp <- 
        G_hosp_ %>% 
        activate(edges) %>% 
        left_join(G_hosp_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
        left_join(G_hosp_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name,system_id), "to")
    
    if (collapse_to_system_level) {
        B <- G_hosp %>% get_collapsed_incidence_matrix( x= system_id)
        return(list(G = G_hosp, B = B, hospitals = hospitals_to_include, systems = hospitals_to_include %>% select(sysname,miles,system_id) %>% 
                        unique(), row_type = row_type))
    } else {
        B <- G_hosp %>% get_incidence_matrix()
        return(list(G = G_hosp, B = B, hospitals = hospitals_to_include, row_type = row_type))
    }

}

