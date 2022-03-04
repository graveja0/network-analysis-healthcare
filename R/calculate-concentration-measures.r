calculate_concentration_measures <- function(
    B,
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
    
    G_hosp = B$G
    hospitals_to_include = B$hospitals
    row_type = B$row_type
    
    if (row_type=="county") idname = quo("fips_code") 
    if (row_type=="commuting-zone") idname = quo("cz_id")
    

    ### OUTFLOW HHI
    if (!collapse_to_system_level) {
        outflow_hhi <- G_hosp %>% get_outflow_hhi_orig(overall = FALSE, collapse = FALSE)
    }  else {
        outflow_hhi <- G_hosp %>% get_outflow_hhi_orig(overall = FALSE, collapse = TRUE) 
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
        filter(within_area==1) %>% 
        group_by(geoid) %>% 
        nest() %>% 
        mutate(n_hosps = map_dbl(data,~(.x %>% nrow()))) %>% 
        mutate(n_systems = map_dbl(data,~(.x %>% pull(system_id) %>% unique() %>% length()))) %>% 
        filter(n_hosps>=1) %>% 
        mutate(in_edgelist = map_dbl(data,~(.x %>% pull(id) %in% unique(edge_list_geography_$prvnumgrp) %>% max()))) %>% 
        filter(in_edgelist==1)
    
    # inflow_hhi <-
    #     hospitals_within %>%
    #     mutate(inflow_hhi = map_dbl(data,~({
    # 
    #         edge_list_hospital <- 
    #             edge_list_geography_ %>% 
    #             filter(prvnumgrp %in% .x$id)  %>% 
    #             inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(-geometry) %>% select(zip_code, geoid), c("zip_code")) %>% 
    #             group_by(geoid,prvnumgrp) %>% 
    #             summarise(total_cases = sum(total_cases, na.rm=TRUE)) %>% 
    #             select(from= geoid, to = prvnumgrp, weight = total_cases) %>% 
    #             ungroup() 
    # 
    #         G_hosp_within_ <- 
    #             edge_list_hospital %>% 
    #             unique() %>% 
    #             graph.data.frame(directed = TRUE) %>% 
    #             as_tbl_graph() %>% 
    #             activate(nodes) %>% 
    #             mutate(from = row_number(), 
    #                    to = row_number())
    #         
    #         G_hosp_within <- 
    #             G_hosp_within_ %>% 
    #             activate(edges) %>% 
    #             left_join(G_hosp_within_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
    #             left_join(G_hosp_within_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name), "to") %>% 
    #             left_join(df_node_hosp_ %>% select(to_name=id, sysname,mname),"to_name") %>% 
    #             activate(nodes) %>% 
    #             left_join(df_node_hosp_ %>% select(name=id, sysname,mname),"name")
    #         
    #         if (collapse_to_system_level) {
    #             inflow_hhi <- G_hosp_within %>% get_inflow_hhi(collapse = TRUE)
    #         } else {
    #             inflow_hhi <- G_hosp_within %>% get_inflow_hhi(collapse = FALSE)
    #         }
    #         inflow_hhi
    #     
    # 
    # })))
    
    areas_to_calculate <- 
        sf_zip_cent %>% 
        filter(zip_code %in% zips_in_area) %>% 
        pull({{idname}}) %>% 
        unique()

    hhi_km <- 
        km$zip %>% 
        select(zip_code,hhi_star_z) %>% 
            inner_join(
                edge_list_geography_ %>% 
                    filter(zip_code %in% zips_in_area) %>% 
                    inner_join(sf_zip_cent %>% data.frame() %>% ungroup() %>% select(zip_code, geoid={{idname}}) %>% unique() , "zip_code") %>% 
                    group_by(zip_code, geoid) %>% 
                    summarise(total_cases = sum(total_cases)) %>% 
                    data.frame()
            ) %>% 
            group_by(geoid) %>% 
            summarise(hhi_km = weighted.mean(hhi_star_z,total_cases))

    tag <- ifelse(collapse_to_system_level,"sys","hosp")
    final <- 
        outflow_hhi %>% 
        mutate(geoid = paste0(geoid)) %>% 
        rename(outflow_hhi = outflow) %>% 
        # left_join(
        #     inflow_hhi %>% 
        #         select(geoid,n_hosps,n_systems,inflow_hhi),"geoid"
        # ) %>% 
        left_join(   hhi_km %>% mutate(geoid=paste0(geoid)),"geoid") %>% 
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
