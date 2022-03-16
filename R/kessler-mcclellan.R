 
collapse = TRUE

if (collapse) {
    output_file <- glue("output/results/kessler-mcclellan-{application_year}-edgelist/km-{application_year}-edgelist-system.rds")
    
    } else {
    output_file <- glue("output/results/kessler-mcclellan-{application_year}-edgelist/km-{application_year}-edgelist-hospital.rds")
    
}

collapsed_output_file <- glue("output/results/kessler-mcclellan-{application_year}-edgelist/km-{application_year}-edgelist-system.rds")
hospital_output_file <-glue("output/results/kessler-mcclellan-{application_year}-edgelist/km-{application_year}-edgelist-hospital.rds")

if (!file.exists(here(output_file))) {

    sf <- sf_
    zips_in_area <- sf_zip_cent$zip_code
    
    edge_list_hospital_zip <- 
    edge_list_geography_ %>% 
    select(from= zip_code, to = prvnumgrp, weight = total_cases) 
    
    # 3. 
    G_ <- 
    edge_list_hospital_zip %>% 
    graph.data.frame(directed = TRUE) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    mutate(from = row_number(), 
           to = row_number())
    
    G <- 
    G_ %>% 
    activate(edges) %>% 
    left_join(G_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
    left_join(G_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name), "to") %>% 
    left_join(df_node_hosp_ %>% select(to_name=id, system_id),"to_name") %>% 
    activate(nodes) %>% 
    left_join(df_node_hosp_ %>% select(name=id, system_id),"name")
    
    if (!collapse) {
    B <- G %>% get_incidence_matrix()
    } else {
    B <- G %>% get_collapsed_incidence_matrix(x = system_id)
    }
    
    # B has one row for each ZIP code with positive (predicted or actual) patient flows into hospitals within the 
    # target area.
    # B has columns that include all these within-area hospitals, but also any other hospital served by the ZIPs.
    dim(B)
    
    # S measures the share of patients from ZIP k going to hosptial j (/hat /alpha_{jk} in KM)
    k_z <- apply(B,1,sum)
    k_j <- apply(B,2,sum)
    S_z <- (as.matrix(diag(1/k_z),sparse=TRUE) %*% as.matrix(B,sparse=TRUE))
    rownames(S_z) <- rownames(B)
    
    # Now get HHI_k^pat
    hhi_z <- apply((100 *S_z) * (100 * S_z),1,sum)
    names(hhi_z) <- rownames(B)
    
    # Now get each hospital's share of demand from each ZIP
    S_h <- (as.matrix(B,sparse=TRUE) %*%  as.matrix(diag(1/k_j),sparse=TRUE))
    colnames(S_h) <- colnames(B)
    rownames(S_h) <- rownames(B)
    
    hhi_j <- as.matrix(diag(hhi_z),sparse=TRUE) %*% S_h %>% apply(.,2,sum)
    names(hhi_j) <- colnames(B)
    
    # Now get back to the ZIP level # HHI^star_k^pat
    hhi_z_final <- diag(hhi_j)  %*% t(S_z)  %>% apply(.,2,sum)
    names(hhi_z_final) <- rownames(B)
    
    out1 <- data.frame(zip_code = zips_in_area) %>% 
        mutate(hhi_z = hhi_z[zip_code],
               hhi_star_z = hhi_z_final[zip_code]) %>% 
        as_tibble()
    
    if (collapse) {
        out2 <- data.frame(system_id = colnames(B), hhi_j = hhi_j[colnames(B)]) %>% 
            inner_join(df_node_hosp_  %>% data.frame() %>% ungroup() %>% select(system_id,sysname) %>% unique(),"system_id")
        as_tibble()  
    } else {
        out2 <- data.frame(prvnumgrp = colnames(B), hhi_j = hhi_j[colnames(B)]) %>% 
            inner_join(df_node_hosp_  %>% data.frame() %>% ungroup() %>% select(prvnumgrp=id,system_id,sysname) %>% unique(),"prvnumgrp")
        as_tibble()    
    }

    
    # Area-wide average
    values <- hhi_z_final[zips_in_area] %>% na.omit()
    weights <- apply(B[intersect(rownames(B),zips_in_area),],1,sum)
    
    out3 <- weighted.mean(values[names(weights)],weights)
    
    # return(list(area = out3, 
    #             zip = out1, 
    #             hosp = out2))
    # 
    km <- list(area = out3,
               zip = out1,
               hosp = out2)
    if (collapse) {
        km %>% write_rds(here(output_file))
    }
    
} 
    

