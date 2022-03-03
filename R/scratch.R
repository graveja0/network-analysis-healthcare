collapse = TRUE

edge_list_hospital_zip <- 
    edge_list_geography_ %>% 
    select(from= zip_code, to = prvnumgrp, weight = total_cases) 

G_hosp_zip_ <- 
    edge_list_hospital_zip %>% 
    graph.data.frame(directed = TRUE) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    mutate(from = row_number(), 
           to = row_number())

G_hosp_zip <- 
    G_hosp_zip_ %>% 
    activate(edges) %>% 
    left_join(G_hosp_zip_ %>% activate(nodes) %>% data.frame() %>% select(from,from_name = name), "from") %>% 
    left_join(G_hosp_zip_ %>% activate(nodes) %>% data.frame() %>% select(to,to_name = name), "to") %>% 
    left_join(df_node_hosp_ %>% select(to_name=id, sysname),"to_name") %>% 
    activate(nodes) %>% 
    left_join(df_node_hosp_ %>% select(name=id, sysname),"name")


