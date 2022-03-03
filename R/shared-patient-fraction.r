# 
# gg <- gr_clin %>%
#     activate(nodes) %>%
#     filter(specialty_group %in% c("primcare","cardiology"))  %>%
#     activate(edges) %>%
#     inner_join(gr_clin %>% activate(nodes) %>% data.frame() %>% mutate(from=row_number()) %>% select(from,from_name = name,from_type=specialty_group)) %>%
#     inner_join(gr_clin %>% activate(nodes) %>% data.frame() %>% mutate(to=row_number()) %>% select(to,to_name = name,to_type=specialty_group)) %>%
#     filter(from_type=="primcare" & to_type=="cardiology")
# 
# # mm = quo(hsanum)
# id = quo(name)
# is_factor = FALSE
# 
# mm = gr_clin %>% 
#     activate(nodes) %>% 
#     data.frame() %>% 
#     select(starts_with("sg")) %>% 
#     names() %>% unlist()
#     

get_shared_patient_fraction <- function(gg,mm, is_factor = TRUE, id) {
    if (is_factor) mm <- enquo(mm) else mm <- syms(mm)
    id <- enquo(id)
    
    node_list <- gg %>% activate(nodes) %>% data.frame()
    edge_list <- gg %>% activate(edges) %>% data.frame()
    
    bp <- 
        edge_list %>% 
        select(from_name,to_name,weight) %>% 
        spread(to_name,weight) %>% 
        column_to_rownames(var = "from_name") %>% 
        as.matrix()
    bp[is.na(bp)] <- 0
    
    if (is_factor) {
        MM <- 
            node_list %>% 
            select({{id}},{{mm}}) %>% 
            fastDummies::dummy_cols(quo_name(mm)) %>% 
            select(-{{mm}}) %>% 
            column_to_rownames(var = quo_name(id))
    } else {
        MM <-
        node_list %>% 
            select_at(c(quo_name(id),paste0(mm))) %>% 
            column_to_rownames(var = quo_name(id))
    }
    
    res <- res2 <- matrix(0,nrow=nrow(bp),ncol=ncol(MM),dimnames=list(rownames(bp),colnames(MM)))
    
    for (cc in 1:ncol(MM)) {
        mm_ <- as.matrix(MM[c(rownames(bp)),cc]) %*% t(as.matrix(MM[c(colnames(bp)),cc])) 
        
        res[,cc] <- apply(bp * mm_,1,sum) 
        res2[,cc] <- apply(bp,1,sum) * MM[rownames(bp),cc]
    }
    
    out <- list(
        overall = sum(res)/sum(bp),
        group = apply(res,2,sum) / apply(res2,2,sum)#,
        # num = res,
        # denom = res2
    )
    return(out)
}

get_shared_patient_fraction_ <- function(gg,mm, is_factor = TRUE, id) {
    mm <- enquo(mm)
    id <- enquo(id)
    
    node_list <- gg %>% activate(nodes) %>% data.frame()
    edge_list <- gg %>% activate(edges) %>% data.frame()
    
    bp <- 
        edge_list %>% 
        select(from_name,to_name,weight) %>% 
        spread(to_name,weight) %>% 
        column_to_rownames(var = "from_name") %>% 
        as.matrix()
    bp[is.na(bp)] <- 0
    
    if (is_factor) {
        MM <- 
            node_list %>% 
            select({{id}},{{mm}}) %>% 
            fastDummies::dummy_cols(quo_name(mm)) %>% 
            select(-{{mm}}) %>% 
            column_to_rownames(var = quo_name(id))
    } else {
        ## TK 
    }
    
    res <- res2 <- matrix(0,nrow=nrow(bp),ncol=ncol(MM),dimnames=list(rownames(bp),colnames(MM)))
    
    for (cc in 1:ncol(MM)) {
        mm_ <- as.matrix(MM[c(rownames(bp)),cc]) %*% t(as.matrix(MM[c(colnames(bp)),cc])) 
        
        res[,cc] <- apply(bp * mm_,1,sum) 
        res2[,cc] <- apply(bp,1,sum) * MM[rownames(bp),cc]
    }
    
    out <- list(
        overall = sum(res)/sum(bp),
        group = apply(res,2,sum) / apply(res2,2,sum)#,
        # num = res,
        # denom = res2
    )
    return(out)
}
