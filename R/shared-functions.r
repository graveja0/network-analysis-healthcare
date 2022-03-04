split_into_groups_of_n <- function(x,n) {
    split(x, ceiling(seq_along(x)/n)) %>% 
        map(~(.x %>% data.frame())) %>% 
        bind_rows(.id = "group") %>% 
        as_tibble() %>% 
        set_names(c("group","x")) %>% 
        data.frame() %>% 
        column_to_rownames(var = "x")
}

ggmatrix <- function(B, normalize = FALSE) {
    if (!normalize) {
        B %>% 
            data.frame() %>% 
            rownames_to_column(var = "geography") %>% 
            gather(hospital,weight,-geography) %>% 
            mutate(geography = factor(geography, levels = rev(rownames(B)))) %>% 
            mutate(hospital = factor(hospital, levels = colnames(B))) %>% 
            ggplot(aes(x = hospital, y = geography)) + 
            geom_tile(aes(fill = weight),alpha = 0.5, colour = "black") + 
            scale_fill_gradient(low = "white", high = scales::muted("red")) + 
            geom_text(aes(label = round(weight,0)))+
            theme_tufte() +
            labs(x = "",y ="") +
            theme(legend.position = "none") +
            scale_x_discrete(position = "top") +
            theme(axis.ticks = element_blank())  
    } else {
        tmp <- diag(1/diag(B)) %*% B
        rownames(tmp) <- rownames(B) 
        
        tmp %>% 
            data.frame() %>% 
            rownames_to_column(var = "geography") %>% 
            gather(hospital,weight,-geography) %>% 
            mutate(geography = factor(geography, levels = rev(rownames(B)))) %>% 
            mutate(hospital = factor(hospital, levels = colnames(B))) %>% 
            ggplot(aes(x = hospital, y = geography)) + 
            geom_tile(aes(fill = weight),alpha = 0.5, colour = "black") + 
            scale_fill_gradient(low = "white", high = scales::muted("red")) + 
            geom_text(aes(label = round(weight,2)))+
            theme_tufte() +
            labs(x = "",y ="") +
            theme(legend.position = "none") +
            scale_x_discrete(position = "top") +
            theme(axis.ticks = element_blank())  
    }
    
}
get_aws_files <- function(project_bucket = "vumc.graves.networks.proj", prefix = "") {
    get_bucket(project_bucket, prefix = prefix) %>%
        transpose() %>%
        purrr::pluck("Key") %>%
        unlist() %>%
        tbl_df()
}

get_contiguous <- function(shp,id) {
    
    id <- enquo(id)
    
    id_names <- shp %>% pull(!!id) 
    
    out <- shp %>% 
        st_intersects() %>% 
        set_names(id_names) %>% 
        map(~(.x %>% data.frame() %>% 
                  set_names("contig"))) %>% 
        bind_rows(.id = quo_name(id)) %>% 
        group_by(!!id) %>% 
        filter(!!id != contig) %>% 
        mutate(contig = id_names[contig]) %>% 
        mutate(test = paste0("contig_",str_pad(row_number(),width=2,pad="0"))) %>% 
        spread(test,contig)  %>% 
        ungroup()
    out
}


rename_in_list <- function(x,from, to) {
    x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}

load_master_sf <- function(area_type) {

    if (area_type=="rating") {
        idname <- sym("ratng_r")
        sf_ <- 
            shapefiles %>% 
            filter(grepl("01_rating-area-shape-file_2021.shp",value)) %>% 
            pull(value) %>% 
            flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
            st_transform(crs = 4326) %>% 
            select(geoid = {{idname}},state,geometry)
        
    } else if (area_type=="county") {
        idname <- sym("fips_code")
        sf_ <- 
            shapefiles %>% 
            filter(grepl("01_county-shape-file.shp",value)) %>% 
            pull(value) %>% 
            flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
            st_transform(crs = 4326) %>% 
            select(geoid = {{idname}},state,geometry)
    }  else if (area_type=="state") {
        idname <- sym("stusps")
        sf_ <- 
            shapefiles %>% 
            filter(grepl("01_state-shape-file.shp",value)) %>% 
            pull(value) %>% 
            flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
            st_transform(crs = 4326) %>% 
            select(geoid = {{idname}},geometry)
    }  else if (area_type=="commuting-zone") {
            idname <- sym("cz_id")
            sf_ <- 
                shapefiles %>% 
                filter(grepl("01_commuting-zone-shape-file.shp",value)) %>% 
                pull(value) %>% 
                flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
                st_transform(crs = 4326) %>% 
                select(geoid = {{idname}},geometry)
        } 
    
    return(sf_)
}


load_target_sf <- function(target_area,return_contiguous = FALSE) {
    
        contiguous <- 
            sf_ %>% get_contiguous(id = geoid) %>% 
            filter(geoid %in% target_area) %>% 
            gather(contiguous,value,-geoid) %>% 
            na.omit() %>% 
            pull(value)
        
        sf_full <- 
            sf_ %>% filter(geoid %in% contiguous)
        
        sf <-
            sf_ %>% filter(geoid %in% target_area)
   
    if (return_contiguous) {
        out <- list(sf = sf, 
                    sf_full = sf_full,
                    contiguous = contiguous)
    } else out <- sf
    
    return(out)
}


get_modularity <- function(G,c,unipartite=FALSE) {
    #   
    #  For the original definition of modularity, see Newman, M. E. J., and Girvan, M. (2004). Finding and evaluating community structure in networks. Physical Review E 69, 026113. https://doi.org/10.1103/PhysRevE.69.026113
    # 
    # For the directed definition of modularity, see Leicht, E. A., and Newman, M. E. J. (2008). Community Structure in Directed Networks. Physical Review Letters 100, 118703. https://doi.org/10.1103/PhysRevLett.100.118703
    # 
    # For the introduction of the resolution parameter, see Reichardt, J., and Bornholdt, S. (2006). Statistical mechanics of community detection. Physical Review E 74, 016110. https://doi.org/10.1103/PhysRevE.74.016110
    
    # Good overview of nonoverlapping detection adn evaluation methods; https://www.cs.rpi.edu/~szymansk/papers/acm-cs.13.pdf
    #https://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/learn/graph/graph.html#:~:text=Given%20and%20adjacency%20matrix%20A,A%20are%20called%20projection%20matrices.&text=Given%20a%20directed%20graph%20G,are%20called%20projections%20of%20G.
    
    c <- enquo(c)
    if (unipartite) {
        if (is.directed(G)) {
            
            gg = 
                G %>% 
                activate(nodes) %>% 
                mutate(c = factor({{c}})) %>% 
                mutate(k_in = strength(.,mode="in")) %>% 
                mutate(k_out = strength(.,mode="out")) %>% 
                mutate(k = strength(.))
            
            m = 
                gg %>% 
                activate(edges) %>% 
                data.frame() %>% 
                summarise(m = sum(weight)) %>% 
                pull(m)
            
            A <- 
                gg %>% 
                as.igraph() %>% 
                igraph::as_adjacency_matrix(sparse = FALSE, attr="weight")
            
            c = 
                gg %>% 
                activate(nodes) %>% 
                select(name,c) %>% 
                data.frame() %>% 
                tibble::deframe()
            
            k_in <- 
                gg %>% 
                activate(nodes) %>% 
                select(name,k_in) %>% 
                data.frame() %>% 
                tibble::deframe()
            
            k_out <- 
                gg %>% 
                activate(nodes) %>% 
                select(name,k_out) %>% 
                data.frame() %>% 
                tibble::deframe()
            
            gamma = 1
            M = matrix(0 , nrow = nrow(A), ncol = ncol(A), dimnames = list(rownames(A),colnames(A)))
            
            ## Modularity 
            
            for (i in rownames(A)) {
                for (j in colnames(A)) {
                    M[i,j] <- (A[i,j] - (gamma* k_out[i]*k_in[j] / (m))) *  as.integer(c[i]==c[j])
                }
            }
            
            Q = 1/(m) * sum(M); Q
        } else {
            gg = 
                G %>% 
                activate(nodes) %>% 
                mutate(c = factor({{c}})) %>% 
                mutate(k_in = strength(.,mode="in")) %>% 
                mutate(k_out = strength(.,mode="out")) %>% 
                mutate(k = strength(.))
            
            m = 
                gg %>% 
                activate(edges) %>% 
                data.frame() %>% 
                summarise(m = sum(weight)) %>% 
                pull(m)
            
            A <- 
                gg %>% 
                as.igraph() %>% 
                igraph::as_adjacency_matrix(sparse = FALSE, attr="weight")
            
            c = 
                gg %>% 
                activate(nodes) %>% 
                select(name,c) %>% 
                data.frame() %>% 
                tibble::deframe()
            
            k <- 
                gg %>% 
                activate(nodes) %>% 
                select(name,k) %>% 
                data.frame() %>% 
                tibble::deframe()
            
            
            gamma = 1
            M = matrix(0 , nrow = nrow(A), ncol = ncol(A), dimnames = list(rownames(A),colnames(A)))
            
            ## Modularity for Unweighted Network
            
            for (i in rownames(A)) {
                for (j in colnames(A)) {
                    M[i,j] <- (A[i,j] - ( gamma*k[i]*k[j] / (2 *m))) *  as.integer(c[i]==c[j])
                }
            }
            
            Q = 1/(2*m) * sum(M); Q
        }
    } else {
        
        B <- G %>% 
            activate(edges) %>% 
            inner_join(G %>% activate(nodes) %>% data.frame() %>% select(from_name = name) %>% mutate(from=row_number()),"from") %>% 
            inner_join(G %>% activate(nodes) %>% data.frame() %>% select(to_name = name) %>% mutate(to=row_number()),"to") %>% 
            data.frame() %>% 
            ungroup() %>% 
            select(from_name,to_name,weight) %>% 
            spread(to_name,weight) %>% 
            column_to_rownames(var = "from_name") %>% 
            as.matrix()
        B[is.na(B)] <- 0
        
        c_ <-
            G %>% 
            activate(nodes) %>% 
            mutate(community = factor({{c}})) %>% 
            data.frame() %>% 
            select(name,community) %>% 
            tibble::deframe()
        
        # Barber (2007) https://journals.aps.org/pre/pdf/10.1103/PhysRevE.76.066102
        
        q_i <- apply(B,1,sum)
        d_j <- apply(B,2,sum)
        F <- sum(d_j)
        
        M <- P <- matrix(0,nrow=nrow(B),ncol=ncol(B),dimnames=list(rownames(B),colnames(B)))
        for (i in rownames(B)) {
            for (j in colnames(B)) {
                M[i,j] <- (B[i,j] - ((q_i[i]*d_j[j])/F)) * as.integer(c_[i]==c_[j])
                P[i,j] <- as.integer(c_[i]==c_[j])
            }
        }
        Q_B <- (1/F) * sum(M)
        P_B <-  1/F * sum(B * P)
        
        out <- c("modularity"=Q_B,"shared_patient_fraction" = P_B)
        return(out)
        
        
    }
    
}

get_edge_list <- function() {
    tmp <-  
        df_npi %>% 
        data.frame() %>% 
        select(from_npi = npi,from_type = specialty_group) %>% 
        data.table() 
    setkey(tmp,from_npi)
    
    tmp2 <-  
        # This uses a national scope for the "to" column
        df_npi_ %>% 
        data.frame() %>% 
        select(to_npi = npi,to_type = specialty_group) %>% 
        data.table() 
    setkey(tmp2,to_npi)
    
    edge_list_careset <- 
        edge_list_npi[tmp,nomatch=0] 
    setkey(edge_list_careset, to_npi)
    edge_list_careset <- 
        edge_list_careset[tmp2,nomatch=0] %>% 
        as_tibble()
    
    # Roll-up hosptial edges to the medicare provider level    
    tmp3 <- 
        df_npi_hosp %>% 
        select(from_npi = npi,prvnumgrp) %>% 
        unique() %>% 
        as_tibble()
    
    tmp4 <- 
        df_npi_hosp %>% 
        select(to_npi = npi,prvnumgrp) %>% 
        unique() %>% 
        as_tibble()
    
    
    edge_list_careset %>% 
        left_join(tmp3,"from_npi") %>% 
        mutate(from_npi = ifelse(!is.na(prvnumgrp) & from_type=="hospital", prvnumgrp,from_npi)) %>% 
        group_by(from_npi,to_npi,from_type,to_type) %>% 
        mutate(average_day_wait  = weighted.mean(average_day_wait, w = transaction_count)) %>% 
        summarise(patient_count = sum(patient_count), 
                  transaction_count = sum(transaction_count),
                  average_day_wait = mean(average_day_wait)) %>% 
        left_join(tmp4,"to_npi") %>% 
        mutate(to_npi = ifelse(!is.na(prvnumgrp) & from_type=="hospital", prvnumgrp,to_npi)) %>% 
        group_by(from_npi,to_npi,from_type,to_type) %>% 
        mutate(average_day_wait  = weighted.mean(average_day_wait, w = transaction_count)) %>% 
        summarise(patient_count = sum(patient_count), 
                  transaction_count = sum(transaction_count),
                  average_day_wait = mean(average_day_wait)) 
}

get_incidence_matrix <- function(G) {
    B <- 
        G %>% 
        activate(edges) %>% 
        select(from,to,weight) %>% 
        data.frame() %>% 
        mutate(from = {G %>% activate(nodes) %>% pull(name)}[from]) %>% 
        mutate(to = {G %>% activate(nodes) %>% pull(name)}[to]) %>% 
        spread(to,weight) %>% 
        column_to_rownames(var = "from") %>% 
        as.matrix()
    B[is.na(B)] <- 0
    return(B)
}

get_collapsed_incidence_matrix <- function(G,x) {
    xx <- enquo(x)
    
    B <- 
        G %>% 
        activate(edges) %>% 
        select(from,to,weight) %>% 
        data.frame() %>% 
        mutate(from = {G %>% activate(nodes) %>% pull(name)}[from]) %>% 
        mutate(to = {G %>% activate(nodes) %>% pull({{xx}})}[to]) %>% 
        #mutate(to = as.numeric(factor(to))) %>% 
        group_by(from,to) %>% 
        summarise(weight = sum(weight)) %>% 
        spread(to,weight) %>% 
        column_to_rownames(var = "from") %>% 
        as.matrix()
    B[is.na(B)] <- 0
    return(B)
}

get_outflow_hhi_orig <- function(G, overall = TRUE, collapse = FALSE) {
    
    # 1. Convert the edge list to an incidence matrix (B)
    # 2. Convert incidence matrix to a market share matrix (S)
    # 3. Use S to get ZIP-level HHI measures
    # 4. Outflow HHI is the weighted average of ZIP level HHI, among ZIPs in the area.
    
    # Note: the outflow hospitals are unrestricted and can be anywhere in the US
    
    if (collapse) {
        B <- G %>% get_collapsed_incidence_matrix( x= system_id)
    } else {
        B <- G %>% get_incidence_matrix()
    }
    
    k_z <- apply(B,1,sum) # Number of patients from each ZIP
    k_j <- apply(B,2,sum)
    B <- as.matrix(B,sparse=TRUE)
    
    S_z <- 100*as.matrix(diag(1/k_z),sparse=TRUE) %*% B
    
    HHI_outflow_z <- apply(S_z * S_z,1, sum)
    names(HHI_outflow_z) <- rownames(B)
    
    if (!overall) return(HHI_outflow_z) else {
        HHI_outflow <- weighted.mean(HHI_outflow_z,k_z) 
        return(HHI_outflow) 
    }
    
}

get_inflow_hhi <- function(G, collapse = FALSE) {
    if (!collapse) {
        B <- G %>% get_incidence_matrix()
    } else {
        B <- G %>% get_collapsed_incidence_matrix(x=sysname)
    }
    
    k_j <- apply(B,2,sum)
    sum((100*(k_j/sum(k_j)))^2)
}

# Step 1: Outflow HHI
get_outflow_hhi <- function(B) {
    # Vectorized volume by geography and firm
    k_z <- apply(B,1,sum); names(k_z)  = rownames(B)
    k_j <- apply(B,2,sum); names(k_j) = colnames(B)
    
    # Market share matrix. 
    S_z <- diag(1/k_z) %*% B
    colnames(S_z) = colnames(B); rownames(S_z) = rownames(B)
    
    # Outflow HHI
    HHI_out <- apply( 100* S_z * 100 *S_z,1,sum); names(HHI_out) <- rownames(B)
    data.frame(id = names(HHI_out), n = k_z, outflow_hhi = HHI_out )  
}

get_bipartite_graph <- function(B) {
    
    # Vectorized volume by geography and firm
    k_z <- apply(B,1,sum); names(k_z)  = rownames(B)
    k_j <- apply(B,2,sum); names(k_j) = colnames(B)
    
    G_ <- 
        graph.incidence(B, weighted=TRUE) 
    
    G <- 
        G_ %>% 
        as_tbl_graph() %>% 
        activate(nodes) %>% 
        mutate(type = ifelse(type==TRUE, "hospital","geography")) %>% 
        mutate(volume = c(k_j,k_z)[name]) 
    
    out <- list(G_ = G_, G = G)
    return(out)
}
create_unipartite_adjacency <- function(B, type ) {
    if (type== "geo") {
        X <- B %*% t(B>0) 
        return(X)
        #return(t(lower.tri(X)*X) + lower.tri(X) * X + diag(diag(X)))
    }
    
    if (type=="firm") {
        X <- t(B>0) %*% B %>% t()
        return(X)
        #return(t(lower.tri(X)*X) + lower.tri(X) * X + diag(diag(X)))
    }
}
detect_markets <- function(B,B_,type) {
    if (type=="geo") {
        
        G_ <- 
            graph_from_adjacency_matrix(B_, weighted=TRUE) 
        
        market <- walktrap.community(G_)
        dendro <- as.dendrogram(market) 
        dendro_plot <- dendro %>% ggdendrogram(rotate = FALSE) 
        max_height <-ggplot_build(dendro_plot)$data[[2]]$y[1]
        markets <- 0:max_height %>% 
            map(~(cut_at(market,steps = .x) %>% 
                      set_names(colnames(B_)) %>% 
                      data.frame() %>% 
                      set_names(c("market")) %>% 
                      rownames_to_column(var = "name") %>% 
                      mutate(level = .x))) %>% 
            bind_rows()
        df_cluster_hhi <- 
            markets %>% 
            group_by(level,market) %>% 
            nest() %>% 
            mutate(B = map(data,~({
                B[.x$name,] }))) %>% 
            ungroup() %>% 
            mutate(N = map(B,~(sum(.x)))) %>% 
            mutate(hhi = map(B,~({
                if (is.null(dim(.x))) {
                    B_ <- as.matrix(.x) %>% t() 
                    k_z <- apply(B_,1,sum); names(k_z)  = rownames(B_)
                    S_z = B_/k_z
                    hhi_z <- apply( 100* S_z * 100 *S_z,1,sum); names(hhi_z) <- rownames(B_)
                    hhi_z
                } else {
                    B_ <- as.matrix(.x)
                    # Vectorized volume by geography and firm
                    k_z <- apply(B_,1,sum); names(k_z)  = rownames(B_)
                    # Market share matrix. 
                    S_z <- diag(1/k_z) %*% B_
                    colnames(S_z) = colnames(B_); rownames(S_z) = rownames(B_)
                    # Outflow HHI
                    hhi_z <- apply( 100* S_z * 100 *S_z,1,sum); names(hhi_z) <- rownames(B_)
                    weighted.mean(hhi_z,w = k_z)
                }
                
            }))) %>% 
            select(level,market,hhi,N) %>% 
            unnest(cols=c(hhi,N)) %>% 
            mutate(market = as.numeric(paste0(market))) %>% 
            mutate(level = as.numeric(paste0(level)))
        out <- list(market_modularitymax = market, 
                    markets = markets,
                    dendro = dendro,
                    max_height = max_height, 
                    hhi = df_cluster_hhi,
                    G = G_)
    } else if (type=="firm") {
        G_j_ <- 
            graph_from_adjacency_matrix(B_, weighted=TRUE) 
        
        firm_market <- walktrap.community(G_j_)
        firm_market_dendro <- as.dendrogram(firm_market) 
        firm_market_dendro_plot <- firm_market_dendro %>% ggdendrogram(rotate = TRUE)
        firm_max_height <-ggplot_build(firm_market_dendro_plot)$data[[2]]$y[1]
        firm_markets <- 0:firm_max_height %>% 
            map(~(cut_at(firm_market,steps = .x) %>% 
                      set_names(colnames(B_)) %>% 
                      data.frame() %>% 
                      set_names(c("market")) %>% 
                      rownames_to_column(var = "name") %>% 
                      mutate(level = .x))) %>% 
            bind_rows()
        df_cluster_hhi_firm <- 
            firm_markets %>% 
            group_by(level,market) %>% 
            nest() %>% 
            mutate(B = map(data,~({
                tmp <- B[,.x$name]  %>% as.matrix()
                colnames(tmp) <- .x$name
                tmp}))) %>% 
            ungroup() %>% 
            mutate(N = map(B,~(sum(.x)))) %>% 
            mutate(hhi = map(B,~({
                
                B_ <- as.matrix(.x)
                # Vectorized volume by geography and firm
                k_j <- apply(B_,2,sum); names(k_j)  = colnames(B_)
                sum((100*(k_j/sum(k_j)))^2)
                
            }))) %>% 
            select(level,market,hhi,N) %>% 
            unnest(cols=c(hhi,N)) %>% 
            mutate(market = as.numeric(paste0(market))) %>% 
            mutate(level = as.numeric(paste0(level)))
        out <- list(market_modularitymax = firm_market, 
                    markets = firm_markets,
                    dendro = firm_market_dendro,
                    max_height = firm_max_height, 
                    hhi = df_cluster_hhi_firm,
                    G = G_j_)
    }
    
    
    
    return(out)
}
analyze_market <- function(B) {
    G <- get_bipartite_graph(B)
    B_z <- B %>% create_unipartite_adjacency(type = "geo")
    B_j <- B %>% create_unipartite_adjacency(type = "firm")
    
    market_z <- B_z %>% detect_markets(B=B,type = "geo")
    #market_z$dendro %>% ggdendrogram(rotate = FALSE)
    market_j <- B_j %>% detect_markets(B=B,type = "firm")
    #market_j$dendro %>% ggdendrogram(rotate = FALSE)
    df_means <- 
        market_j$hhi %>% 
        group_by(level) %>% 
        summarise(mean_hhi = weighted.mean(hhi,w = .data$N))
    out <- list(G = G, market_z = market_z, market_j = market_j, hhi = df_means)
    return(out)
}

