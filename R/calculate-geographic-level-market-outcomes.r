# market_z$markets %>% 
#     group_by(level,market) %>% 
#     nest() -> foo
# 
# .x = c = foo[1,]$data[[1]]
# 

calculate_geographic_level_market_outcomes <- function(c,B) {
    # We need the full B matrix for KM measures; we'll just subset to the market-specific rows for the output
    B_ <- B

    q_z <- apply(B_,1,sum); names(q_z)  = rownames(B_)
    d_j <- apply(B_,2,sum); names(d_j) = colnames(B_)
    
    scaling1 <- 1
    scaling2 <- 100
    
    D_z <- diag(scaling1/q_z); colnames(D_z) = rownames(D_z) = rownames(B_)
    D_j <- diag(scaling1/d_j); colnames(D_j) = rownames(D_j) = colnames(B_)
    
    # Market share matrix. 
    S_z <-  D_z %*% B_
    S_z[is.na(S_z)] <- 0
    colnames(S_z) = colnames(B_); rownames(S_z) = rownames(B_)
    
    # Outflow HHI
    HHI_z <- apply(scaling2*S_z * scaling2*S_z,1,sum)  ; names(HHI_z) = rownames(B_)
    
    # Now get each hospital's share of demand from each ZIP
    S_j <- (as.matrix(B_,sparse=TRUE) %*%  as.matrix(D_j,sparse=TRUE))
    colnames(S_j) <- colnames(B_)
    rownames(S_j) <- rownames(B_)
    
    # Firm-specific HHI
    HHI_jKM <- as.matrix(diag(HHI_z),sparse=TRUE) %*% S_j %>% apply(.,2,sum) ; names(HHI_jKM) =  colnames(B_)
    
    # Now get back to the ZIP level # HHI^star_k^pat
    HHI_zKM <- diag(HHI_jKM)  %*% t(S_z)  %>% apply(.,2,sum) 
    names(HHI_zKM) <- rownames(B_)
    
    data.frame(id = names(HHI_z), n = q_z, outflow_hhi =HHI_z) %>%
        mutate(km_hhi = HHI_zKM[id]) %>% 
        filter(id %in% unlist(c)) %>% 
        mutate(market_hhi = weighted.mean(outflow_hhi,w=n),
               market_km_hhi = weighted.mean(km_hhi,w=n)) %>% 
        mutate(N = sum(n))

}
#.x %>% calculate_geographic_level_market_outcomes(B = B)
