# firm_markets %>% 
#     group_by(level,market) %>% 
#     nest() -> foo
# 
# .x = c = foo[45,]$data[[1]]

calculate_firm_level_market_outcomes <- function(c,B) {
    
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
    HHI_jKM <- HHI_jKM[unlist(c)]
    
    # Now we construct the WTP measures
    B_ <- B[,unlist(c)]

    singleton = FALSE

    if (is.null(rownames(B_))) {
        singleton = TRUE
        tmp <- as.matrix(B_)
        colnames(tmp) = unlist(c)
        B_ <- tmp
    }

    q_z <- apply(B_,1,sum); names(q_z)  = rownames(B_)
    d_j <- apply(B_,2,sum); names(d_j) = colnames(B_)

    scaling1 <- 1
    scaling2 <- 100

    D_z <- diag(scaling1/q_z); colnames(D_z) = rownames(D_z) = rownames(B_)
    if (!singleton) {
        D_j <- diag(scaling1/d_j); colnames(D_j) = rownames(D_j) = colnames(B_)
    } else {
        D_j = matrix(1/d_j)
    }

    # Market share matrix.
    S_z <-  D_z %*% B_
    S_z[is.na(S_z)] <- 0
    colnames(S_z) = colnames(B_); rownames(S_z) = rownames(B_)

    Spr_z <- log(1-S_z)
    Spr_z[is.infinite(Spr_z)] <- log(1-0.999999)
    WTP_j <- -diag(apply(B_,1,sum)) %*% Spr_z %>% apply(2,sum)
    # data.frame(id = names(HHI_z), n = q_z, outflow_hhi =HHI_z) %>% 
    #     mutate(km_hhi = HHI_zKM[id])
    data.frame(id = names(HHI_jKM), hhi_km = HHI_jKM, n = d_j) %>% 
        mutate(WTP = WTP_j[id]) %>% 
        as_tibble() %>% 
        mutate(market_hhi = weighted.mean(hhi_km,w=n)) %>% 
        mutate(N = sum(n))
}
