shapefiles <- get_aws_files(project_bucket = markets_project_bucket, prefix = "tidy-mapping-files") 

# 1. Load Shapefile and Common Data
sf_ <- load_master_sf(area_denom)
sf_county <- load_master_sf("county")
sf_cz <- load_master_sf("commuting-zone")
source(here::here("R/load-common-data.r")) # Uses sf_ as input

# 2. Calculate/Load Kessler-McClellan at the National Level (makes for an easier time later)
source(here::here("R/kessler-mcclellan.R"))
km_system <- read_rds(here(collapsed_output_file))
km_hospital <- read_rds(here(hospital_output_file))

# 3. Get adjacency matrix
source(here("R/get-adjacency-matrix.r"))
B_county <- get_adjacency_matrix(
                          application_area = application_area, 
                          area_type="state", 
                          row_type="county",
                          collapse_to_system_level = TRUE ,
                          system_radius = 100,
                          minimum_market_share = 0.01,
                          minimum_market_size = 25,
                          km = km
                          )
B_county %>% write_rds(here(glue("output/adjacency-matrices/B-county-{application_area}")))

B_cz<- get_adjacency_matrix(application_area = application_area, 
                                 area_type="state", 
                                 row_type="commuting-zone",
                                 collapse_to_system_level = TRUE ,
                                 system_radius = 100,
                                 minimum_market_share = 0.01,
                                 minimum_market_size = 25,
                                 km = km
)
B_cz %>% write_rds(here(glue("output/adjacency-matrices/B-commuting-zone-{application_area}")))

# 4. Market Concentration Measures
source(here::here("R/calculate-concentration-measures.r"))
df_hhi <- 
    suppressWarnings({
        calculate_concentration_measures(
            B = B_county,
            target_area = application_area,
            area_denom = area_denom,
            km = km_system,
            collapse_to_system_level = TRUE ,
            system_radius = 100,
            minimum_market_share = 0.01,
            minimum_market_size = 25)
        })
df_hhi %>% write_rds(here(glue("output/hhi/hhi-county-{application_area}.rds")))

df_hhi <- 
    suppressWarnings({
        calculate_concentration_measures(
            B = B_cz,
            target_area = application_area,
            area_denom = area_denom,
            km = km_system,
            collapse_to_system_level = TRUE ,
            system_radius = 100,
            minimum_market_share = 0.01,
            minimum_market_size = 25)
    })
df_hhi %>% write_rds(here(glue("output/hhi/hhi-commuting-zone-{application_area}.rds")))

# ############
# # SCRATCH
# ############
# 

# # 3. Calculate Geography-Level Measures
# source(here::here("R/calculate-concentration-measures.r"))
# .x <- "TN"
# collapse_to_system_level = TRUE
# system_radius = 100
# minimum_market_share = 0.01
# minimum_market_size = 25
# km = km_system


# hhi_sys <- list() 
# 
# for (.x in states[-which(states %in% c("AK","HI","DC"))]) {
#     if (is.null(hhi_sys[[.x]])) {
#         cat(.x)
#     hhi_sys[[.x]] <- suppressWarnings({
#                             calculate_concentration_measures(target_area = .x, 
#                                                 area_denom = area_denom, 
#                                                 km = km_system, 
#                                                 collapse_to_system_level = TRUE)})
#     }
# }
# 
# hhi_sys %>% write_rds(here(glue("output/system-hhi-{target_area}.rds")))
# 
# 
# hhi_hosp <- list() 
# 
# for (.x in states[-which(states %in% c("AK","HI","DC"))]) {
#     if (is.null(hhi_hosp[[.x]])) {
#         cat(.x)
#         hhi_hosp[[.x]] <- suppressWarnings({
#             calculate_concentration_measures(target_area = .x, 
#                                              area_denom = area_denom, 
#                                              km = km_system, 
#                                              collapse_to_system_level = FALSE)})
#     }
# }
# 
# hhi_hosp %>% write_rds(here(glue("output/hospital-hhi-{target_area}.rds")))
# 
# 
# 
# df_hhi <- 
#     hhi_sys %>% 
#     bind_rows() %>% 
#     as_tibble() %>% 
#     mutate( diff_outflow_km_sys = outflow_hhi_sys - hhi_km_sys)
# 
# p1 <- sf_target %>% 
#     inner_join(df_hhi,"geoid") %>% 
#     ggplot() + geom_sf(aes(fill = hhi_km_sys),alpha = 0.75,lwd=0) +
#     #geom_sf(data = hosps,size=2, aes(colour = color, shape = factor(shape)))  +
#     ggthemes::theme_map() +
#     theme(legend.key.size = unit(1, 'cm')) +
#     theme(legend.position = "none") +
#     scale_fill_gradient2(
#         name = "Herfindahl-Hirschman Index",
#         limits = c(000,10000),
#         midpoint = 4000,
#         low = scales::muted("blue"),
#         mid = "white",
#         high = scales::muted("red")
#     ) + 
#     geom_sf(data = sf_ %>% filter(!(geoid %in% c("AK","HI"))) ,alpha = 0,lwd =.1)
# 
# 
# p2 <- 
#     sf_target %>% 
#     inner_join(df_hhi,"geoid") %>% 
#     ggplot() + geom_sf(aes(fill = outflow_hhi_sys),alpha = 0.75,lwd=0) +
#     #geom_sf(data = hosps,size=2, aes(colour = color, shape = factor(shape)))  +
#     ggthemes::theme_map() +
#     theme(legend.key.size = unit(1, 'cm')) +
#     theme(legend.position = "bottom") +
#     scale_fill_gradient2(
#         name = "Herfindahl-Hirschman Index",
#         limits = c(0000,10000),
#         midpoint = 4000,
#         low = scales::muted("blue"),
#         mid = "white",
#         high = scales::muted("red")
#     ) + 
#     geom_sf(data = sf_ %>% filter(!(geoid %in% c("AK","HI"))) ,alpha = 0,lwd =.1)
# 
# 
# library(patchwork)
# p1 / p2
# 
# df_hhi %>% 
#     ggplot(aes(x = outflow_hhi_sys, y = hhi_km_sys)) + geom_point(alpha = 0.25) + 
#     theme_ipsum() +
#     geom_abline(intercept = 0, slope = 1, lty=3) + 
#     scale_x_continuous(limits = c(0000,10000)) +
#     scale_y_continuous(limits = c(0000,10000)) 
# 
# 
# outflow <- df_hhi %>% pull(outflow_hhi_sys)
# km <- df_hhi %>% pull(hhi_km_sys)
# qq.out <- qqplot(outflow,km,asp=1)
# qq.out <- as.data.frame(qq.out) 
# 
# xylim <- range( c(qq.out$x, qq.out$y) )
# 
# # Generate the QQ plot
# ggplot(qq.out[,], aes( x= x, y = y)) + 
#     geom_line() + 
#     geom_abline( intercept=0, slope=1,lty=3) +
#     coord_fixed(ratio = 1, xlim=xylim, ylim = xylim) +
#     xlab("Outflow HHI") + ylab("Kessler-McClellan") + 
#     theme_ipsum() +
#     scale_x_continuous(limits = c(0,10000))+
#     scale_y_continuous(limits = c(0,10000))
# 

# 
# 

# 
# 
# 
# 
# hhi_hosp <- calculate_concentration_measures(target_area = .x, 
#                                                 area_denom = area_denom, 
#                                                 km = km_hospital, 
#                                                 collapse_to_system_level = FALSE)
# 
# df_hhi <- 
#     hhi_sys %>% 
#     left_join(hhi_hosp %>% select(-n_hosps,-n_systems),"geoid") %>% 
#     as_tibble() %>% 
#     mutate(diff_outflow_hosp_sys = outflow_hhi_sys - outflow_hhi_hosp,
#            diff_km_hosp_sys = hhi_km_sys - hhi_km_hosp, 
#            diff_outflow_km_sys = outflow_hhi_sys - hhi_km_sys,
#            diff_outflow_km_hosp = outflow_hhi_hosp - hhi_km_hosp)
# 
# df <- 
#     df_hhi %>% 
#     select(contains("hhi"),geoid) %>% 
#     gather(measure,value,-geoid) 
# 
# ff <-  lm(value~as.integer(geoid), data = df)
# 
# df_resid <- 
#     df[rownames(model.frame(ff)),] %>% 
#     mutate(value_resid = residuals(summary(ff))) %>% 
#     mutate(pooled_mean = mean(value))  %>% 
#     mutate(value_resid = value_resid + pooled_mean) %>% 
#     select(geoid, measure,value_resid) %>% 
#     spread(measure,value_resid) %>% 
#     mutate(diff_outflow_hosp_sys = outflow_hhi_sys - outflow_hhi_hosp,
#            diff_km_hosp_sys = hhi_km_sys - hhi_km_hosp, 
#            diff_outflow_km_sys = outflow_hhi_sys - hhi_km_sys,
#            diff_outflow_km_hosp = outflow_hhi_hosp - hhi_km_hosp)
# 
# hosps_ <- df_node_hosp_[which(!is.na(as.numeric(st_within(df_node_hosp_,sf_ %>% filter(geoid %in% .x))))),] %>% 
#     filter(!grepl("Army",mname)) 
# 
# plot_groups <- unique(hosps_$system_id) %>% split_into_groups_of_n(25)
# n_colors_needed <- length(unique(plot_groups$group))
# plot_colors <- c("black",scales::muted("red"),scales::muted("blue"),scales::muted("green"), "purple","yellow","brown","orange")[1:n_colors_needed]
# plot_shapes <- 
#     plot_groups %>% 
#     rownames_to_column(var = "system_id") %>% 
#     mutate(group = as.numeric(paste0(group))) %>% 
#     mutate(color = plot_colors[group]) %>% 
#     group_by(group) %>% 
#     mutate(shape = row_number())
# 
# hosps <- 
#     hosps_ %>% 
#     left_join(plot_shapes,"system_id")
# 
# plot_hhi_map <- function(df, measure, diff = FALSE, legend_pos = "none") {
#     xx <- enquo(measure)
#     tt <- glue("{quo_name(xx)}: {.x}")
#     p <- 
#         sf_target %>% 
#         inner_join(df,"geoid") %>% 
#         ggplot() + geom_sf(aes(fill = {{xx}}),alpha = 0.75) +
#         geom_sf(data = hosps,size=2, aes(colour = color, shape = factor(shape)))  +
#         ggthemes::theme_map() +
#         
#         scale_shape_manual(values = 1:36, guide = FALSE) +
#         theme(legend.key.size = unit(1, 'cm')) +
#         labs(title = glue(tt)) +#,
#             # subtitle = "Points correspond to the location of hospitals; shapes correspond to hospital systems.")  +
#         scale_colour_manual(guide = FALSE, values = unique(hosps$color )) +
#         theme(legend.position = legend_pos) 
#     
#     if (diff) {
#         p + scale_fill_gradient2(low = "blue", mid = "white", high = scales::muted("red"), 
#                              midpoint = 0) 
#     } else {
#         p + scale_fill_viridis(
#         name = "Herfindahl–Hirschman Index",
#         limits = c(0, 10000),
#         breaks = c(0,2000,4000,  6000,8000,10000),
#         labels = c("Not\nConcentrated","2,000","4,000","6,000","8,000","10,000\nMost\nConcentrated")) 
#     }
# }
# 
# p1 <- df_hhi %>% plot_hhi_map(measure = outflow_hhi_sys)
# p2 <- df_hhi %>% plot_hhi_map(measure = outflow_hhi_hosp)
# p3 <- df_hhi %>% plot_hhi_map(measure = diff_outflow_hosp_sys, diff = TRUE)
# 
# p4 <- df_hhi %>% plot_hhi_map(measure = hhi_km_sys)
# p5 <- df_hhi %>% plot_hhi_map(measure = hhi_km_hosp)
# p6 <- df_hhi %>% plot_hhi_map(measure = diff_km_hosp_sys, diff = TRUE)
# 
# p7 <- df_hhi %>% plot_hhi_map(measure = diff_outflow_km_sys, diff = TRUE)
# p8 <- df_hhi %>% plot_hhi_map(measure = diff_outflow_km_hosp, diff = TRUE)
# 
# p1 + p2 + p3 + p4 + p5 + p6  + p7 + p8  + plot_layout(nrow = 3, ncol = 3)
# 
# 
# hhi_sys %>% 
#     ggplot(aes(x = outflow_hhi_sys, y = hhi_km_sys)) + geom_point() +
#     hrbrthemes::theme_ipsum() +
#     geom_smooth(se = FALSE,method = "lm", colour = scales::muted("blue")) +
#     geom_abline(intercept = 0, slope =1, lty=3, colour = "black") + 
#     xlim(c(0,10000)) + 
#     ylim(c(0,10000)) + 
#     labs(title = "Outflow HHI vs. Kessler-McClellan HHI", y = "Kessler-McClellan HHI", x = "Outflow HHI")
# 
# hhi_sys %>% 
#     select(geoid, hhi_km_sys) %>% 
#     inner_join(
#         hhi_hosp %>% select(geoid, hhi_km_hosp), "geoid"
#     ) %>% 
#     ggplot(aes(x = hhi_km_hosp, y = hhi_km_sys)) + geom_point() +
#     hrbrthemes::theme_ipsum() +
#     geom_smooth(se = FALSE,method = "lm", colour = scales::muted("blue")) +
#     geom_abline(intercept = 0, slope =1, lty=3, colour = "black") + 
#     #xlim(c(0,10000)) + 
#     #ylim(c(0,10000)) + 
#     labs(title = "KM HHI: Hospital vs. System", y = "HHI-System", x = "HHI-Hospital")
# 
# 
# 
# df_hhi %>% 
#     select(geoid,contains("hhi")) %>% 
#     gather(measure, value,-geoid) %>% 
#     mutate(category = cut(value,breaks = c(0,2500,5000, 10000), labels = c("Not Concentrated","Moderately Concentrated", "Concentrated")))   %>% 
#     arrange(category,value) %>% 
#     group_by(category) %>% 
#     filter(row_number()==1)
#     spread(geoid,measure)
#  
# 
# df_hhi %>% 
#     select(geoid,contains("hhi")) %>% 
#     mutate_at(vars(contains("hhi")),function(x) rank(x)) %>% 
#     rename_at(vars(contains("hhi")), function(x) paste0(x,"_rank")) %>% 
#     gather(measure, rank,-geoid) 
# 
# df_hhi %>% 
#     select(geoid,contains("hhi")) %>% 
#     mutate_at(vars(contains("hhi")),function(x) rank(x)) %>% 
#     ggplot(aes(x = outflow_hhi_sys, y = hhi_km_sys)) + geom_point() + 
#     theme_ipsum() +
#     labs(title="Rank Plot: Outflow vs. KM",x = "Outflow HHI", y = "KM HHI") +
#     geom_abline(intercept = 0, slope = 1, lty=3)
# 
# df_hhi %>% 
#     select(geoid,contains("hhi")) %>% 
#     mutate_at(vars(contains("hhi")),function(x) rank(x)) %>% 
#     ggplot(aes(x = outflow_hhi_sys, y = outflow_hhi_hosp)) + geom_point() + 
#     theme_ipsum() +
#     labs(title="Rank Plot: Outflow vs. KM",x = "Outflow HHI", y = "KM HHI") + 
#     geom_abline(intercept = 0, slope = 1, lty=3)
# 
# target_area <- "TN01"
# 
# hhi_hosp <- 
#     c("TN01","TN02","TN03","TN04","TN05","TN06","TN07","TN08") %>% 
#     map(~calculate_concentration_measures(target_area = .x, collapse = FALSE))
# 
# hhi_system <- 
#     c("TN01","TN02","TN03","TN04","TN05","TN06","TN07","TN08") %>% 
#     map(~calculate_concentration_measures(target_area = .x, collapse = TRUE))
# 
# sf_zip_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>%  
#     st_transform(crs = 4326) 
# 
# zip_hhi <- 
#     hhi_system %>% map(~(.x[["km_zip"]])) %>% 
#     bind_rows() %>% 
#     group_by(zip_code) %>% 
#     filter(row_number()==1)
# 
# sf_zip_ %>% 
#     inner_join(zip_hhi,c("zip_code")) %>% 
#     ggplot() + geom_sf(aes(fill = hhi_star_z)) + ggthemes::theme_map() +
#     scale_fill_gradient2(limits = c(2000,10000), midpoint=4000, low = "blue", mid = "white", high = "darkred") +
#     theme(legend.position = "bottom")
# 
# target_area = "TN02"
# 
# hosp_flows <- pcp_card <- pcp_onc <- list()
# threshold_for_minimum_within = 0.01
# 
# areas_to_run <- grep("^47",unique(sf_$geoid),value=TRUE)
# areas_to_run <- unique(sf_$geoid)
# 
# 
# hosp_flows <- read_rds(here("output/results/2022-02-14/hosp-flows.rds"))
# pcp_card <- read_rds(here("output/results/2022-02-14/pcp-card-shared.rds"))
# pcp_onc <- read_rds(here("output/results/2022-02-14/pcp-onc-shared.rds"))
# 
# for (target_area in areas_to_run) {
#     if (is.null(    hosp_flows[[target_area]])) {
# 
#         cat(paste0(target_area))
#         # 2. Load the shapefile
#         sf <- load_target_sf(target_area = target_area )
#         zips_in_area <- sf_zip_cent$zip_code[!is.na(as.numeric(st_within(sf_zip_cent,sf)))] 
#         
#         # hospitals_within_area <- 
#         #     df_node_hosp_$id[!is.na(as.numeric(st_within(df_node_hosp_,sf)))]  %>% 
#         #     as_tibble() %>% 
#         #     filter(value !="") %>%   
#         #     pull(value)
#         # 
#         # df_node_hosp <- 
#         #     df_node_hosp_ %>% 
#         #     filter(id %in% hospitals_within_area) %>% 
#         #     bind_cols(st_coordinates(.) %>% as_tibble() %>% set_names(c("x","y"))) 
#         
#         # Restrict to those only located in the target area
#         tmp <- df_npi_[which(!is.na(as.numeric((st_within(df_npi_,sf))))),]
#          
#         if ( nrow(tmp %>% filter(specialty_group!="hospital")) >1) {
#             
#             df_npi <- 
#              tmp %>% 
#                 bind_cols(st_coordinates(.) %>% as_tibble() %>% set_names(c("x","y")))
#         
#             edge_list <- get_edge_list()
#         
#             edge_list_geography <- 
#                 edge_list_geography_ %>% 
#                 ## NOTE THIS WILL EXCLUDE HOSPITALS WELL OUTSIDE THE TOTAL AREA
#                 filter(zip_code %in%  zips_in_area) %>% 
#                 select(from= zip_code, to = prvnumgrp, weight = total_cases)
#             
#             total_nodes <- 
#                 data.frame(npi = unique(c(edge_list$to_npi,edge_list$from_npi,edge_list_geography$to))) %>% 
#                 as_tibble()
#             
#             df_node <- 
#                 df_npi_  %>% 
#                 filter(specialty_group != "hospital") %>% 
#                 bind_rows(
#                     df_node_hosp_ %>% 
#                         rename(npi = id)  %>% 
#                         mutate(specialty_group = "hospital")
#                 ) %>% 
#                 inner_join(total_nodes,"npi")
#             df_node <- 
#                 df_node %>% 
#                 bind_cols(st_coordinates(df_node) %>% data.frame() %>% select(x=X,y=Y))
#             
#             hosp_flows[[target_area]] <- 
#                 edge_list_geography %>% 
#                 left_join(df_node_hosp_ %>% data.frame() %>% ungroup() %>% select(to=id,to_area = geoid),"to") %>% 
#                 left_join(sf_zip_cent %>% data.frame() %>% ungroup() %>%  select(from = zip_code, from_area = geoid),"from") %>% 
#                 filter(from_area %in% target_area) %>% 
#                 mutate(within = as.integer(from_area==to_area | is.na(to_area)))  %>% 
#                 group_by(within) %>% 
#                 summarise(weight = sum(weight)) %>% 
#                 ungroup() %>% 
#                 mutate(pct = 100 * (weight / sum(weight))) %>% 
#                 mutate(geoid = target_area)
#             
#         pct_flows_in_mapped_area_ <-  
#             edge_list %>% 
#                 filter(from_type == "primcare" & to_type=="cardiology")  %>% 
#                 inner_join(
#                     df_node %>% data.frame() %>% ungroup() %>%  select(from_npi = npi, from_x = x, from_y = y, from_area = geoid), "from_npi"
#                 ) %>% 
#                 inner_join(
#                     df_node %>% data.frame() %>% ungroup() %>%  select(to_npi = npi, to_x = x, to_y = y, to_area = geoid), "to_npi"
#                 ) %>% 
#                 filter(from_area %in% target_area) %>% 
#                 mutate(to_area = ifelse(is.na(to_area) | from_area!=target_area,"outside__area",to_area)) %>% 
#                 mutate(within = as.integer(from_area==to_area)) %>% 
#                 group_by(from_npi) %>% 
#                 mutate(pct_within = sum(patient_count * within) / sum(patient_count)) 
#          
#         pcp_card[[target_area]] <- 
#             pct_flows_in_mapped_area_ %>% 
#             group_by(within) %>% 
#             mutate(weight = patient_count) %>% 
#             summarise(weight = sum(weight)) %>% 
#             ungroup() %>% 
#             mutate(pct = 100 * (weight / sum(weight)))  
#         
#         
#         pct_flows_in_mapped_area_ <-  
#             edge_list %>% 
#             filter(from_type == "primcare" & to_type=="oncology")  %>% 
#             inner_join(
#                 df_node %>% data.frame() %>% ungroup() %>%  select(from_npi = npi, from_x = x, from_y = y, from_area = geoid), "from_npi"
#             ) %>% 
#             inner_join(
#                 df_node %>% data.frame() %>% ungroup() %>%  select(to_npi = npi, to_x = x, to_y = y, to_area = geoid), "to_npi"
#             ) %>% 
#             filter(from_area %in% target_area) %>% 
#             mutate(to_area = ifelse(is.na(to_area),"outside__area",to_area)) %>% 
#             mutate(within = as.integer(from_area==to_area)) %>% 
#             group_by(from_npi) %>% 
#             mutate(pct_within = sum(patient_count * within) / sum(patient_count)) 
#         
#         pcp_onc[[target_area]] <- 
#             pct_flows_in_mapped_area_ %>% 
#             group_by(within) %>% 
#             mutate(weight = patient_count) %>% 
#             summarise(weight = sum(weight)) %>% 
#             ungroup() %>% 
#             mutate(pct = 100 * (weight / sum(weight)))  
#             
#             
#         } else {
#             pcp_onc[[target_area]] <- pcp_card[[target_area]] <- data.frame(within=0,weight=0,pct=-Inf) 
#             
#             edge_list_geography <- 
#                 edge_list_geography_ %>% 
#                 ## NOTE THIS WILL EXCLUDE HOSPITALS WELL OUTSIDE THE TOTAL AREA
#                 filter(zip_code %in%  zips_in_area) %>% 
#                 select(from= zip_code, to = prvnumgrp, weight = total_cases)
#             
#             if (nrow(edge_list_geography)>0) {
#             total_nodes <- 
#                 data.frame(npi = unique(c(edge_list_geography$to))) %>% 
#                 as_tibble()
#             
#             df_node <- 
#                 df_node_hosp_ %>% 
#                 rename(npi = id)  %>% 
#                 mutate(specialty_group = "hospital") %>% 
#                 inner_join(total_nodes,"npi")
#             df_node <- 
#                 df_node %>% 
#                 bind_cols(st_coordinates(df_node) %>% data.frame() %>% select(x=X,y=Y))
#             
#             hosp_flows[[target_area]] <- 
#                 edge_list_geography %>% 
#                 left_join(df_node_hosp_ %>% data.frame() %>% ungroup() %>% select(to=id,to_area = geoid),"to") %>% 
#                 left_join(sf_zip_cent %>% data.frame() %>% ungroup() %>%  select(from = zip_code, from_area = geoid),"from") %>% 
#                 filter(from_area %in% target_area) %>% 
#                 mutate(within = as.integer(from_area==to_area | is.na(to_area)))  %>% 
#                 group_by(within) %>% 
#                 summarise(weight = sum(weight)) %>% 
#                 ungroup() %>% 
#                 mutate(pct = 100 * (weight / sum(weight))) %>% 
#                 mutate(geoid = target_area)
#             } else {
#                 hosp_flows[[target_area]]  <- data.frame(within=0,weight=0,pct=-Inf) 
#             }
#         }
#             
#         cat("\n")
#     }
#     
# }
# 
# hosp_flows %>% write_rds(here("output/results/2022-02-14/hosp-flows.rds"))
# pcp_card %>% write_rds(here("output/results/2022-02-14/pcp-card-shared.rds"))
# pcp_onc %>% write_rds(here("output/results/2022-02-14/pcp-onc-shared.rds"))
# 
# sf_county <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/county/01_county-shape-file.shp")) %>%
#     st_transform(crs = 4326) %>% 
#     inner_join(
#         hosp_flows %>% 
#             bind_rows(.id="fips_code") %>% 
#             arrange(fips_code,desc(pct)) %>% 
#             group_by(fips_code) %>% 
#             mutate(pct = ifelse(within==0,100-pct,pct)) %>% 
#             filter(row_number()==1), "fips_code"
#     )
# 
# setdiff(areas_to_run,unique(sf_county$fips_code))
# 
# sf_county %>% 
#     ggplot() + geom_sf(aes(fill = as.numeric(pct))) +
#     viridis::scale_fill_viridis(name="Percent in County", limits = c(0,100), breaks = seq(0,100,25)) +
#     ggthemes::theme_map() +
#     theme(legend.position ="bottom")
# 
# sf_county %>% 
#     summarise(weighted.mean(pct,w=weight))
# 
# pcp_onc %>% 
#     bind_rows() %>% 
#     filter(within==1) %>% 
#     ggplot() + geom_density(aes(x=pct)) + ggthemes::theme_tufte()
