sf_zip_cent <- # Source: https://mcdc.missouri.edu/applications/geocorr2014.html (2022-02-10)
    read.csv(here("data/zip-population-centroids/geocorr2014_2204104231.csv"),skip=1) %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    # Some ZIPs straddle states; get the weighted centroid again.
    group_by(zip_census_tabulation_area) %>% 
    summarise_at(vars(wtd_centroid_latitude_degrees,wtd_centroid_w_longitude_degrees),
                 ~weighted.mean(.,w = .data$total_population_2010 )) %>%
    select(zip_code = zip_census_tabulation_area, 
           y = wtd_centroid_latitude_degrees, 
           x =  wtd_centroid_w_longitude_degrees) %>% 
    st_as_sf(coords = c("x","y"), crs = 4326) %>% 
    bind_cols(st_coordinates(.) %>% as.tibble() %>% set_names(c("x","y")))  %>% 
    mutate(zip_code = str_pad(zip_code,width = 5, side ="left",pad = "0"))

sf_zip_cent <- 
    sf_zip_cent %>% 
    left_join(
        st_within(sf_zip_cent,sf_county) %>% 
            data.frame() %>% 
            right_join(sf_zip_cent %>% data.frame() %>% select(zip_code) %>% mutate(row.id = row_number()),"row.id") %>% 
            left_join(sf_county %>% data.frame() %>% select(geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zip_code,geoid) %>% 
            as_tibble() %>% 
            rename(fips_code = geoid)
        ,"zip_code") %>% 
    left_join(
        st_within(sf_zip_cent,sf_cz) %>% 
            data.frame() %>% 
            right_join(sf_zip_cent %>% data.frame() %>% select(zip_code) %>% mutate(row.id = row_number()),"row.id") %>% 
            left_join(sf_cz %>% data.frame() %>% select(geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zip_code,geoid) %>% 
            as_tibble() %>% 
            rename(cz_id = geoid)
        ,"zip_code") 

aha_files <- c(
    "2018" = "../../Research-AHA_Data/data/aha/annual/raw/2018/ASDB FY 2018/COMMA/ASPUB18.CSV",
    "2017" = "../../Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV",
    "2016" = "../../Research-AHA_Data/data/aha/annual/raw/2016/FY2016 Annual Survey Database/COMMA/ASPUB16.CSV",
    "2015" = "../../Research-AHA_Data/data/aha/annual/raw/2015/FY2015 Annual Survey Database/COMMA/ASPUB15.CSV"
)
aha_files <- aha_files[[application_year]]

df_node_hosp_ <- 
    aha_files %>% 
    map(~(
        data.table::fread(here(.x)) %>% 
            janitor::clean_names() %>% 
            as_tibble() %>% 
            filter(mstate %in% states) %>% 
            mutate(system_id = ifelse(!is.na(sysid),paste0("SYS_",sysid),id)) %>% 
            filter(serv==10))) %>% 
    map(~rename_in_list(x = .x, from = "hcfaid", to = "mcrnum")) %>% 
    map(~(.x %>% 
              select(mname, id, mcrnum , latitude = lat, longitude = long, hrrnum = hrrcode, 
                     hsanum = hsacode, admtot, system_id, mloczip, sysname,
                     fips_code=fcounty,mloccity ) %>% 
              mutate(hrrnum = paste0(hrrnum)) %>% 
              mutate(hsanum = paste0(hsanum)) %>% 
              mutate(prvnumgrp = str_pad(mcrnum,width = 6, pad="0")) %>% 
              mutate(hosp_zip_code = str_sub(mloczip,1,5)) %>% 
              mutate(longitude = as.numeric(paste0(longitude))) %>% 
              mutate(latitude = as.numeric(paste0(latitude))) %>% 
              filter(!is.na(longitude) & !is.na(latitude)) %>% 
              as_tibble()
    )) %>% 
    set_names(names(aha_files)) %>% 
    bind_rows(.id = "aha_year") %>% 
    as_tibble() %>% 
    ungroup() %>% 
    select(id = mcrnum, x= longitude,  y = latitude, admtot,mname,sysname) %>% 
    mutate(type = "hospital") %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
    mutate(sysname = ifelse(sysname=="" | is.na(sysname), mname, sysname)) %>% 
    filter(sysname!="Department of Veterans Affairs")

df_node_hosp_ <- 
    df_node_hosp_ %>% 
        as_tibble() %>% 
        filter(id!="") %>% 
    inner_join(
        st_within(df_node_hosp_,sf_county) %>% 
            data.frame() %>% 
            right_join(df_node_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number()),"row.id") %>% 
            left_join(sf_county %>% data.frame() %>% select(geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,fips_code = geoid) %>% 
            as_tibble()
        ,"id") %>% 
    left_join(
        st_within(df_node_hosp_,sf_cz) %>% 
            data.frame() %>% 
            right_join(df_node_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number()),"row.id") %>% 
            left_join(sf_cz %>% data.frame() %>% select(geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,cz_id = geoid) %>% 
            as_tibble()
        ,"id")  %>% 
    left_join(
        st_within(df_node_hosp_,sf_) %>% 
            data.frame() %>% 
            right_join(df_node_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number()),"row.id") %>% 
            left_join(sf_ %>% data.frame() %>% select(denomid = geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,denomid) %>% 
            as_tibble()
        ,"id") %>% 
    mutate(system_id = str_pad(as.numeric(factor(sysname)),width =4, side = "left", pad = "0"))

# # These are all the NPIs in the US
# df_npi_ <- 
#     #read_rds("~/Desktop/geocoded-physician-and-hospital-locations.rds") %>% 
#     s3readRDS(object = "data/geocoded-physician-and-hospital-locations/2019/geocoded-physician-and-hospital-locations.rds",
#               bucket = networks_project_bucket) %>%
#     select(npi,specialty_group,loc_x,loc_y) %>% 
#     filter(specialty_group %in% c("cardiology","primcare", "oncology","hospital")) %>% 
#     group_by(npi) %>% 
#     filter(row_number()==1) %>% 
#     st_as_sf(coords = c("loc_x", "loc_y"), crs = 4326) 
# 
# df_npi_ <- 
#     df_npi_ %>% 
#     left_join(
#         st_within(df_npi_,sf_target) %>% 
#             data.frame() %>% 
#             right_join(df_npi_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number()),"row.id") %>% 
#             left_join(sf_target %>% data.frame() %>% select(geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
#             select(npi,geoid) %>% 
#             as_tibble()
#         ,"npi") %>% 
#     left_join(
#         st_within(df_npi_,sf_) %>% 
#             data.frame() %>% 
#             right_join(df_npi_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number()),"row.id") %>% 
#             left_join(sf_ %>% data.frame() %>% select(denomid = geoid) %>% mutate(col.id = row_number()),"col.id") %>% 
#             select(npi,denomid) %>% 
#             as_tibble()
#         ,"npi")
# 
# df_hosp_xw <- # NPI to AHA/Medicare ID crosswalk from Penn/JHU
#     s3readRDS(object = "data/penn-xw-api/general-acute-care-hospital-npi-crosswalk.rds",
#               bucket = networks_project_bucket, check_region = F) %>% 
#     mutate(npi = paste0(npi))
# 
# # Crosswalking of NPI to PRVNUMGRP at the national level
# df_npi_hosp <-
#     df_npi_ %>% 
#     inner_join(df_hosp_xw,"npi") %>% 
#     data.frame() %>% 
#     select(npi,prvnumgrp,aha_id) %>% 
#     as_tibble() %>% 
#     group_by(npi) %>% 
#     mutate(n = n()) %>% 
#     ungroup() 

# if (appplication_year==2015) {
#     edge_list_npi <- # Careset raw edge list
#         data.table::fread(here(glue("../data/careset/DocGraph_Hop_Teaming_{application_year}/docgraph_hop_teaming_{application_year}.csv")))  %>% 
#         set_names(c("from_npi","to_npi","patient_count","transaction_count","average_day_wait","std_day_wait")) %>% 
#         mutate(from_npi = as.character(from_npi),
#                to_npi = as.character(to_npi)) %>% 
#         as.data.table()
# } else {
#     edge_list_npi <- # Careset raw edge list
#         data.table::fread(here(glue("../data/careset/DocGraph_Hop_Teaming_{application_year}/docgraph_hop_teaming_{application_year}.csv")))  %>% 
#         mutate(from_npi = as.character(from_npi),
#                to_npi = as.character(to_npi)) %>% 
#         as.data.table()
# }
# 
# setkey(edge_list_npi,from_npi)  

# Created in R/read-and-tidy-cms-hospital-service-areas
edge_list_geography_ <- 
    read_rds(here(glue("../../health-care-markets/output/hospital-county-patient-data/{application_year}/hospital-zip-patient-data.rds")))

