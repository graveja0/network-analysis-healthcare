##################################
# Construct Shapefiles for a 
# rating area
##################################

# This file was originally copied from health-care-markets github on 2021-09-07

library(usethis)
library(tidyverse)
library(here)
library(devtools)
library(rlang)
library(sf)
library(aws.s3)
library(flyio) #install_github("atlanhq/flyio")

if (Sys.getenv("LOGNAME") == "gravesj") source("~/auth-aws.r")
project_bucket <- "health-care-markets"
get_aws_files <- function(project_bucket = "vumc.graves.networks.proj", prefix = "") {
    get_bucket(project_bucket, prefix = prefix) %>%
        transpose() %>%
        purrr::pluck("Key") %>%
        unlist() %>%
        tbl_df()
}
shapefiles <- get_aws_files(project_bucket = project_bucket, prefix = "tidy-mapping-files") 

flyio_set_datasource("s3")
flyio::flyio_set_bucket(project_bucket)

sf_rating <- 
    shapefiles %>% 
    filter(grepl("01_rating-area-shape-file_2021.shp",value)) %>% 
    pull(value) %>% 
    flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
    filter(ratng_r == "TN04") %>% 
    st_transform(crs = 4326) 

## ZIP CODES

sf_zip_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
cent_zip <- sf_zip_ %>% st_centroid()
zips_to_show <- cent_zip %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating, cent_zip))) %>% 
    pull(zcta5ce10)
sf_zip <- sf_zip_ %>% filter(zcta5ce10 %in% zips_to_show)


## HSA CODES

sf_hsa_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/hsa/01_hsa-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
cent_hsa <- sf_hsa_ %>% st_centroid()
hsas_to_show <- cent_hsa %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating, cent_hsa))) %>% 
    pull(hsanum)
sf_hsa <- sf_hsa_ %>% filter(hsanum %in% hsas_to_show)

## COUNTY 
sf_county <- 
    shapefiles %>% 
    filter(grepl("tidy-mapping-files/county/01_county-shape-file.shp",value)) %>% 
    pull(value) %>% 
    flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
    filter(statefp == "47" | statefp == "21") %>% 
    st_transform(crs = 4326) 

cent_county <- sf_county %>% st_centroid()
counties_to_show <- 
    cent_county %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating,cent_county))) %>% 
    pull(fips_code)

sf_county_rating <- 
    sf_county %>% 
    filter(fips_code %in% counties_to_show)

sf_zip %>% ggplot() + geom_sf() + geom_sf(data = sf_hsa, colour = scales::muted("blue"), alpha = 0.1)


df_zip_pop <-
    read_csv(here("../../../box/health-care-markets/public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
    filter(row_number()!=1) %>% 
    group_by(zcta5) %>% 
    mutate(pop10 = as.numeric(paste0(pop10))) %>% 
    summarise(pop10 = sum(pop10,na.rm=TRUE)) %>% 
    select(zip = zcta5, pop10) 


# Get the hospital x,y coordinates from the AHA Data.

rename_in_list <- function(x,from, to) {
    x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}


states <- c(
    "AK",
    "AL",
    "AR",
    "AZ",
    "CA",
    "CO",
    "CT",
    "DC",
    "DE",
    "FL",
    "GA",
    "HI",
    "IA",
    "ID",
    "IL",
    "IN",
    "KS",
    "KY",
    "LA",
    "MA",
    "MD",
    "ME",
    "MI",
    "MN",
    "MO",
    "MS",
    "MT",
    "NC",
    "ND",
    "NE",
    "NH",
    "NJ",
    "NM",
    "NV",
    "NY",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VA",
    "VT",
    "WA",
    "WI",
    "WV",
    "WY"
)



aha_files <- c(
    "2018" = "../../Research-AHA_Data/data/aha/annual/raw/2018/ASDB FY 2018/COMMA/ASPUB18.CSV"#,
    # "2017" = "../../Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV",
    #            "2016" = "../../Research-AHA_Data/data/aha/annual/raw/2016/FY2016 Annual Survey Database/COMMA/ASPUB16.CSV",
    #            "2015" = "../../Research-AHA_Data/data/aha/annual/raw/2015/FY2015 Annual Survey Database/COMMA/ASPUB15.CSV"
)

# Get latitude and longitue of general acute care hospitals in 2017 AHA survey. 
aha <- 
    aha_files %>% 
    map(~(
        data.table::fread(here(.x)) %>% 
            janitor::clean_names() %>% 
            filter(mstate %in% states) %>% 
            filter(mstate !="AK" & mstate!="HI") %>% 
            mutate(system_id = ifelse(!is.na(sysid),paste0("SYS_",sysid),id)) %>% 
            filter(serv==10))) %>% 
    map(~rename_in_list(x = .x, from = "hcfaid", to = "mcrnum")) %>% 
    map(~(.x %>% 
              select(mname, id, mcrnum , latitude = lat, longitude = long, admtot, system_id, mloczip, sysname,mloccity ) %>% 
              mutate(prvnumgrp = str_pad(mcrnum,width = 6, pad="0")) %>% 
              mutate(hosp_zip_code = str_sub(mloczip,1,5)) %>% 
              mutate(longitude = as.numeric(paste0(longitude))) %>% 
              mutate(latitude = as.numeric(paste0(latitude))) %>% 
              filter(!is.na(longitude) & !is.na(latitude))
    )) %>% 
    set_names(names(aha_files)) #%>% 

# Shape file of x,y coordinates of all hospitals
xy_aha <- aha[["2018"]] %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_hosp_ <- 
    xy_aha %>% filter(row_number() %in% unlist(st_intersects(sf_zip,xy_aha))) %>% 
    select(id,mname,system_id,sysname,geometry,admtot)  

df_hosp <- 
    df_hosp_ %>% 
    mutate(zip_code = sf_zip$zcta5ce10[unlist(st_within(df_hosp_,sf_zip))]) %>% 
    mutate(hsanum = sf_hsa$hsanum[unlist(st_within(df_hosp_,sf_hsa))]) %>% 
    mutate(fips_code = sf_county_rating$fips_code[unlist(st_within(df_hosp_,sf_county_rating))]) 

##########################
# Save!
###########################

cent_zip_ <- sf_zip %>% st_centroid()

cent_zip <- 
    cent_zip_ %>% 
    mutate(hsanum = sf_hsa_$hsanum[unlist(st_within(cent_zip_,sf_hsa_))]) %>% 
    mutate(fips_code = sf_county_rating$fips_code[unlist(st_within(cent_zip_,sf_county_rating))]) 

zip_location <- 
    cent_zip %>% 
    data.frame() %>% 
    select(zip_code = zip_code, 
           hsanum, 
           fips_code)
sf_zip %>% 
    left_join(zip_location,"zip_code") %>% 
    sf::write_sf(here("data/shp/tn04.shp"))

bind_cols(df_hosp, df_hosp %>% st_coordinates() %>% as_tibble()) %>% 
    data.frame() %>% 
    ungroup() %>% 
    select(-geometry) %>% 
    mutate(type = "HOSP") %>% 
    bind_rows(
        bind_cols(cent_zip, cent_zip %>% st_coordinates() %>% as_tibble()) %>% 
            data.frame() %>% 
            ungroup() %>% 
            select(-geometry) %>% 
            rename(zip = zcta5ce10) %>% 
            left_join(df_zip_pop,"zip") %>% 
            select(zip,X,Y,pop10,zip_code,hsanum,fips_code) %>% 
            gather(type,id,-X,-Y,-pop10,-zip_code,-hsanum,-fips_code) %>% 
            as_tibble()  
    ) %>% 
    select(id,type,X,Y,everything()) %>% 
    write_rds(here("data/coordinates.rds"))







