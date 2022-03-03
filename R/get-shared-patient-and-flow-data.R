# Get Shared Patient and Patient Flow Data

# The objective of this file is to construct a series of edge and node lists
# that facilite analyses of the Nashville metro health care market. 

# JACKSON MADISON 
# # A tibble: 5 × 4
# npi        aha_id  prvnumgrp genacute
# <chr>      <chr>   <chr>        <dbl>
#     1 1093705428 6524010 440002           1
# 2 1326063108 6524010 440002           1
# 3 1942225727 6524010 440002           1
# 4 1992720676 6524010 440002           1
# 5 1750306437 6524010 440002           1
# > jack <- "6524010"


target_area <- "TN04" #c("TN01","TN02","TN03","TN04","TN05","TN06","TN07","TN08")
state_abb <- "TN"

library(data.table)
library(dtplyr)
library(glue)
library(usethis)
library(tidyverse)
library(here)
library(devtools)
library(rlang)
library(sf)
library(aws.s3)
library(stringr)
library(janitor)
library(httr)
library(flyio) #install_github("atlanhq/flyio")

if (Sys.getenv("LOGNAME") == "gravesj") source("~/auth-aws.r")
project_bucket <- "health-care-markets"
networks_project_bucket = "vumc.graves.networks.proj"

get_aws_files <- function(project_bucket = "vumc.graves.networks.proj", prefix = "") {
    get_bucket(project_bucket, prefix = prefix) %>%
        transpose() %>%
        purrr::pluck("Key") %>%
        unlist() %>%
        tbl_df()
}
shapefiles <- get_aws_files(project_bucket = project_bucket, prefix = "tidy-mapping-files") 

rename_in_list <- function(x,from, to) {
    x %>% rename_at(vars(contains(from)), funs(sub(from, to, .)))
}

states <- c(
    "AK",    "AL",    "AR",    "AZ",    "CA",    "CO",    "CT",    "DC",    "DE",    
    "FL",    "GA",    "HI",    "IA",    "ID",    "IL",    "IN",    "KS",    "KY",    
    "LA",    "MA",    "MD",    "ME",    "MI",    "MN",    "MO",    "MS",    "MT",    
    "NC",    "ND",    "NE",    "NH",    "NJ",    "NM",    "NV",    "NY",    "OH",    
    "OK",    "OR",    "PA",    "RI",    "SC",    "SD",    "TN",    "TX",    "UT",    
    "VA",    "VT",    "WA",    "WI",    "WV",    "WY"
)

aha_files <- c(
    "2017" = "../../Research-AHA_Data/data/aha/annual/raw/2017/FY2017 ASDB/COMMA/ASPUB17.CSV"
)

####################################################
# Our first objective is to identify the geographic 
# boundaries of the TN04 rating area. 
####################################################

flyio_set_datasource("s3")
flyio::flyio_set_bucket(project_bucket)

sf_rating <- 
    shapefiles %>% 
    filter(grepl("01_rating-area-shape-file_2021.shp",value)) %>% 
    pull(value) %>% 
    flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
    filter(ratng_r %in% target_area) %>% 
    st_transform(crs = 4326) 

## ZIP CODES

sf_zip_ <- 
    read_sf(here("../../health-care-markets/output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
cent_zip <- sf_zip_ %>% st_centroid()
zips_to_show <- cent_zip %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating, cent_zip))) %>% 
    pull(zcta5ce10)
sf_zip <- sf_zip_ %>% filter(zcta5ce10 %in% zips_to_show)

# Commuting Zone

sf_commuting_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/commuting-zone/01_commuting-zone-shape-file.shp")) %>% 
    st_transform(crs = 4326)

# Primcare Care Service Area
sf_pcsa_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/pcsa/01_pcsa-shape-file.shp")) %>% 
    st_transform(crs = 4326)

## HSA CODES

sf_hsa_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/hsa/01_hsa-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
cent_hsa <- sf_hsa_ %>% st_centroid()
hsas_to_show <- cent_hsa %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating, cent_hsa))) %>% 
    pull(hsanum)
sf_hsa <- sf_hsa_ %>% filter(hsanum %in% hsas_to_show)

## COUNTY 
sf_county_ <- 
    shapefiles %>% 
    filter(grepl("tidy-mapping-files/county/01_county-shape-file.shp",value)) %>% 
    pull(value) %>% 
    flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
    st_transform(crs = 4326) 

broad_county_geography <- 
    st_intersects(sf_rating,sf_county_) %>% 
    data.frame() %>% 
    as_tibble() %>% 
    mutate(fips_code = sf_county_$fips_code[col.id]) %>% 
    pull(fips_code)

sf_county <- 
    sf_county_ %>% 
    filter(fips_code %in% broad_county_geography)

cent_county <- sf_county %>% st_centroid()
counties_to_show <- 
    cent_county %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating,cent_county))) %>% 
    pull(fips_code)

sf_county_rating <- 
    sf_county %>% 
    filter(fips_code %in% counties_to_show)

df_zip_pop <-
    read_csv(here("../../../box/health-care-markets/public-data/zcta-to-fips-county/zcta-to-fips-county.csv")) %>% 
    filter(row_number()!=1) %>% 
    group_by(zcta5) %>% 
    mutate(pop10 = as.numeric(paste0(pop10))) %>% 
    summarise(pop10 = sum(pop10,na.rm=TRUE)) %>% 
    select(zip = zcta5, pop10) 

cent_zip_ <- sf_zip %>% st_centroid()

cent_zip <- 
    cent_zip_ %>% 
    left_join(
        st_within(cent_zip_,sf_hsa_) %>% 
            data.frame() %>% 
            right_join(cent_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_hsa_ %>% data.frame() %>% select(hsanum) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,hsanum),"zcta5ce10") %>% 
    left_join(
        st_within(cent_zip_,sf_county) %>% 
            data.frame() %>% 
            right_join(cent_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_county %>% data.frame() %>% select(fips_code) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,fips_code),"zcta5ce10") %>% 
    left_join(
        st_within(cent_zip_,sf_commuting_) %>% 
            data.frame() %>% 
            right_join(cent_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_commuting_ %>% data.frame() %>% select( cz_id) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10, cz_id),"zcta5ce10") %>% 
    left_join(
        st_within(cent_zip_,sf_pcsa_) %>% 
            data.frame() %>% 
            right_join(cent_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_pcsa_ %>% data.frame() %>% select( pcsa) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10, pcsa),"zcta5ce10")

zip_location <- 
    cent_zip %>% 
    data.frame() %>% 
    select(zip_code = zip_code, 
           hsanum, 
           fips_code,
           cz_id,
           pcsa)
sf_zip %>% 
    left_join(zip_location,"zip_code") %>% 
    sf::write_sf(here(glue("data/shp/{paste0(target_area,collapse = '-')}.shp")))

###############################################
# We next need to identify the set of providers 
# contained within this area.
###############################################

# These are all the NPIs in the US
 df_npi_ <- 
    #read_rds("~/Desktop/geocoded-physician-and-hospital-locations.rds") %>% 
    s3readRDS(object = "data/geocoded-physician-and-hospital-locations/2019/geocoded-physician-and-hospital-locations.rds",
              bucket = networks_project_bucket) %>%
    select(npi,specialty_group,loc_x,loc_y) %>% 
    filter(specialty_group %in% c("hospital","cardiology","primcare")) %>% 
    group_by(npi) %>% 
    filter(row_number()==1) %>% 
    st_as_sf(coords = c("loc_x", "loc_y"), crs = 4326) 

# Restrict to those only located in the TN04 rating area
df_npi <- 
    df_npi_[which(!is.na(as.numeric((st_within(df_npi_,sf_rating))))),]

# A dataframe version of the above geographic object
df_npi_tn04 <- 
    df_npi %>% data.frame() %>% 
    select(from_npi = npi)

# NPI to AHA/Medicare ID crosswalk from Penn/JHU
df_hosp_xw <- 
    s3readRDS(object = "data/penn-xw-api/general-acute-care-hospital-npi-crosswalk.rds",
              bucket = networks_project_bucket, check_region = F) %>% 
    mutate(npi = paste0(npi))

# Crosswalking of NPI to PRVNUMGRP at the national level
df_npi_hosp <-
    df_npi_ %>% 
    inner_join(df_hosp_xw,"npi") %>% 
    data.frame() %>% 
    select(npi,prvnumgrp,aha_id) %>% 
    as_tibble() %>% 
    group_by(npi) %>% 
    mutate(n = n()) %>% 
    ungroup() 

# Now get the careset data and restrict to only those NPIs within the geographic area.

if (!file.exists(here(glue("ignore/edge_list_{paste0(target_area,collapse = '-')}.rds")))) {
    yy = 2017
    edge_list_npi <- data.table::fread(here("../data/careset/DocGraph_Hop_Teaming_2017/DocGraph_Hop_Teaming_2017.csv")) %>%
        lazy_dt()
    
    edge_list_tn04 <- edge_list_npi %>% mutate(from_npi = paste0(from_npi), to_npi = paste0(to_npi)) %>% inner_join(df_npi_tn04,"from_npi") %>% data.frame()
    
    #edge_list_tn04 %>% saveRDS(here("ignore/edge_list_tn04.rds"))
    edge_list_tn04 %>% saveRDS(here(glue("ignore/edge_list_{paste0(target_area,collapse = '-')}.rds")))
}

df_edge_npi_ <- 
    read_rds(here(glue("ignore/edge_list_{paste0(target_area,collapse = '-')}.rds"))) %>% as_tibble() %>% 
########################################
## AVERAGE DAY WAIT OF 30 DAYS OR LESS
########################################
    filter(average_day_wait < 30) %>% 
    rename(from = from_npi, to = to_npi, weight = patient_count) %>% 
    select(from,to,weight) %>% 
######################################################################################3
# TO GET ALL REFERRALS EVEN OUTSIDE THE RATING AREA INNER JOIN ON DF_NPI_ NOT DF_NPI
######################################################################################3
    inner_join(df_npi_ %>% data.frame() %>% select(to = npi, specialty_group) ,"to") %>% 
    #inner_join(df_npi %>% data.frame() %>% select(to = npi, specialty_group) ,"to") 
    group_by(from) %>% 
    mutate(total_to_weight = sum(weight))

min(df_edge_npi_$weight)
length(unique(df_edge_npi_$from))
length(unique(df_edge_npi_$to))

# The hospitals are listed at the NPI level, however we need to roll up the edge list data
# to the medicare provider ID level

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
    set_names(names(aha_files)) 

df_node_npi_ <- 
    df_npi_ %>% 
    as_tibble() %>% 
    filter(npi %in% c(unique(df_edge_npi_$from),unique(df_edge_npi_$to))) %>% 
    left_join(df_hosp_xw,"npi") %>% 
    rename(orig_npi = npi) %>%
    group_by(orig_npi) %>% 
    mutate(n_ids = n()) %>% 
    ungroup() %>% 
    as_tibble() %>% 
    mutate(id = ifelse(is.na(prvnumgrp),orig_npi,prvnumgrp)) %>% 
    select(id,orig_npi,aha_id,prvnumgrp,specialty_group,geometry,n_ids)
    
df_node_hosps <- 
    df_node_npi_ %>% 
    filter(!is.na(aha_id)) %>% 
    group_by(orig_npi) %>% 
    #filter(row_number()==n()) %>% 
    select(prvnumgrp,orig_npi,n_ids) %>% 
    left_join(
        aha[["2017"]] %>% data.frame() %>% select(prvnumgrp=mcrnum,admtot) , "prvnumgrp"
    ) %>% 
    arrange(orig_npi,desc(admtot)) %>% 
    filter(row_number()==1) %>% 
    select(-admtot,-n_ids) %>% 
    data.frame() %>% 
    unique() %>% 
    column_to_rownames(var = "orig_npi") 

df_edge_npi <-     
    df_edge_npi_ %>% 
    mutate(orig_from = from,
           orig_to = to) %>% 
    mutate(from = df_node_hosps[orig_from,]) %>% 
    mutate(to = df_node_hosps[orig_to,]) %>% 
    mutate(from = coalesce(from,orig_from),
           to = coalesce(to,orig_to)) %>% 
    #select(from,to,weight) %>% 
    group_by(from,to) %>% 
    arrange(from,to) %>% 
    summarise(weight = sum(weight,na.rm=TRUE),
              total_to_weight = sum(total_to_weight,na.rm=TRUE))

# With the edge list finalized and rolled up we can now construct a
# final node list

nodes_in_edge_list <-
    unique(c(df_edge_npi$from,df_edge_npi$to))

df_node <-
    data.frame(id = unique(c(df_edge_npi$from,df_edge_npi$to))) %>% 
    as_tibble() %>% 
    mutate(type = ifelse(id %in% df_node_hosps$prvnumgrp,"hosp","md"))

# Shapefiles to place within ZIP, County, HSA, etc.

sf_zip_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/zcta/01_zcta-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
sf_hsa_ <- read_sf(here("../../health-care-markets/output/tidy-mapping-files/hsa/01_hsa-shape-file.shp")) %>%  
    st_transform(crs = 4326) 
sf_county <- 
    shapefiles %>% 
    filter(grepl("tidy-mapping-files/county/01_county-shape-file.shp",value)) %>% 
    pull(value) %>% 
    flyio::import_shp(pathshp= . , FUN = sf::read_sf, dir = tempdir()) %>% 
    st_transform(crs = 4326) 

# Get latitude and longitue of general acute care hospitals in 2017 AHA survey. 

xy_aha <- aha[["2017"]] %>% 
    data.frame() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_hosp_ <- 
    xy_aha %>% filter(mcrnum %in% df_node$id) %>% 
    select(id = mcrnum,mname,system_id,sysname,geometry,admtot)  

df_node_hosp <-
    df_hosp_ %>% 
    left_join(
        st_within(df_hosp_,sf_zip_) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,zcta5ce10),"id") %>% 
    left_join(
        st_within(df_hosp_,sf_hsa_) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_hsa_ %>% data.frame() %>% select(hsanum) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,hsanum),"id") %>% 
    left_join(
        st_within(df_hosp_,sf_county) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_county %>% data.frame() %>% select(fips_code) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,fips_code),"id") %>% 
    left_join(
        st_within(df_hosp_,sf_rating) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_rating %>% data.frame() %>% select(ratng_r) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,ratng_r),"id") %>% 
    left_join(
        st_within(df_hosp_,sf_commuting_) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_commuting_ %>% data.frame() %>% select(cz_id) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,cz_id),"id") %>% 
    left_join(
        st_within(df_hosp_,sf_pcsa_) %>% 
            data.frame() %>% 
            right_join(df_hosp_ %>% data.frame() %>% select(id) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_pcsa_ %>% data.frame() %>% select(pcsa) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(id,pcsa),"id") %>% 
    mutate(x = map(geometry,~(st_coordinates(.x)[1])),
           y = map(geometry,~(st_coordinates(.x)[2])))

#######################################
## HOSPITAL ISOCHRONES AND DISTANCES
#######################################

df_node_hosps_in_area <- 
    df_node_hosp %>% 
    filter(fips_code %in% counties_to_show) 
    
get_mapbox_isochrone <- function(long, lat, contours_minutes, base_url = "https://api.mapbox.com/", mapbox_token = "pk.eyJ1IjoiZ3JhdmVzaiIsImEiOiJjanhieXdyNmMwOHh2M3RueXllcGIzaWlxIn0.mSiBJ9z2Ny9k6F1m6k46JA") {
        request_url <- paste0(
            "isochrone/v1/mapbox/driving/",
            long, ",", lat, "?contours_minutes=", contours_minutes,
            "&polygons=true&access_token=", mapbox_token
        )
        url <- modify_url(base_url, path = request_url)
        
        tryCatch({
            temp <- suppressWarnings(httr::GET(url, verbose = T))
            Sys.sleep(1)
        },
        error = function(cond) {
            message(paste("URL does not seem to exist:", url))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },
        finally = {
            out <- suppressMessages(jsonlite::fromJSON(content(temp, "text"),
                                                       simplifyVector = FALSE, simplifyDataFrame = TRUE, flatten = TRUE
            ))
            
            name_iso <- sort(unlist(str_split(contours_minutes, pattern = ", ", n = 4)), decreasing = T)
            # print(name_iso)
            coords <- out$features$geometry.coordinates
            
            # If isochrone is found, name the the subsets by their corresponding driving-times
            # for example, a 10 minute isochrones is named "10"
            if (!is.null(coords)) {
                names(coords) <- name_iso
            }
            return(coords)
        }
        )
    }
source("~/Dropbox/setup-mapbox.r")

if (!file.exists(here(glue("output/hospitals-within-60min-{paste0(target_area,collapse = '-')}.rds")))) {
    
    hosp_iso <- 
        df_node_hosps_in_area  %>% 
        mutate(test = map2(x, y, ~ (c(.x, .y)))) %>%
        pull(test) %>% 
        map(~ (get_mapbox_isochrone(long = .x[1], lat = .x[2], contours_minutes = c("60"),
                                    mapbox_token = my_mapbox_token)), .progress = TRUE)  %>% 
        set_names(df_node_hosps_in_area$id)
    
    df_iso60 <- list() 
    
    for (i in 1:length(hosp_iso)) {
        cat(paste0(i," of ",length(hosp_iso),"\n"))
        tmp_ <- 
            hosp_iso[i] %>% 
            map(~(.x %>% pluck("60") %>% 
                      pluck(1) %>% 
                      map(~(data.frame(.x) %>%
                                mutate(var = c("x","y")) %>% 
                                spread(var,.x))) %>% 
                      bind_rows())) %>% 
            bind_rows(.id = "id")  %>% 
            unique() %>% 
            st_as_sf(coords = c("x","y"), crs = 4326) %>% 
            group_by(id) %>%
            summarize(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON") %>%
            st_simplify()
        
        
        df_iso60[[i]] <- 
            st_within(xy_aha,tmp_) %>% 
            data.frame() %>% 
            inner_join(xy_aha %>% data.frame() %>% select(within_id = prvnumgrp) %>% mutate(row.id = row_number())) %>% 
            inner_join(tmp_ %>% data.frame() %>% select(id) %>% mutate(col.id = row_number()),"col.id") %>% 
            as_tibble() %>% 
            filter(within_id!="000000") %>% 
            select(-row.id,-col.id) %>% 
            mutate(within_60min = 1) 
        
    }
    
    df_iso60  %>% 
        bind_rows() %>% 
        write_rds(here(glue("output/hospitals-within-60min-{paste0(target_area,collapse = '-')}.rds")))
} 

df_iso60 <- read_rds(here(glue("output/hospitals-within-60min-{paste0(target_area,collapse = '-')}.rds")))

if (!file.exists(here(glue("output/hospitals-within-30min-{paste0(target_area,collapse = '-')}.rds")))) {
    
    hosp_iso30 <- 
        df_node_hosps_in_area  %>% 
        mutate(test = map2(x, y, ~ (c(.x, .y)))) %>%
        pull(test) %>% 
        map(~ (get_mapbox_isochrone(long = .x[1], lat = .x[2], contours_minutes = c("30"),
                                    mapbox_token = my_mapbox_token)), .progress = TRUE)  %>% 
        set_names(df_node_hosps_in_area$id)
    
    df_iso30 <- list() 
    
    for (i in 1:length(hosp_iso30)) {
        cat(paste0(i," of ",length(hosp_iso30),"\n"))
        tmp_ <- 
            hosp_iso30[i] %>% 
            map(~(.x %>% pluck("30") %>% 
                      pluck(1) %>% 
                      map(~(data.frame(.x) %>%
                                mutate(var = c("x","y")) %>% 
                                spread(var,.x))) %>% 
                      bind_rows())) %>% 
            bind_rows(.id = "id") %>% 
            unique() %>% 
            st_as_sf(coords = c("x","y"), crs = 4326) %>% 
            group_by(id) %>%
            summarize(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON") %>%
            st_simplify()
        
        
        df_iso30[[i]] <- 
            st_within(xy_aha,tmp_) %>% 
            data.frame() %>% 
            inner_join(xy_aha %>% data.frame() %>% select(within_id = prvnumgrp) %>% mutate(row.id = row_number())) %>% 
            inner_join(tmp_ %>% data.frame() %>% select(id) %>% mutate(col.id = row_number()),"col.id") %>% 
            as_tibble() %>% 
            filter(within_id!="000000") %>% 
            select(-row.id,-col.id) %>% 
            mutate(within_30min = 1) 
        
    }
    
    df_iso30  %>% 
        bind_rows() %>% 
        write_rds(here(glue("output/hospitals-within-30min-{paste0(target_area,collapse = '-')}.rds")))
}

df_iso30 <- read_rds(here(glue("output/hospitals-within-30min-{paste0(target_area,collapse = '-')}.rds")))


dist_matrix <- 
    df_node_hosp %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
    st_distance() 
dist_matrix <- 
    dist_matrix * 0.000621371 # Convert to miles
colnames(dist_matrix) <- rownames(dist_matrix) <- df_node_hosp$id

df_dist_matrix <- 
    dist_matrix %>% 
    data.frame() %>% 
    rownames_to_column(var = "id") %>% 
    as_tibble() %>% 
    gather(dist_id,distance,-id) %>% 
    mutate(distance = as.numeric(distance)) %>% 
    arrange(id,distance) %>% 
    mutate(dist_id = gsub("X","",dist_id))

df_dist_matrix  %>% 
    write_rds(here(glue("output/hospital-distance-{paste0(target_area,collapse = '-')}.rds")))


df_iso60 <- 
    read_rds(here(glue("output/hospitals-within-60min-{paste0(target_area,collapse = '-')}.rds"))) %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(within_60min = map(data,
                              ~(.x %>% pull(within_id) %>% paste0(collapse="; ")))) %>% 
    select(id,within_60min) %>% 
    unnest(cols = c(within_60min))

df_iso30 <- 
    read_rds(here(glue("output/hospitals-within-30min-{paste0(target_area,collapse = '-')}.rds"))) %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(within_30min = map(data,
                              ~(.x %>% pull(within_id) %>% paste0(collapse="; ")))) %>% 
    select(id,within_30min) %>% 
    unnest(cols = c(within_30min))

df_dist15 <- 
    read_rds(here(glue("output/hospital-distance-{paste0(target_area,collapse = '-')}.rds"))) %>% 
    group_by(id) %>% 
    nest() %>% 
    inner_join(df_iso30 %>% select(id),"id") %>% 
    mutate(within_15miles = map(data,
                                ~(.x %>% filter(distance<=15) %>% pull(dist_id) %>% paste0(collapse = "; ")))) %>% 
    select(id,within_15miles) %>% 
    unnest(cols = c(within_15miles))

df_dist30 <- 
    read_rds(here(glue("output/hospital-distance-{paste0(target_area,collapse = '-')}.rds"))) %>% 
    group_by(id) %>% 
    nest() %>% 
    inner_join(df_iso30 %>% select(id),"id") %>% 
    mutate(within_30miles = map(data,
                                ~(.x %>% filter(distance<=30) %>% pull(dist_id) %>% paste0(collapse = "; ")))) %>% 
    select(id,within_30miles) %>% 
    unnest(cols = c(within_30miles))

df_dist60 <- 
    read_rds(here(glue("output/hospital-distance-{paste0(target_area,collapse = '-')}.rds"))) %>% 
    group_by(id) %>% 
    nest() %>% 
    inner_join(df_iso30 %>% select(id),"id") %>% 
    mutate(within_60miles = map(data,
                                ~(.x %>% filter(distance<=60) %>% pull(dist_id) %>% paste0(collapse = "; ")))) %>% 
    select(id,within_60miles) %>% 
    unnest(cols = c(within_60miles))

df_iso_dist <- 
    df_iso30 %>% 
    left_join(df_iso60,"id") %>% 
    left_join(df_dist15,"id") %>% 
    left_join(df_dist30,"id") %>% 
    left_join(df_dist60,"id") 

df_node_hosp <-
    df_node_hosp %>% 
    left_join(df_iso_dist,"id")

df_node_hosp %>% filter(id=="440039")

### END ISOCHRONES

df_node_md_ <- 
    df_npi_ %>% 
    inner_join(df_node %>% filter(type!="hosp") %>% unique(),c("npi"="id")) 

df_node_md <- 
    df_node_md_ %>% 
    left_join(
        st_within(df_node_md_,sf_zip_) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_zip_ %>% data.frame() %>% select(zcta5ce10) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,zcta5ce10),"npi") %>% 
    left_join(
        st_within(df_node_md_,sf_hsa_) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_hsa_ %>% data.frame() %>% select(hsanum) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,hsanum),"npi") %>% 
    left_join(
        st_within(df_node_md_,sf_county) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_county %>% data.frame() %>% select(fips_code) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,fips_code),"npi") %>% 
    left_join(
        st_within(df_node_md_,sf_rating) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_rating %>% data.frame() %>% select(ratng_r) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,ratng_r),"npi") %>% 
    left_join(
        st_within(df_node_md_,sf_commuting_) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_commuting_ %>% data.frame() %>% select(cz_id) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,cz_id),"npi")  %>% 
    left_join(
        st_within(df_node_md_,sf_pcsa_) %>% 
            data.frame() %>% 
            right_join(df_node_md_ %>% data.frame() %>% select(npi) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_pcsa_ %>% data.frame() %>% select(pcsa) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(npi,pcsa),"npi")  %>% 
    rename(id = npi)  %>% 
    mutate(x = map(geometry,~(st_coordinates(.x)[1])),
           y = map(geometry,~(st_coordinates(.x)[2]))) %>% 
    unnest(cols = c(x,y))

sf_rating %>% ggplot() + 
    geom_sf(data = sf_county %>% filter(statefp=="47"))+ geom_sf() + 
    geom_sf(data = df_node_md %>% filter(specialty_group=="cardiology"),aes(colour = specialty_group))  +
    geom_sf(data = df_node_hosp, colour = "black") 


##############################################
#### Get Provider Network Participation Data
##############################################

# Step 1. ACA Individual and Small Group Market

aca_network_files <- list.files("~/Desktop/networks-2021/ACA Researcher Network Data Package - 2021-06-03//") %>% 
    paste0("~/Desktop/networks-2021/ACA Researcher Network Data Package - 2021-06-03/",.) %>% 
    set_names(list.files("~/Desktop/networks-2021/ACA Researcher Network Data Package - 2021-06-03//"))

# Marketplace Network Service Area Public Use File

if (!file.exists(here("data/service-area-puf_2021.rds"))) {
    service_area_file <- "https://download.cms.gov/marketplace-puf/2021/service-area-puf.zip"
    temp <- tempfile()
    download.file(service_area_file,temp)
    df_service_area_puf <- 
        read.csv(unz(temp, "Service_Area_PUF.csv")) %>% 
        as_tibble() %>% 
        janitor::clean_names() 
    df_service_area_puf %>% write_rds(here("data/service-area-puf_2021.rds"))
} else {
    df_service_area_puf <- read_rds(here("data/service-area-puf_2021.rds"))
}

if (!file.exists(here("data/plan_attributes-puf_2021.rds"))) {
    plan_attributes_puf <- "https://download.cms.gov/marketplace-puf/2021/plan-attributes-puf.zip"
    temp <- tempfile()
    download.file(plan_attributes_puf,temp)
    df_plan_attributes_puf <- 
        read.csv(unz(temp, "Plan_Attributes_PUF.csv")) %>% 
        as_tibble() %>% 
        janitor::clean_names() 
    df_plan_attributes_puf %>% write_rds(here("data/plan_attributes-puf_2021.rds"))
} else {
    df_plan_attributes_puf <- read_rds(here("data/plan_attributes-puf_2021.rds"))
}

if (!file.exists(here("data/network-puf_2021.rds"))) {
    network_puf <- "https://download.cms.gov/marketplace-puf/2021/network-puf.zip"
    temp <- tempfile()
    download.file(network_puf,temp)
    df_network_puf <- 
        read.csv(unz(temp, "Network_PUF.csv")) %>% 
        as_tibble() %>% 
        janitor::clean_names() 
    df_network_puf %>% write_rds(here("data/network-puf_2021.rds"))
} else {
    df_network_puf <- read_rds(here("data/network-puf_2021.rds"))
}

# First identify the issuers that service the counties in question

df_issuers_in_area <- 
    df_service_area_puf %>% 
    mutate(county = str_pad(county,width = 5, pad = "0"),
           issuer_id = paste0(issuer_id)) %>% 
    filter(county %in% counties_to_show | (cover_entire_state=="Yes" & state_code %in% state_abb))%>% 
    select(issuer_id,market_coverage,service_area_id) %>% 
    unique()

# Now find the networks available 
aca_network_plans <- 
    df_plan_attributes_puf %>% 
    mutate(issuer_id = paste0(issuer_id)) %>% 
    inner_join(df_issuers_in_area, c("issuer_id","service_area_id")) %>% 
    select(network_id = network_id,external_plan_id  = standard_component_id,issuer_market_place_marketing_name,market_coverage = market_coverage.x) %>% 
    unique() %>% 
    mutate(issuer_id = as.numeric(substr(external_plan_id,1,5))) %>% 
    inner_join(df_network_puf %>% select(network_id, issuer_id,network_name,market_coverage),c("issuer_id","network_id","market_coverage")) %>% 
    mutate(network_name = paste0(issuer_market_place_marketing_name,"-",network_name)) %>% 
    select(cciio_network_name = network_id,external_plan_id,network_name,market_coverage) %>% 
    inner_join(data.table::fread(aca_network_files[["network_plans.csv"]]) %>% 
                   as_tibble() %>% 
                   clean_names() %>% 
                   filter(market=="individual" | market=="small_group") %>% 
                   unique(),"external_plan_id") %>% 
    select(network_name,network_id,market) %>% 
    unique() %>% 
    as.data.table()


aca_network_plans %>% write_rds(here(glue("output/aca-networks-{paste0(target_area,collapse = '-')}.rds")))

providers <- 
    data.table::fread(aca_network_files[["providers.csv"]]) %>% 
    data.table()
setkey(providers, provider_id)

aca_networks_ <- 
    data.table::fread(aca_network_files[["network_providers.csv"]]) 
setkey(aca_networks_,network_id)
setkey(aca_network_plans,network_id)
aca_networks <- 
    aca_networks_[aca_network_plans, nomatch = 0] 
setkey(aca_networks,provider_id)
aca_networks <- 
    aca_networks[providers,nomatch = 0]

# Merge in the hospital ID
tmp_ <- 
    df_hosp_xw %>%  
    mutate(npi = as.numeric(paste0(npi))) %>% 
    data.table()
setkey(tmp_, npi)
setkey(aca_networks,npi)

aca_networks_hosp <- 
    aca_networks[tmp_,nomatch=0] %>% 
    as_tibble() %>% 
    select(provider_id,network_id,market,npi,prvnumgrp) %>% 
    mutate(npi = paste0(npi)) %>% 
    mutate(id = prvnumgrp) %>% 
    select(id,network_id,market) %>% 
    group_by(id,network_id,market) %>% 
    filter(row_number()==1) %>% 
    mutate(network_id = ifelse(market=="individual",glue("i{network_id}"),glue("sg{network_id}"))) %>% 
    select(id,network_id) %>% 
    fastDummies::dummy_cols("network_id") %>% 
    select(id,starts_with("network_id_")) %>% 
    group_by(id) %>% 
    mutate_at(vars(starts_with("network_id_")),~max(.,na.rm=TRUE)) %>% 
    filter(row_number()==1) %>% 
    rename_at(vars(starts_with("network_id_")),function(x) gsub("network_id_","",x)) %>% 
    inner_join(df_node_hosp %>% select(id),"id")

networks_to_keep <- 
    aca_networks_hosp %>% 
    ungroup() %>% 
    data.frame() %>% 
    select(-geometry) %>% 
    select(-id) %>% 
    summarise_all(mean) %>% 
    gather(network,value) %>% 
    filter(value>.1) %>% 
    pull(network)
    
# Get the physician-level Network Data
tmp2_ <- 
    df_node_md %>% 
    select(npi = id) %>% 
    mutate(npi = as.numeric(paste0(npi))) %>% 
    data.table() 
setkey(tmp2_, npi)

aca_networks_md <- 
    aca_networks[tmp2_,nomatch=0] %>% 
    as_tibble() %>% 
    select(id = npi, network_id,market) %>% 
    mutate(id = paste0(id)) %>%
    group_by(id,network_id,market) %>% 
    filter(row_number()==1) %>% 
    mutate(network_id = ifelse(market=="individual",glue("i{network_id}"),glue("sg{network_id}"))) %>% 
    filter(network_id %in% networks_to_keep) %>% 
    select(id,network_id) %>% 
    fastDummies::dummy_cols("network_id") %>%
    rename_at(vars(starts_with("network_id_")),function(x) gsub("network_id_","",x)) %>% 
    inner_join(df_node_md %>% ungroup() %>% select(id),"id") %>% 
    ungroup() %>% 
    select(-market,-network_id,-geometry) %>% 
    group_by(id) %>% 
    mutate_at(vars(-id),~max(.,na.rm=TRUE)) %>% 
    filter(row_number()==1) 

networks_to_keep2 <- 
    aca_networks_md %>% 
    ungroup() %>% 
    select(-id) %>% 
    summarise_all(mean,na.rm=TRUE) %>% 
    gather(network,value) %>% 
    filter(value >0.1) %>% 
    pull(network)

aca_networks <-
    aca_networks_hosp %>% 
    bind_rows(aca_networks_md) %>% 
    select_at(c("id",networks_to_keep2))


#### MEDICARE ADVANTAGE NETWORKS

ma_plan_types <- c("HMO/HMOPOS","Local PPO","Regional PPO")

# These are downloaded to the desktop to facilitate faster processing, but are stored in data/vericred/2021/Medicare Advantage Researcher Network Data Package - 2021-06-03.zip
ma_network_files <- list.files("~/Desktop/networks-2021/Medicare Advantage Researcher Network Data Package - 2021-06-03/") %>% 
    paste0("~/Desktop/networks-2021/Medicare Advantage Researcher Network Data Package - 2021-06-03/",.) %>% 
    set_names(list.files("~/Desktop/networks-2021/Medicare Advantage Researcher Network Data Package - 2021-06-03/"))

if (!file.exists(here("../analytic-file-creation/data/ma-service-area/service-area-ma_2021.rds"))) {
    # MEDICARE ADVANTAGE
    ma_enrollment_file <- "https://www.cms.gov/files/zip/monthly-enrollment-cpsc-august-2021.zip"
    tmp <- tempfile()  
    download.file(ma_enrollment_file, tmp) 
    df_ma_service_area <- 
        read.csv(unz(tmp, "CPSC_Enrollment_2021_08/CPSC_Enrollment_Info_2021_08.csv")) %>% 
        clean_names() %>% 
        as_tibble()
    df_ma_service_area %>% write_rds(here("../analytic-file-creation/data/ma-service-area/service-area-ma_2021.rds"))
} else {
    df_ma_service_area <- read_rds(here("../analytic-file-creation/data/ma-service-area/service-area-ma_2021.rds"))
}

df_ma_state_service <-
    read.csv(here("../analytic-file-creation/data/ma-service-area/State_Service_Area_2021_09/State_Service_Area_2021_09.csv")) %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    select(contract_number, state_code, entire_state,plan_type) %>%
    mutate(entire_state = ifelse(entire_state=="Yes",1,ifelse(entire_state=="No",0,NA))) %>% 
    rename(state = state_code) %>% 
    mutate(serves_state = 1)

# Overall Denominator file (contains plans not included in Vericred)
df_ma_plans <- 
    df_ma_service_area %>% 
    mutate(enrollment = as.numeric(enrollment)) %>% 
    filter(enrollment >0 & enrollment < Inf) %>% 
    mutate(fips_code = stringr::str_pad(fips_state_county_code, width = 5, pad = "0")) %>% 
    inner_join(df_ma_state_service,c("contract_number","state"))  %>% 
    group_by(fips_code,plan_type,state,contract_number,plan_id) %>% 
    filter(plan_type %in% ma_plan_types) %>% 
    summarise(enrollment = sum(enrollment, na.rm = TRUE))  %>% 
    ungroup() %>% 
    group_by(fips_code,state) %>% 
    mutate(total_enrollment = sum(enrollment, na.rm = TRUE))  %>% 
    mutate(share_enrollment = enrollment / total_enrollment) %>% 
    mutate(plan_id = glue("{contract_number}-{str_pad(plan_id, 3, pad = '0')}")) %>% 
    ungroup() %>% 
    filter(fips_code %in% counties_to_show)

# Rolled up denominator  (not listed at FIPS level...)
df_ma_plans_tot <- 
    df_ma_plans %>% 
    select(contract_number, plan_id,enrollment) %>% 
    group_by(contract_number,plan_id) %>% 
    summarise(enrollment = sum(enrollment)) %>% 
    ungroup() %>% 
    mutate(total_enrollment = sum(enrollment)) 

# MA Plan ID to Vericred Network ID Crosswalk
ma_network_plans <- 
    data.table::fread(ma_network_files[["network_plans.csv"]]) %>% 
    as_tibble() %>% 
    clean_names() %>% 
    #rename(external_plan_id = hios_id) %>% 
    mutate(plan_id = gsub("-[0-9]$","",external_plan_id)) %>% 
    inner_join(  df_ma_plans_tot %>% unique(),c("plan_id")) %>% 
    select(plan_id,network_id,enrollment) %>% 
    inner_join(data.table::fread(ma_network_files[["networks.csv"]]) %>% clean_names() %>% lazy_dt() %>% as.data.table(), "network_id") %>% 
    unique() %>% 
    group_by(plan_id) %>% 
    filter(n()==1) %>% # This gets rid of a few duplicate plan IDs because there are multiple network_names for them...
    data.table()
setkey(ma_network_plans,network_id)

providers_ <-
    data.table::fread(ma_network_files[["providers.csv"]])   %>%
    lazy_dt() %>%
    mutate(npi = paste0(npi)) %>%
    dplyr::select(provider_id,npi) %>%
    as.data.table()
setkey(providers_,provider_id)

ma_networks_ <- 
    data.table::fread(ma_network_files[["network_providers.csv"]]) %>% 
    lazy_dt() %>% 
    as.data.table()
setkey(ma_networks_,network_id)

ma_networks <- 
    ma_networks_[ma_network_plans, nomatch = 0] 
setkey(ma_networks,provider_id)
ma_networks <- 
    ma_networks[providers,nomatch = 0]

# Merge in the hospital ID
tmp_ <- 
    df_hosp_xw %>%  
    mutate(npi = as.numeric(paste0(npi))) %>% 
    data.table()
setkey(tmp_, npi)
setkey(ma_networks,npi)

ma_networks_hosp <- 
    ma_networks[tmp_,nomatch=0] %>% 
    mutate(market = "medicareadv") %>% 
    as_tibble() %>% 
    select(provider_id,network_id,npi,market,prvnumgrp) %>% 
    mutate(npi = paste0(npi)) %>% 
    mutate(id = prvnumgrp) %>% 
    select(id,network_id,market) %>% 
    group_by(id,network_id) %>% 
    filter(row_number()==1) %>% 
    mutate(network_id = ifelse(market=="medicareadv",glue("ma{network_id}"),glue("XX{network_id}"))) %>% 
    select(id,network_id) %>% 
    fastDummies::dummy_cols("network_id") %>% 
    select(id,starts_with("network_id_")) %>% 
    group_by(id) %>% 
    mutate_at(vars(starts_with("network_id_")),~max(.,na.rm=TRUE)) %>% 
    filter(row_number()==1) %>% 
    rename_at(vars(starts_with("network_id_")),function(x) gsub("network_id_","",x)) %>% 
    inner_join(df_node_hosp %>% select(id),"id")

networks_to_keep <- 
    ma_networks_hosp %>% 
    ungroup() %>% 
    data.frame() %>% 
    select(-geometry) %>% 
    select(-id) %>% 
    summarise_all(mean) %>% 
    gather(network,value) %>% 
    filter(value>.1) %>% 
    pull(network)

# Get the physician-level Netowrk Data
tmp2_ <- 
    df_node_md %>% 
    select(npi = id) %>% 
    mutate(npi = as.numeric(paste0(npi))) %>% 
    data.table() 
setkey(tmp2_, npi)

ma_networks_md <- 
    ma_networks[tmp2_,nomatch=0] %>% 
    mutate(market = "medicareadv") %>% 
    as_tibble() %>% 
    select(id = npi, network_id,market) %>% 
    mutate(id = paste0(id)) %>%
    group_by(id,network_id,market) %>% 
    filter(row_number()==1) %>% 
    mutate(network_id = ifelse(market=="medicareadv",glue("ma{network_id}"),glue("XX{network_id}"))) %>% 
    filter(network_id %in% networks_to_keep) %>% 
    select(id,network_id) %>% 
    fastDummies::dummy_cols("network_id") %>%
    rename_at(vars(starts_with("network_id_")),function(x) gsub("network_id_","",x)) %>% 
    inner_join(df_node_md %>% ungroup() %>% select(id),"id") %>% 
    ungroup() %>% 
    select(-market,-network_id,-geometry) %>% 
    group_by(id) %>% 
    mutate_at(vars(-id),~max(.,na.rm=TRUE)) %>% 
    filter(row_number()==1) 

networks_to_keep2 <- 
    ma_networks_md %>% 
    ungroup() %>% 
    select(-id) %>% 
    summarise_all(mean,na.rm=TRUE) %>% 
    gather(network,value) %>% 
    filter(value >0.1) %>% 
    pull(network)

ma_networks <-
    ma_networks_hosp %>% 
    bind_rows(ma_networks_md) %>% 
    select_at(c("id",networks_to_keep2))

#########################################
### Save Final Edge and Node List Data
##########################################

df_node <- 
    df_node_hosp %>% 
    ungroup() %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    unnest(cols = c(x,y)) %>% 
    bind_rows(df_node_md %>% as_tibble() %>% select(-geometry))  %>% 
    data.frame() %>% 
    mutate(type=paste0(type),
           specialty_group = paste0(specialty_group)) %>% 
    mutate(type = ifelse(type!="md" ,"hospital",type)) %>% 
    mutate(specialty_group = ifelse(is.na(specialty_group) | specialty_group=="NA","hospital",specialty_group)) %>% 
    inner_join(aca_networks,"id") %>% 
    inner_join(ma_networks,"id") %>% 
    mutate(located_in_area = as.integer(fips_code %in% counties_to_show))

df_edge_ <- 
    df_edge_npi %>% 
    data.frame()

df_edge <- 
    df_edge_ %>% 
    filter(!(from %in% setdiff(df_edge_$from,df_node$id)) & !(to %in% setdiff(df_edge_$to,df_node$id)))

df_edge %>% 
    write_rds(here(glue("output/clinical-edge-data-{paste0(target_area,collapse = '-')}.rds")))

df_node %>% 
    write_rds(here(glue("output/clinical-node-data-{paste0(target_area,collapse = '-')}.rds")))


#####################################
#### NOW ADD THE PATIENT FLOW DATA
#####################################

cent_zip <- sf_zip_ %>% st_centroid()
zips_to_show <- cent_zip %>% 
    filter(row_number() %in% unlist(st_intersects(sf_rating, cent_zip))) %>% 
    pull(zcta5ce10)
sf_zip_final <- cent_zip %>% filter(zcta5ce10 %in% zips_to_show)

# Created in R/read-and-tidy-cms-hospital-service-areas
df_edge_hosp_zip <- 
    read_rds(here("../../health-care-markets/output/hospital-county-patient-data/2018/hospital-zip-patient-data.rds")) %>% 
    filter(prvnumgrp %in% df_node$id) %>% 
    filter(zip_code %in% zips_to_show) %>% 
    select(from= zip_code, to = prvnumgrp, weight = total_cases)

df_node_hosp_zip <- 
    sf_zip_final %>% 
    as_tibble() %>% 
    mutate(x = map(geometry,~(st_coordinates(.x)[1]))) %>% 
    mutate(y = map(geometry,~(st_coordinates(.x)[2]))) %>% 
    unnest(cols = c(x,y)) %>% 
    select(-geometry) %>% 
    mutate(id = zcta5ce10) %>% 
    left_join(
        st_within(sf_zip_final,sf_hsa_) %>% 
            data.frame() %>% 
            right_join(sf_zip_final %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_hsa_ %>% data.frame() %>% select(hsanum) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,hsanum),"zcta5ce10") %>% 
    left_join(
        st_within(sf_zip_final,sf_county) %>% 
            data.frame() %>% 
            right_join(sf_zip_final %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_county %>% data.frame() %>% select(fips_code) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,fips_code),"zcta5ce10") %>% 
    left_join(
        st_within(sf_zip_final,sf_rating) %>% 
            data.frame() %>% 
            right_join(sf_zip_final %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_rating %>% data.frame() %>% select(ratng_r) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,ratng_r),"zcta5ce10") %>% 
    left_join(
        st_within(sf_zip_final,sf_commuting_) %>% 
            data.frame() %>% 
            right_join(sf_zip_final %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_commuting_ %>% data.frame() %>% select(cz_id) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,cz_id),"zcta5ce10") %>% 
    left_join(
        st_within(sf_zip_final,sf_pcsa_) %>% 
            data.frame() %>% 
            right_join(sf_zip_final %>% data.frame() %>% select(zcta5ce10) %>% mutate(row.id = row_number())) %>% 
            left_join(sf_pcsa_ %>% data.frame() %>% select(pcsa) %>% mutate(col.id = row_number()),"col.id") %>% 
            select(zcta5ce10,pcsa),"zcta5ce10") %>% 
    mutate(id = zcta5ce10) %>% 
    select(id, zcta5ce10, hsanum,fips_code,ratng_r,cz_id,pcsa,x,y) %>% 
    mutate(type = "zip_code") 
    
df_node_flow <- 
    df_node_hosp %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    unnest(cols= c(x,y)) %>% 
    mutate(type = "hospital") %>% 
    inner_join(aca_networks,"id") %>% 
    inner_join(ma_networks,"id") %>% 
    bind_rows(df_node_hosp_zip) %>% 
    as_tibble() %>% 
    mutate(zip_code = zcta5ce10) %>% 
    mutate(located_in_area = as.integer(fips_code %in% counties_to_show))

df_edge_flow_ <- 
    df_edge_hosp_zip

df_edge_flow <- 
    df_edge_flow_ %>% 
    filter(!(from %in% setdiff(df_edge_flow_$from,df_node_flow$id)) & !(to %in% setdiff(df_edge_flow_$to,df_node_flow$id)))

df_edge_flow %>% 
    write_rds(here(glue("output/flow-edge-data-{paste0(target_area,collapse = '-')}.rds")))

df_node_flow %>% 
    write_rds(here(glue("output/flow-node-data-{paste0(target_area,collapse = '-')}.rds")))

