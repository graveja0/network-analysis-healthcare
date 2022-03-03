# Simluate a health care system!
library(tidyverse)
library(glue)
library(sf)
library(here)

sf <- sf::read_sf(here("data/shp/tn04.shp"))
xy <- read_rds(here("data/coordinates.rds"))
hosp_xy <- 
    xy %>% filter(type=="HOSP") %>% 
    mutate(sysname = ifelse(sysname=="",NA,sysname)) %>% 
    mutate(sysname = coalesce(sysname,mname)) %>% 
    st_as_sf(coords = c("X", "Y"), crs = 4326) 

hosp_location <- 
    cbind.data.frame(id = hosp_xy$id,location = sf$zcta5ce10[unlist(st_within(hosp_xy,sf))]) %>% 
    as_tibble()

#sf %>% ggplot() + geom_sf() + geom_sf(data = hosp_xy,aes(colour = sysname))

N = 20000
Z = length(unique(sf$zip_code))
S = length(unique(hosp_xy$sysname))

# Individual patients
unit <- 1:N
unit_fe <- rnorm(N, 0, 0.5)

zips <- xy %>% filter(type=="zip") %>% select(id,pop10) %>% pull(id)
pop <- xy %>% filter(type=="zip") %>% select(id,pop10) %>% pull(pop10)

geography <- sample(zips,size = N, prob = pop/sum(pop), replace = TRUE) 

patients <- data.frame(i = unit, i_fe = unit_fe, z = geography) %>% 
    as_tibble()  %>% 
    mutate(runif = runif(n=nrow(.)))

# Distances

dist <- 
    xy %>% 
    st_as_sf(coords= c("X","Y"), crs=4326) %>% 
    sf::st_distance() 
colnames(dist) = rownames(dist) = xy$id
df_dist <- 
    dist %>% 
    data.frame() %>% 
    rownames_to_column(var = "id1") %>% 
    gather(id2,distance,-id1) %>% 
    as_tibble() %>% 
    mutate(id2 = gsub("^X","",id2)) %>% 
    filter(id1!=id2) %>% 
    mutate(miles = distance / 1609.34) %>% 
    filter(id1 %in% zips & !(id2 %in% zips)) %>% 
    rename(z = id1, npi = id2)

# Health Systems
systems <- unique(xy$system_id) %>% na.omit()
system_attributes <- 
    data.frame(system = systems) %>% 
    mutate(system_quality = rnorm(nrow(.),0, 1)) %>% 
    rowwise() %>% 
    mutate(network_a = as.integer(runif(1)>.25),
           network_b = as.integer(runif(1)>.5),
           network_c = as.integer(runif(1)>.75))

hospitals <- 
    xy %>% 
    select(npi = id, system = system_id,mname,hsanum,fips_code) %>% 
    na.omit() %>% 
    left_join(system_attributes,"system") %>% 
    ungroup() %>% 
    mutate(facility_quality = rnorm(nrow(.), 0, 0.1)) %>% 
    mutate(quality = system_quality + facility_quality) %>% 
    left_join(hosp_location,c("npi" = "id")) %>% 
    rename(zip_code = location)  %>% 
    select(npi,mname,system,system_quality,facility_quality,quality,zip_code,mname,hsanum,fips_code,starts_with("network_"))


# Covariate Matrix
df_ <- 
    expand.grid(patients$i, hospitals$npi) %>% 
    as_tibble() %>% 
    set_names(c("i","npi")) %>% 
    left_join(patients,"i") %>% 
    left_join(hospitals,"npi") %>% 
    left_join(df_dist,c("z","npi")) %>% 
    arrange(i,npi) %>% 
    mutate(miles = as.numeric(miles)) %>% 
    #mutate(v =  .5 + 1 * quality * -i_fe + -.025 * miles^2) %>% 
    mutate(v =  .5 + 2 * quality  + -.025 * miles^2) %>% 
    mutate(utility = exp(v + rnorm(n = nrow(.),mean=0,sd=.05))) %>% 
    group_by(i) %>% 
    mutate(denom = sum(utility)) %>% 
    mutate(probability = utility / denom) %>% 
    mutate(cumprob = cumsum(probability)) %>% 
    filter(cumprob > runif) %>% 
    filter(row_number()==1); df_

df_ %>% select(i,npi,miles,quality,probability,cumprob) %>% filter(i==1) %>% data.frame() %>% mutate(probability = round(probability,3))


# mutate(choice = as.integer(utility==max(utility))) %>% 
# filter(choice==1)

# 
# params <- 
#     list(
#         N = 20000,        # Number of patients
#         Z = 15,           # Number of geographies
#         S = 3,            # Number of health systems
#         H = 2,            # Average number of hospitals within each health system
#         D = 30,           # Average number of physicians within each health system
#         I = 3             # Number of insurers
#     )
# 
# attach(params)
# 
# set.seed(1234)
# 
# # Patient geography
# geography <- sample(rep(1:Z, N/Z), N, replace = TRUE)
# 
# patients <- data.frame(i = unit, i_fe = unit_fe, z = paste0("ZIP_",LETTERS[geography])) %>% 
#     as_tibble()  %>% 
#     mutate(runif = runif(n=nrow(.)))
# 
# 
# # Health Systems
# systems <- paste0("SYS",1:S)
# n_hospitals <- rpois(S,H) 
# system_attributes <- 
#     data.frame(system = systems) %>% 
#     mutate(system_quality = rnorm(nrow(.),0, 1)) %>% 
#     rowwise() %>% 
#     mutate(network_a = as.integer(runif(1)>.25),
#            network_b = as.integer(runif(1)>.5),
#            network_c = as.integer(runif(1)>.75))
#     
# hospitals <- 
#     map2(systems,n_hospitals,~rep(.x,.y))  %>% 
#     unlist() %>% 
#     data.frame() %>% 
#     set_names("system") %>% 
#     group_by(system) %>% 
#     mutate(npi = as.character(glue("NPI{row_number()}_{system}"))) %>% 
#     left_join(system_attributes, "system") %>% 
#     ungroup() %>% 
#     mutate(facility_quality = rnorm(nrow(.), 0, 0.1)) %>% 
#     mutate(quality = system_quality + facility_quality) %>% 
#     rowwise() %>% 
#     mutate(location = paste0("ZIP_",LETTERS[sample(1:Z,1,replace=TRUE)])) %>% 
#     select(npi, system,location,system_quality,facility_quality,quality,starts_with("network_"))
# 
# # Covariate Matrix
# df_ <- 
#     expand.grid(patients$i, hospitals$npi) %>% 
#     as_tibble() %>% 
#     set_names(c("i","npi")) %>% 
#     left_join(patients,"i") %>% 
#     left_join(hospitals,"npi") %>% 
#     mutate(same_location = as.integer(z == location)) %>% 
#     arrange(i,npi) %>% 
#     mutate(v = 0 * i_fe + .3 * quality + 1 * same_location) %>% 
#     mutate(utility = exp(v + rnorm(n = nrow(.),mean=0,sd=.05))) %>% 
#     group_by(i) %>% 
#     mutate(denom = sum(utility)) %>% 
#     mutate(probability = utility / denom) %>% 
#     mutate(cumprob = cumsum(probability)) %>% 
#     filter(cumprob > runif) %>% 
#     filter(row_number()==1); df_
# 
#     # mutate(choice = as.integer(utility==max(utility))) %>% 
#     # filter(choice==1)
# 
#

df_ %>% ungroup() %>% count(z,npi) %>% spread(npi,n)

df_node_ <- 
    df_ %>% 
    ungroup() %>% 
    select(npi,z) %>% 
    gather(type,id) %>% 
    unique() %>% 
    select(id,type)  

df_node <- 
    df_node_ %>% 
    left_join(xy %>% select(id,system_id,sysname,admtot,zip_code,hsanum,fips_code), "id")  %>% 
    left_join(hospitals %>% select(id = npi, starts_with("network_")),"id")

df_edge <- 
    df_ %>% 
    ungroup() %>% 
    count(z,npi) %>% 
    set_names(c("from","to","weight"))

# # Construct Node List Data
# N = 26
# K = 3
# set.seed(12345)
# df_node <- 
#     data.frame(id = paste0("npi_",LETTERS[1:N])) %>% 
#     rowwise() %>% 
#     mutate(type = sample(c("MD","HOSP"), replace =TRUE, prob = c(0.8,0.2), size=1)) %>% 
#     mutate(system = sample(LETTERS[1:3], replace =TRUE, prob = c(0.25 ,0.5, 0.25), size=1)) %>% 
#     mutate(geomarket = sample(c("M1","M2","M3"), replace = TRUE, prob = c(0.5, 0.25, 0.25), size = 1)) %>% 
#     mutate(network_a = sample(0:1, replace =TRUE , prob = c(0.25,0.75), size =1)) %>% 
#     mutate(network_b = sample(0:1, replace =TRUE , prob = c(0.5,0.5), size =1)) %>% 
#     mutate(network_c = sample(0:1, replace =TRUE , prob = c(0.75,0.25), size =1)) %>% 
#     as_tibble()
# df_node 
# 
# # Construct edge list data
# set.seed(12345)
# df_edge <- 
#     expand.grid(df_node$id,df_node$id) %>% 
#     set_names(c("from_id","to_id")) %>% 
#     mutate(weight= rnbinom(n=nrow(.),mu=10,size = 1))  %>% 
#     as_tibble() %>% 
#     filter(from_id != to_id)
# df_edge 
