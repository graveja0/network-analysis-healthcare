# library(googleway)
# key <- c("<GET KEY>")
# #
# tn_cities_xy <-
#   c("Nashville","Knoxville","Columbia","Memphis","Cookeville","Chattanooga","Franklin","Clarksville","Murfreesboro","Jackson","Gatlinburg","Johnson City") %>%
#   map_df(~(
#     google_geocode(address = glue::glue("{.x}, TN"), key = key)$results$geometry  %>%
#       pluck("location") %>%
#       tbl_df() %>%
#       mutate(city = .x)
#     #pluck(1)
#     # mutate(city = .x,
#     #        lat = location.lat,
#     #        long = location.lng)
#    ))
# tn_cities_xy %>%
#   st_as_sf(coords = c("lng", "lat"), remove = FALSE,
#            crs = 4326, agr = "constant") %>%
#   write_rds("data/misc/xy-coords-tn-cities.rds")

