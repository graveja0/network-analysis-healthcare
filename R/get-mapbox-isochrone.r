get_mapbox_isochrone <- function(long, lat, contours_minutes, base_url = "https://api.mapbox.com/", mapbox_token = "") {
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