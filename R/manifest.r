source("~/auth-aws.r")
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")

libraries_needed <- c("sf",
                      "tidyverse",
                      "aws.s3",
                      "furrr",
                      "foreach",
                      "tictoc",
                      "here",
                      "igraph",
                      "ggraph",
                      "tidygraph",
                      "gt",
                      "matrixStats",
                      "knitr",
                      "glue",
                      "usethis",
                      "readr",
                      "data.table",
                      "dtplyr",
                      "conflicted",
                      "usethis",
                      "glue",
                      "fastDummies",
                      "patchwork",
                      "magrittr",
                      "flyio",
                      "ggthemes",
                      "ggsci",
                      "hrbrthemes",
                      "ggrepel",
                      "ggdendro")
x <- lapply(libraries_needed, require, character.only = TRUE)

x <- lapply(libraries_needed, require, character.only = TRUE)
# Take care of package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("count","dplyr") 
conflict_prefer("transpose", "purrr")

markets_project_bucket = "health-care-markets"
networks_project_bucket = "vumc.graves.networks.proj"
flyio_set_datasource("s3")
flyio::flyio_set_bucket(markets_project_bucket)

states <- c(
    "AK",    "AL",    "AR",    "AZ",    "CA",    "CO",    "CT",    "DC",    "DE",    
    "FL",    "GA",    "HI",    "IA",    "ID",    "IL",    "IN",    "KS",    "KY",    
    "LA",    "MA",    "MD",    "ME",    "MI",    "MN",    "MO",    "MS",    "MT",    
    "NC",    "ND",    "NE",    "NH",    "NJ",    "NM",    "NV",    "NY",    "OH",    
    "OK",    "OR",    "PA",    "RI",    "SC",    "SD",    "TN",    "TX",    "UT",    
    "VA",    "VT",    "WA",    "WI",    "WV",    "WY"
)

