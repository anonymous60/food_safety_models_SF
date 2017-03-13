
##=============================================================================
## This file contains foundational API functions that downlaod and process data 
##  required for the SF Food Inspection package. 
## These helper functions are primarily designed to be used indirectly by 
##  through S4 objects.   
##                                                                         
## The functions include:
##  - loadFoodInspections
##
##=============================================================================

##=============================================================================
## Socrata login credentials
##=============================================================================
email <- "bryanrulli@yahoo.com"
password <- "SFOpenPortal290"

##=============================================================================
## Set data source urls and api keys.
##=============================================================================
food_inspect_url <- "https://data.sfgov.org/resource/sipz-fjte.csv"
sf311_calls_url <- "https://data.sfgov.org/resource/ktji-gk7t.csv"
sfcrime_url <- "https://data.sfgov.org/resource/cuks-n6tp.json"
sfcrime_url2 <- "https://data.sfgov.org/resource/gxxq-x39z.json"

##=============================================================================
## Set default data file paths.
##=============================================================================
food_inspect_rds <- "/data/FoodInspections.Rds"
sf311_calls_rds <- "/data/SF311Calls.Rds"
sfcrime_rds <- "/data/SFCrimes.Rds"


##=============================================================================
## require(RSocrata);
##=============================================================================

##=============================================================================
## loadFoodInspections
##
## loadFoodInspections is a utility function that pulls the Food 
##  Insepction Restaurant Scores data from the Socrata API and creates the
##  base Food Inspection table used for subsequent processing.  
##
## @importFrom RSocrata
##
## @param url 
## @return The Food Inspections data frame.  
## @export
## @examples
##  
loadFoodInspections <- function(url = food_inspect_url, 
                                       outdata = food_inspect_rds) {
    rs <- read.socrata(url, stringsAsFactors = FALSE)
    rs$violation_key <- sapply(strsplit(rs$violation_id, "\\_"), "[",3) 
    saveRDS(rs, outdata)
    return(rs)
}


##=============================================================================
## load311Calls
##
## load311Calls is a utility function that pulls the San Francisco 311 call 
##  data from the Socrata API and creates the
##  base SF311Calls table used for subsequent processing.  
##
## @importFrom RSocrata
##
## @param url 
## @return The SF311Calls data frame.  
## @export
## @examples
##  
load311Calls <- function(url = sf311_calls_url, 
                                outdata = sf311_calls_rds) {
    rs <- read.socrata(url, stringsAsFactors = FALSE)
    saveRDS(rs, outdata)
    return(rs)
}

##=============================================================================
## loadSFCrime
##
## loadSFCrime is a utility function that pulls the San Francisco crime data
##  from the Socrata API and creates the
##  base SFCrimes table used for subsequent processing.  
##
## @importFrom RSocrata
##
## @param url 
## @return The SFCrime data frame.  
## @export
## @examples
##  
loadSFCrime <- function(url = sfcrime_url, 
                                outdata = sfcrime_rds) {
    rs <- read.socrata(url, stringsAsFactors = FALSE)
    saveRDS(rs, outdata)
    return(rs)
}



