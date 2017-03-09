##=============================================================================
## This file contains foundational analytic functions  
##                                                                         
## The functions include:
##  - createInspectionsABT
##  - createBusinessABT
##=============================================================================

# mywd <- "/Users/bryanrulli/Dropbox/Edu/Stats290\ -\ Paradigms\ for\ Computing\ with\ Data\ -\ 2017/Project/package"
# setwd(mywd)
# getwd()

##=============================================================================
## Set default data file paths.
##=============================================================================
food_inspect_rds <- paste0(mywd, "data/FoodInspections.Rds")

##=============================================================================
## createBusinessABT
##
## createBusinessABT is a utility function that builds the analytic base table
##  at the business level.  
##
## @return A data frame that summarizes inspection results by business.  
## @export
## @examples
##  
createBusinessABT <- function(data = food_inspect_rds, snap_date = NA) {
    rs <- readRDS(data)

    if (!is.na(snap_date)) 
        rs <- rs[rs$Inspection_Date <= snap_date, ]
    
    bizABT <- rs %>% 
        group_by(business_id, business_name) %>%
        mutate(complaint = if_else(inspection_type == "Complaint", inspection_id, NA_character_)
               ,highrisk = if_else(risk_category == "High Risk", violation_id, NA_character_)) %>%
        summarize(count = n()
                  ,inspections = n_distinct(inspection_id)
                  ,violations = n_distinct(violation_id)
                  ,complaints = n_distinct(complaint, na.rm = TRUE)
                  ,highrisk_viols = n_distinct(highrisk, na.rm = TRUE)
                  ,first_inspection = min(inspection_date)
                  ,last_inspection = max(inspection_date)                      
        ) %>%
        mutate(viol_per_insp = violations / inspections
               ,comp_per_insp = complaints / inspections
               ,highrisk_per_viol = highrisk_viols / violations
               ,highrisk_per_insp = highrisk_viols / inspections                   
               ,days_since_first_insp = last_inspection - first_inspection
               ,days_since_last_insp = difftime(Sys.Date(), last_inspection) 
        )  %>%
        arrange(desc(count))
    
    return(bizABT)
}


##=============================================================================
## createInspectionABT
##
## createInspectionABT is a utility function that builds the analytic base table
##  at the inspection level.  
##
## @return A data frame that summarizes inspection results across various features.  
## @export
## @examples
##  
##=============================================================================
createInspectionABT <- function(data = food_inspect_rds, 
                                snap_date = NA) {
    
    if(file.exists(data)) {
        
        rs <- readRDS(data)
    
        if (!is.na(snap_date)) 
            rs <- rs[rs$inspection_date <= snap_date, ]
        
        # Summarize inspection history
        inspections <- rs %>% 
            mutate(complaint = if_else(inspection_type == "Complaint", inspection_id, NA_character_),
                   followup = if_else(inspection_type %in% c("Reinspection/Followup", "Complaint Reinspection/Followup"), inspection_id, NA_character_),
                   highrisk_viol = if_else(risk_category == "High Risk", violation_id, NA_character_),
                   newownerconst = if_else(inspection_type %in% c("New Ownership", "New Construction"), inspection_id, NA_character_),
                   foodillness = if_else(inspection_type %in% c("Foodborne Illness Investigation"), inspection_id, NA_character_)
            ) %>%            
            group_by(inspection_id, 
                     inspection_date,
                     inspection_type,
                     inspection_score,                     
                     business_name,
                     business_id,
                     business_latitude,
                     business_longitude,
                     business_postal_code
                     ) %>%
            summarize(count = n()
                      ,violations = n_distinct(violation_id)
                      ,complaint = n_distinct(complaint, na.rm = TRUE)
                      ,followup = n_distinct(followup, na.rm = TRUE)
                      ,highrisk_viols = n_distinct(highrisk_viol, na.rm = TRUE)
                      ,newownerconst = n_distinct(newownerconst, na.rm = TRUE)
            ) %>%
            mutate(highrisk_ratio = highrisk_viols / violations, 
                   critical_found = pmin(1, highrisk_viols),
                   non_criticals_found = violations - highrisk_viols
            ) %>%                
            arrange(business_id, inspection_date)
        
        # Create a copy of the inspection history that will be used for the rolling join.        
        inspect_subset <- inspections %>% 
            select(b_inspection_id = inspection_id,
                   b_inspection_date = inspection_date,
                   b_business_id = business_id,
                   b_critical_found = critical_found,
                   b_violations = violations,
                   b_complaint = complaint,
                   b_followup = followup,
                   b_highrisk_viols = highrisk_viols,
                   b_inspection_type = inspection_type,
                   b_inspection_score = inspection_score,                     
                   b_business_name = business_name,
                   b_business_latitude = business_latitude,
                   b_business_longitude = business_longitude,
                   b_business_postal_code = business_postal_code                   
            ) 
            
        # For each inspection observation, join in all prior inspections. 
        # Then, summarize the prior inspection history back to each 
        #  inspection observation.
        # This creates the cumulative "look-back" context for 
        #  each new inspection.  
        businessInsp <- inspections %>% 
            inner_join(inspect_subset, by = c("business_id" = "b_business_id")) %>%
            filter(inspection_date > b_inspection_date) %>%
            group_by(inspection_id, 
                     inspection_date
            ) %>%
            summarize(prior_inspections = n()
                      ,prior_violations = sum(b_violations)
                      ,prior_complaints = sum(b_complaint)
                      ,prior_followups = sum(b_followup)
                      ,prior_highrisk_viols = sum(b_highrisk_viols)
                      ,prior_inspection_date = max(b_inspection_date)
                      ,first_inspection_date = min(b_inspection_date)
            ) %>%  
            select(b_inspection_id = inspection_id,
                   b_inspection_date = inspection_date,
                   prior_inspections,
                   prior_violations,
                   prior_complaints,
                   prior_followups,
                   prior_highrisk_viols,
                   prior_inspection_date,
                   first_inspection_date
                   ) %>%
            arrange(b_inspection_id) 
                 
        # Join the inspection detail with the summarized "look-back" context. 
        inspectionABT1 <- inspections %>% 
            left_join(businessInsp, by = c("inspection_id" = "b_inspection_id")) %>%
            mutate(prior_highrisk_ratio = prior_highrisk_viols / prior_violations, 
                   prior_critical_found = pmin(1, prior_highrisk_viols),
                   prior_non_criticals_found = prior_violations - prior_highrisk_viols,
                   days_since_first_insp = as.integer(inspection_date - first_inspection_date),
                   days_since_last_insp = as.integer(difftime(inspection_date, prior_inspection_date)),
                   first_inspection = if_else(is.na(prior_inspection_date), 1, 0)
            ) %>% 
            arrange(inspection_id, inspection_date) 
        
        weather_3day$Inspection_Date <- as.Date(weather_3day$date, format = "%Y-%m-%d")
        inspectionABT1$inspection_date <- as.Date(inspectionABT1$inspection_date, format = "%Y-%m-%d")

        # Join the weather data. 
        inspectionABT <- inspectionABT1 %>% 
            left_join(weather_3day, by = c("inspection_date" = "Inspection_Date")) %>%
            select(-date)

        return(inspectionABT)
        
    } else 
        stop(paste0("File does not exist: ", data))
}

##=============================================================================
## createPostalCodeABT
## 
##=============================================================================
createPostalCodeABT <- function(data = food_inspect_rds, snap_date = NA) {
    rs <- readRDS(data)
    
    if (!is.na(snap_date)) 
        rs <- rs[rs$Inspection_Date <= snap_date, ]
    
    postals <- data %>% 
        group_by(business_postal_code) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
    
    #WORK IN PROGRESS 
}

##=============================================================================
## createTimeABT
##
##=============================================================================
createTimeABT <- function(data = food_inspect_rds, snap_date = NA) {
    rs <- readRDS(data)
    
    if (!is.na(snap_date)) 
        rs <- rs[rs$Inspection_Date <= snap_date, ]
    
    #WORK IN PROGRESS 
}

##=============================================================================
## createNeighborhoodABT
##
##=============================================================================
createNeighborhoodABT <- function(data = food_inspect_rds, snap_date = NA) {
    rs <- readRDS(data)
    
    if (!is.na(snap_date)) 
        rs <- rs[rs$Inspection_Date <= snap_date, ]
    
    #WORK IN PROGRESS     
}

