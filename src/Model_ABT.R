
#' Food inspection input
#'
#' Analytical base table for classification models, such as the Logistic regression, Random forest
#' @format A data frame with y rows and x variables:
#' \describe{
#'   \item{Neighborhood}{Neighborhood, in x units}
#'   \item{arrestsPast6mo}{arrestsPast6mo, in x units}
#'   \item{arrestsPast90d}{arrestsPast90d, in x units}
#'   \item{arrestsPastWeek}{arrestsPastWeek, in x units}
#'   \item{arrestsPastYear}{arrestsPastYear, in x units}
#'   \item{b_inspection_date}{b_inspection_date, in x units}
#'   \item{burgsPast6mo}{burgsPast6mo, in x units}
#'   \item{burgsPast90d}{burgsPast90d, in x units}
#'   \item{burgsPastWeek}{burgsPastWeek, in x units}
#'   \item{burgsPastYear}{burgsPastYear, in x units}
#'   \item{business_id}{business_id, in x units}
#'   \item{business_latitude}{business_latitude, in x units}
#'   \item{business_longitude}{business_longitude, in x units}
#'   \item{business_name}{business_name, in x units}
#'   \item{business_postal_code}{business_postal_code, in x units}
#'   \item{complaint}{complaint, in x units}
#'   \item{count}{count, in x units}
#'   \item{crimePast6mo}{crimePast6mo, in x units}
#'   \item{crimePast90d}{crimePast90d, in x units}
#'   \item{crimePastWeek}{crimePastWeek, in x units}
#'   \item{crimePastYear}{crimePastYear, in x units}
#'   \item{critical_found}{critical_found, in x units}
#'   \item{days_since_first_insp}{days_since_first_insp, in x units}
#'   \item{days_since_last_insp}{days_since_last_insp, in x units}
#'   \item{first_inspection}{first_inspection, in x units}
#'   \item{first_inspection_date}{first_inspection_date, in x units}
#'   \item{followup}{followup, in x units}
#'   \item{highrisk_ratio}{highrisk_ratio, in x units}
#'   \item{highrisk_viols}{highrisk_viols, in x units}
#'   \item{inspection_date}{inspection_date, in x units}
#'   \item{inspection_id}{inspection_id, in x units}
#'   \item{inspection_score}{inspection_score, in x units}
#'   \item{inspection_type}{inspection_type, in x units}
#'   \item{last_critical_found}{last_critical_found, in x units}
#'   \item{last_highrisk_viols}{last_highrisk_viols, in x units}
#'   \item{last_inspection_date}{last_inspection_date, in x units}
#'   \item{last_noncritical_found}{last_noncritical_found, in x units}
#'   \item{last_noncritical_viols}{last_noncritical_viols, in x units}
#'   \item{maxhumidity}{maxhumidity, in x units}
#'   \item{maxtemp_F}{maxtemp_F, in x units}
#'   \item{meanwindspdm}{meanwindspdm, in x units}
#'   \item{newownerconst}{newownerconst, in x units}
#'   \item{non_critical_found}{non_critical_found, in x units}
#'   \item{non_critical_viols}{non_critical_viols, in x units}
#'   \item{precipi}{precipi, in x units}
#'   \item{prior_complaints}{prior_complaints, in x units}
#'   \item{prior_critical_found}{prior_critical_found, in x units}
#'   \item{prior_followups}{prior_followups, in x units}
#'   \item{prior_highrisk_ratio}{prior_highrisk_ratio, in x units}
#'   \item{prior_highrisk_viols}{prior_highrisk_viols, in x units}
#'   \item{prior_inspections}{prior_inspections, in x units}
#'   \item{prior_noncritical_found}{prior_noncritical_found, in x units}
#'   \item{prior_noncritical_viols}{prior_noncritical_viols, in x units}
#'   \item{prior_violations}{prior_violations, in x units}
#'   \item{violations}{violations, in x units}
#' }
#'
"Model_ABT"
