#********
# Generate the Restaurant Scores data set from SF open data 
# and store it as SFopenData_Restaurant_Scores.Rds 
#********


generate_Rscore_data <- function(){
  if(!file.exists("data_source/SFopenData_Restaurant_Scores.Rds")
     || !file.exists("data_source/SFopendata.sqlite")){
    
      require(readr)
      require(dplyr)
      require(anytime)
      require(RSQLite)
      new_table = read.csv("data_source/Restaurant_Scores_-_LIVES_Standard.csv",stringsAsFactors=FALSE)
      new_table$inspection_date = anydate(new_table$inspection_date)
      new_table$violation_code = sapply(strsplit(new_table$violation_id,"\\_"),"[",3)
      
      d1  = new_table[,c("business_id", 
                       "business_postal_code", 
                       "business_latitude", 
                       "business_longitude",
                       "inspection_date",
                       "inspection_score",
                       "inspection_type",
                       "violation_code", 
                       "violation_description",
                       "risk_category")]
      saveRDS(d1, file = "data_source/SFopenData_Restaurant_Scores.Rds")
      
      
      m <- dbDriver(drv = "SQLite")
      con <- dbConnect(m, dbname = "data_source/SFopendata.sqlite")
      dbWriteTable(con, "d1", d1, overwrite = TRUE)
      
      
  }else{
    
    print("SFopenData_Restaurant_Scores.Rds or SFopendata.sqlite already exist!")
    
  }

}