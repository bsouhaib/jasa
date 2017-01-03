rm(list = ls())
source("config_paths.R")

library(data.table)
library(dplyr)
library(lubridate)
library(gdata)

#system("aws s3 ls  s3://code-old-ukmeters")

library("aws.s3")
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAI7CHVBJDZPHKX72Q",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "ap-southeast-2",
           "AWS_SESSION_TOKEN" = "")
bucketlist()
get_bucket(bucket = 'code-old-ukmeters')

# Copy data set from S3
if(FALSE){
  system("aws s3 cp  s3://data-ukmeters/edrp_elec.csv /home/rstudio/codemeters/data/")
}

save_object("edrp_geography_data.xlsx", bucket = "data-ukmeters", 
            file = file.path(data.folder, "edrp_geography_data.xlsx"))
save_object("edrp_metadata.xlsx", bucket = "data-ukmeters", 
            file = file.path(data.folder, "edrp_metadata.xlsx"))


# Create Rdata files and save them in S3
DT <- fread(file.path(data.folder, "edrp_elec.csv"))
DT <- tbl_df(DT) %>% rename(IDMETER = ANON_ID) 



# Delete data set in S3 + in instance

