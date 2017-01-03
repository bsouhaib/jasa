rm(list = ls())

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
save_object("edrp_geography_data.xlsx", bucket = "data-ukmeters", 
                 file = "local_edrp_geography_data.xls")
save_object("edrp_elec.csv", bucket = "data-ukmeters", 
                 file = "local_edrp_elec.csv")

# Create Rdata files and save them in S3




# Delete data set in S3 + in instance

