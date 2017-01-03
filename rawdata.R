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
DT <- DT %>% rename(IDMETER = ANON_ID) 

######### GEO DATA and META DATA #########
metaDT <- tbl_df(read.xls(file.path(data.folder, "edrp_metadata.xlsx"), nrows = 14319, skip = 1)) %>%
  mutate_each(funs(ymd_hm), firstAdvance) %>% 
  mutate_each(funs(ymd_hm), lastAdvance) %>%
  rename(IDMETER = Hhold_ID) 

geodemoDT  <- tbl_df(read.xls(file.path(data.folder, "edrp_geography_data.xlsx"))) %>% 
  rename(IDMETER = anonID) %>% # replace "--" entries in NUTS1 by "---" and in NUTS4 by "-------"
  mutate(NUTS1 = ifelse(NUTS1 == "--", "---", as.character(NUTS1))) %>%
  mutate(NUTS4 = ifelse(NUTS4 == "--" | NUTS4 == "", paste(NUTS1, "----", sep = ""), as.character(NUTS4))) %>%
  mutate(NUTS2 = substr(NUTS4, 1, 4)) %>%
  mutate(NUTS3 = substr(NUTS4, 1, 5))

geodemoDT <- geodemoDT %>% 
  mutate(ACORN_Category = ifelse(ACORN_Category == "" | is.na(ACORN_Category), "-", ACORN_Category)) %>%
  mutate(ACORN_Group = as.numeric(ifelse(ACORN_Group == "" | is.na(ACORN_Group), "99", ACORN_Group))) %>%
  mutate(ACORN_Type = as.numeric(ifelse(ACORN_Type == "" | is.na(ACORN_Type), "99", ACORN_Type))) %>% 
  mutate(DEMO1 = paste("D", ACORN_Category, sep = ""), 
         DEMO2 = paste(DEMO1, sprintf("%02d", ACORN_Group), sep = ""), 
         DEMO3 = paste(DEMO2, sprintf("%02d", ACORN_Type), sep = ""))

# infoDT <- inner_join(metaDT, geodemoDT, by = "IDMETER") # 14319 meters (not 14621 meters = 16249 - 1628)
infoDT <- inner_join(metaDT, geodemoDT, by = "IDMETER")


allmeters <- infoDT %>% dplyr::select(IDMETER) %>% .$IDMETER

print("Making info file")
# Create the info file
save(file = file.path(work.folder, "info.Rdata") , list = c("infoDT", "allmeters"))

stop("done")


# Delete data set in S3 + in instance

