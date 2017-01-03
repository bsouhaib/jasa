rm(list = ls())

library(data.table)
library(dplyr)
library(lubridate)
library(gdata)

system("aws s3 ls  s3://code-old-ukmeters")
