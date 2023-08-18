# CM_simba_interpolation.R
## Created by Caleb C. Vogt, PhD Candidate @ Cornell University

# load packages
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(zoo)

# SET WD, LOAD DATA, FORMAT TIME SERIES
wd <- setwd("Z:/15_CM_Female_Preference_Tracking/SLEAP_CM_Female_Preference_v3/simba_v3/project_folder/csv/input_csv")
# wd <- setwd("Z:/15_CM_Female_Preference_Tracking/SLEAP_CM_Female_Preference_v3/simba_v3/project_folder/csv/input_csv/test")
dir.create("R_proc")
out_fp = "Z:/15_CM_Female_Preference_Tracking/SLEAP_CM_Female_Preference_v3/simba_v3/project_folder/csv/input_csv/R_proc"  #### CHANGE THIS
# out_fp = "Z:/15_CM_Female_Preference_Tracking/SLEAP_CM_Female_Preference_v3/simba_v3/project_folder/csv/input_csv/test/R_proc"  #### CHANGE THIS

## LIST DATA
filenames <- list.files(wd, pattern = "*.csv")

aa = 1
for(aa in 1:length(filenames)){
  
  df <- fread(filenames[aa], header = FALSE)
  df[df == 0 ] <- NA
  df[df == "0.0" ] <- NA
    # replace frame 0 NA with 0 again
  df[4,1] <- 0
 
  headers <- df[1:3,]
  df2 <- df[-c(1:3),]
  df3 <- zoo::na.fill(df2, "extend")
  df4 <- rbind(headers, df3)
  
  write.table(df4, paste0(out_fp, "/", filenames[aa]), row.names = FALSE, col.names = FALSE, sep =",")
}
