## Created by Caleb C. Vogt, PhD Candidate @ Cornell University
library(tidyverse)
library(dplyr)
library(readr)

setwd('Y:/Data/FieldProject/video_behavior_sleap/2_ephys_rig_low_quality/simba/project_folder/logs')
movement <- read.csv('nose_Movement_log_20230821160005.csv')
entries <- read.csv('nose_ROI_entry_data_20230821155857.csv')
time <- read.csv('nose_ROI_time_data_20230821155857.csv')

## create ROI_analysis_concatenated.csv for each simba project
df1 <- movement %>%
  pivot_wider(names_from = MEASURE, values_from = VALUE) %>%
  select(VIDEO,`Distance (cm)`,`Velocity (cm/s)`) %>% 
  rename(video=VIDEO,distance_cm=`Distance (cm)`,velocity_cm_s=`Velocity (cm/s)`)

df2 <- entries %>%
  group_by(VIDEO) %>%
  summarise(
    shape_1_entries = sum(ENTRIES[SHAPE == 1], na.rm = T),
    shape_2_entries = sum(ENTRIES[SHAPE == 2], na.rm = T)
  ) %>% 
  rename(video=VIDEO)

df3 <- time %>%
  group_by(VIDEO) %>%
  summarise(
    shape_1_time = sum(TIME[SHAPE == 1], na.rm = T),
    shape_2_time = sum(TIME[SHAPE == 2], na.rm = T)
  ) %>% 
  rename(video=VIDEO)

df4 <- df1 %>% 
  inner_join(df2,by="video") %>%
  inner_join(df3,by="video")

# Write the reshaped data to a CSV file
write.csv(df4,'ROI_analysis_concatenated.csv',row.names=F)
