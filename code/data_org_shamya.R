# Necessary imports
library(tidyverse) 

df <- read_csv("./datasets/event_master_file_D10_R500_RNG1000_sprint2_shou.csv") # Read in dataset

df <- df %>%
  # Make all stopping subjects NA so code doesn't break when attempting to collapse "Stopping lines"
  mutate(subject_stopping = case_when((event == "Stopping") ~ TRUE, (event != "Stopping") ~ FALSE )) %>%
  mutate(subject = case_when((subject_stopping == TRUE) ~ NA, (subject_stopping == FALSE) ~ subject)) %>%
  select(-subject_stopping) %>%
  mutate(start = timestamp, end = 0) %>% # Make start and ending columns
  select(-timestamp) %>%
  relocate(start, .after = "periodID") %>%
  relocate(end, .after = "start") %>% 
  # Set all ending timestamps of instantaneous events to their starting timestamps
  mutate(end = if_else( (event == "Talking to class: ON-task" | event == "Talking to class: OFF-task" |
                           event == "Talking to student: ON-task" | event == "Talking to student: OFF-task" |
                           event == "Monitoring class: Moving" | event == "Monitoring class: Fixed" |
                           event == "Monitoring student" | event == "Correct attempt" |
                           event == "Incorrect attempt" | event == "Raising hand" | event == "Hint Request" | 
                           event == "Inactive" | event == "Talking to small group: ON-task" | 
                           event == "Talking to small group: OFF-task" | event == "Questioning: On-Task" |
                           event == "Questioning: Off-Task" | event == "Hint request"), start, 0)) %>%
  # Extract locations
  mutate(locationStart = str_locate(content, "\\("), locationEnd = str_locate(content, "\\)")) %>%
  mutate(locationStart = locationStart[,"start"], locationEnd = locationEnd[,"start"]) %>%
  mutate(location =  case_when(event == "Stopping" ~ str_sub(content, locationStart+1, locationEnd-1), 
                               event != "Stopping" ~ "None")  ) %>%
  select(-c(locationStart, locationEnd)) %>%
  arrange(start) %>%
  group_by(location) %>% 
  mutate(group_id = cur_group_id()) %>%
  nest() 

not_stopping <- df$data[[1]] # To join later

df <- df[-1,] # Delete the first row of the dataframe, the non-stopping data

thingymajig <- function(df) {
  first_row_ts <- head(df, n = 1)$start
  last_row_ts <- tail(df, n = 1)$start
  
  df["start"] <- first_row_ts
  df["end"] <- last_row_ts
  
  distinct(df)
}

df <- df %>%
  rowwise() %>%
  mutate(what = list(thingymajig(data))) %>%
  select(location, what) %>%
  unnest(cols = c(what)) %>%
  relocate(location, .after = modality)
  
df <- full_join(df, not_stopping) %>%
  select(-group_id) 
  
df["location"][is.na(df["location"])] <- "None"

df <- df %>%
  ungroup() %>%
  mutate(is_moving = case_when(event == "Moving" ~ TRUE, event != "Moving" ~ FALSE)) %>%
  mutate(group_id = 0) %>%
  arrange(start) 

numbers <- 1:10000
idx <- which(df$is_moving==FALSE) 
idx <- append(idx, 1, after = 0)
difference <- diff(idx) 
df$group_id <- rep(numbers[seq(idx)],c(difference <- diff(idx),nrow(df)-sum(difference)))

non_moving <- df %>% # to join later
  filter(is_moving == FALSE) %>%
  select(-is_moving)

moving <- df %>%
  filter(is_moving == TRUE) %>%
  group_by(group_id) %>%
  nest() %>%
  rowwise() %>%
  mutate(what = list(thingymajig(data))) %>%
  select(what) %>%
  unnest(cols = c(what)) %>%
  select(-is_moving) 

df <- full_join(non_moving, moving) %>%
  select(-group_id)

# Now, we're just worrying about entering, exiting gaming state; entering, exiting idle state; entering, exiting misuse state;
# entering, exiting struggle state

non_states <- df %>% # to join later
  filter(end != 0)

states <- df %>%
  filter(end == 0)

# Split states dataset into gaming, idle, misuse, struggle

gaming <- states %>%
  filter(event == "Entering gaming State" | event ==  "Exiting gaming State")

idle <- states %>%
  filter(event == "Entering idle State" | event ==  "Exiting idle State")

misuse <- states %>%
  filter(event == "Entering misuse State" | event ==   "Exiting misuse State")

struggle <- states %>%
  filter(event == "Entering struggle State" | event ==   "Exiting struggle State")

gaming <- gaming %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

gaming$pairs_gaming <- c(1,1) + rep(seq(0, 275, 1), each = 2)

gaming <- gaming %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_gaming) %>%
  nest() 

pairs_thingymajig <- function(data) {
  data[1, ]$end <- data[2, ]$start
  data <- head(data, n = 1)
}

gaming <- gaming %>%
  rowwise() %>%
  mutate(what = list(pairs_thingymajig(data))) %>%
  select(what) %>%
  unnest(cols = c(what)) %>%
  ungroup() %>% 
  select(-pairs_gaming) %>%
  mutate(event = "Gaming State") 

idle <- idle %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

idle$pairs_idle <- c(1,1) + rep(seq(0,93,1), each = 2)

idle <- idle %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_idle) %>%
  nest() %>%
  rowwise() %>%
  mutate(what = list(pairs_thingymajig(data))) %>%
  select(what) %>%
  unnest(cols = c(what)) %>%
  ungroup() %>% 
  select(-pairs_idle) %>%
  mutate(event = "Idle State") 

misuse <- misuse %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

misuse$pairs_misuse <- c(1,1) + rep(seq(0,167,1), each = 2)

misuse <- misuse %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_misuse) %>%
  nest() %>%
  rowwise() %>%
  mutate(what = list(pairs_thingymajig(data))) %>%
  select(what) %>%
  unnest(cols = c(what)) %>%
  ungroup() %>% 
  select(-pairs_misuse) %>%
  mutate(event = "Misuse State") 

struggle <- struggle %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

struggle$pairs_struggle <- c(1,1) + rep(seq(0,36,1), each = 2)

struggle <- struggle %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_struggle) %>%
  nest() %>%
  rowwise() %>%
  mutate(what = list(pairs_thingymajig(data))) %>%
  select(what) %>%
  unnest(cols = c(what)) %>%
  ungroup() %>% 
  select(-pairs_struggle) %>%
  mutate(event = "Struggle State") 

gaming_idle <- full_join(gaming, idle)
misuse_struggle <- full_join(misuse, struggle)
states <- full_join(gaming_idle, misuse_struggle) 
df <- full_join(non_states, states) 

df["subject"][is.na(df["subject"])] <- "None"
df["content"][is.na(df["content"])] <- "None"

df_locations <- read_csv("./datasets/teacher_position_sprint1_shou (1).csv") 
df_locations <- df_locations[!duplicated(df_locations[ , "time_stamp"]), ] # Remove rows with duplicated timestamps
 # Verify that joining variables are all numeric 
df_locations <- df_locations %>%
  mutate(time_stamp = as.numeric(time_stamp), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  relocate(periodID, .before = "time_stamp") %>%
  relocate(dayID, .before = "periodID") %>%
  arrange(time_stamp) %>%
  mutate(time_stamp = round(time_stamp, digits = 0))
df <- df %>%
  mutate(start = as.numeric(start), end = as.numeric(end), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  arrange(start) %>%
  mutate(start = round(start, digits = 0)) %>%
  mutate(end = round(end, digits = 0))

df <- df %>% 
  left_join(df_locations, by = c("dayID", "periodID", "start" = "time_stamp")) %>%
  arrange(start) %>%
  select(-c(tag19_X, tag20_X, tag19_Y, tag20_Y, tag19_score, tag20_score))  # Deselect irrelevant column

# Pull relevant locations out of chosen_X and chosen Y, concatenate and save them
df <- df %>%
  mutate(imp_loc = str_c(as.character(chosen_X), ", ", as.character(chosen_Y))) %>%
  mutate(silly = if_else(actor == "teacher" & event != "Stopping", TRUE, FALSE))  %>%
  mutate(location_temp = location) %>%
  mutate(location = case_when((silly == TRUE) ~ imp_loc, (silly == FALSE) ~ location_temp)) %>%
  #Deselect irrelevant columns 
  select(-c(chosen_X, chosen_Y, imp_loc, silly, location_temp))

# Note: There are some teacher positions not included in the location dataset
write.csv(df, "~/Desktop/epistemic_analytics/shamya_collab/shamya_collab/code/collapsed_AI_classroom_data.csv", row.names = TRUE)

# Unused experimental code for bug fixes and whatnot 
# 
# Save end timestamps in another dataset, to join later
# end_of_times <- df["end"]
# 
# df <- df %>%
#   select(-end)

# na_df <- df %>%
#   filter(is.na(chosen_X) | is.na(chosen_Y)) %>%
#   select(-c(chosen_X, chosen_Y))
# 
# not_na_df <- df %>%
#   filter(!is.na(chosen_X) & !is.na(chosen_Y))

# relocate(periodID, .before = "time_stamp") %>%
# relocate(dayID, .before = "periodID")# %>%
#mutate(time_stamp2 = time_stamp, .after = "time_stamp")

#df_na_locs <- left_join(na_df, df_locations, by = c("dayID", "periodID", "time_stamp" = "start", "time_stamp2" = "end"))

# Code to impute Monitoring class: Fixed location data with last Stopping row's location
# not_fixed_stopping <- df %>%
#   arrange(start) %>%
#   filter(event != "Monitoring class: Fixed" & event != "Stopping")
# 
# fixed_stopping <- df %>%
#   arrange(start) %>%
#   filter(event == "Monitoring class: Fixed" | event == "Stopping") %>%
#   mutate()
# 
# 
# fixed <- which(fixed_stopping["event"] == "Monitoring class: Fixed")
# 
# for (i in fixed) {
#   fixed_stopping[i, ]$location <- fixed_stopping[i-1, ]$location
# }
# 
# df <- full_join(not_fixed_stopping, fixed_stopping) %>%
#   arrange(start)
