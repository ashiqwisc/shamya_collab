# install.packages("tibbletime")
# Necessary imports
library(tidyverse) 
library(tibbletime)
# Set working directory (local machine)
setwd("~/Desktop/epistemic_analytics/shamya_collab/shamya_collab")

# Read in dataset
df <- read_csv("./datasets/event_master_file_D10_R500_RNG1000_sprint2_shou.csv") 

# Preliminary data tidying and organization before collapsing stopping rows
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
                               event != "Stopping" ~ NA)  ) %>%
  select(-c(locationStart, locationEnd)) %>%
  arrange(start) %>%
  group_by(location) %>% 
  mutate(group_id = cur_group_id()) %>%
  nest() 

# A dataframe of items to join later
not_stopping <- df$data[[1]] 
# Delete the first row of our data; it contains all rows that aren't stopping. Now, we are just left with stopping data
df <- df[-1,] 

# Define a function to impute start and end given a dataframe using the timestamp of the first item in the group and 
# the last item in the group
time_collapser <- function(df) {
  first_row_ts <- head(df, n = 1)$start
  last_row_ts <- tail(df, n = 1)$start
  
  df["start"] <- first_row_ts
  df["end"] <- last_row_ts
  
  distinct(df)
}

# Now, our dataframe is a list of dataframes. Use rowwise to pass each dataframe to time collapser. Then, unnest the new 
# dataframes, rejoin them to the old dataset, and prepare the data to collapse moving rows 
df <- df %>%
  rowwise() %>%
  mutate(new = list(time_collapser(data))) %>%
  select(location, new) %>%
  unnest(cols = c(new)) %>%
  relocate(location, .after = modality) %>%
  full_join(not_stopping) %>%  # Join the new stopping dataset with the non-stopping dataset
  select(-group_id) %>%
  ungroup() %>%
  mutate(is_moving = case_when(event == "Moving" ~ TRUE, event != "Moving" ~ FALSE)) %>%
  mutate(group_id = 0) %>%
  arrange(start) 

# It is particularly difficult to group the moving data now. We could group all moving data, but we want to group by each
# continuous, uninterrupted sequence of moving rows. And, unlike the stopping rows, we don't have locations to base our grouping
# on. As such, we will find each index in which the event isn't moving  and store it as the vector "idx". Then, we'll take 
# the next number from our sequence "numbers"  (starting at 0), use that as the group id of our next uninterrupted sequence 
# of moving rows, and then have the rows from that index to the previous index in the "idx" vector by placing a sequence 
# of the group id to the corresponding number of rows calculated in "difference". 
numbers <- 1:10000 # An arbitrary sequence of numbers, enough to encapsulate all the groups of moving data
idx <- which(df$is_moving == FALSE) # Store which indices aren't moving
# Prepend 1 to the vector of indices, since there is no non-moving event preceding it and the dataframe starts with moving
idx <- append(idx, 1, after = 0)
difference <- diff(idx) # Store a vector of the differences between indices; these will be the number of rows for each group
df$group_id <- rep(numbers[seq(idx)], c(difference <- diff(idx), nrow(df) - sum(difference))) # Place group IDs

# Save a dataset of non-moving rows to join later
non_moving <- df %>% 
  filter(is_moving == FALSE) %>%
  select(-is_moving)

# Get all moving rows, nest them into datasets depending on their groups, perform a rowwise time collapse operation on them, 
# and deselect the now-irrelevant column
moving <- df %>%
  filter(is_moving == TRUE) %>%
  group_by(group_id) %>%
  nest() %>%
  rowwise() %>%
  mutate(new = list(time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  select(-is_moving) 

# Join the non-moving and moving rows back together again
df <- full_join(non_moving, moving) %>%
  select(-group_id)

# Now, we're just worrying about entering, exiting gaming state; entering, exiting idle state; entering, exiting misuse state;
# entering, exiting struggle state.

# Save a dataset of rows that don't include relevant "state" events
non_states <- df %>%
  filter(end != 0)

# Save a dataset of rows that include "state" events
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

# Group gaming sequences. This groups the pairs into groups that are unique to each day, period, and actor. 
gaming <- gaming %>%
  group_by(dayID, periodID, actor) %>%
  mutate(group_id = cur_group_id()) %>%
  arrange(group_id)

# The above groups aren't good enough, they don't represent each pair of entering and exiting the gaming state. 
# Write a mathematical lambda function to generate relevant pair indices
gaming$pairs_gaming <- c(1,1) + rep(seq(0, 275, 1), each = 2)

# Nest gaming
gaming <- gaming %>%
  select(-group_id) %>%
  ungroup() %>%
  group_by(pairs_gaming) %>%
  nest() 

# Define a function like time_collapser, except for pairs rather than whole datasets
pairs_time_collapser <- function(data) {
  data[1, ]$end <- data[2, ]$start
  data <- head(data, n = 1)
}

# Perform the time collapsing rowwise on each nested dataframe, unnest and ungroup, deselect the pair ID column, and rename
# the event to something clearer
gaming <- gaming %>%
  rowwise() %>%
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_gaming) %>%
  mutate(event = "Gaming State") 

# Repeat this process for the other 3 states

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
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
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
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
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
  mutate(new = list(pairs_time_collapser(data))) %>%
  select(new) %>%
  unnest(cols = c(new)) %>%
  ungroup() %>% 
  select(-pairs_struggle) %>%
  mutate(event = "Struggle State") 

# Rejoin all partitioned dataframes
gaming_idle <- full_join(gaming, idle)
misuse_struggle <- full_join(misuse, struggle)
states <- full_join(gaming_idle, misuse_struggle) 
df <- full_join(non_states, states) 

# Read in the locations dataset 
df_locations <- read_csv("./datasets/teacher_position_sprint1_shou (1).csv") 
# Remove rows with duplicated timestamps
df_locations <- df_locations[!duplicated(df_locations[ , "time_stamp"]), ] 

# Relocate relevant columns to join with. Verify that important variables are numeric and rounded. 
df_locations <- df_locations %>%
  mutate(time_stamp = as.numeric(time_stamp), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  relocate(periodID, .before = "time_stamp") %>%
  relocate(dayID, .before = "periodID") %>%
  arrange(time_stamp) %>%
  mutate(time_stamp = round(time_stamp, digits = 0))

# Verify that important variables are numeric and rounded. Join locations, deselect irrelevant columns, pull relevant locations
# out of relevant columns and concatenate them in a readable format, then attach them if the rows have a teacher actor and 
# aren't stopping events
df <- df %>%
  mutate(start = as.numeric(start), end = as.numeric(end), dayID = as.numeric(dayID), periodID = as.numeric(periodID)) %>%
  mutate(start = round(start, digits = 0)) %>%
  mutate(end = round(end, digits = 0)) %>% 
  left_join(df_locations, by = c("dayID", "periodID", "start" = "time_stamp")) %>%
  select(-c(tag19_X, tag20_X, tag19_Y, tag20_Y, tag19_score, tag20_score))  %>%
  mutate(imp_loc = str_c(as.character(chosen_X), ", ", as.character(chosen_Y))) %>%
  mutate(silly = if_else(actor == "teacher" & event != "Stopping", TRUE, FALSE))  %>%
  mutate(location_temp = location) %>%
  mutate(location = case_when((silly == TRUE) ~ imp_loc, (silly == FALSE) ~ location_temp)) %>%
  select(-c(chosen_X, chosen_Y, imp_loc, silly, location_temp)) %>%
  ungroup() %>%
  arrange(dayID, periodID, start)

# Impute teacher location based on the mean of the adjacent time points (the location of the teacher right before, the location 
# of the teacher right after). Instances when location is NA twice in a row occur at the same timestamp; thus, we can have the same 
# imputation for both instances 

# Define a function to impute teacher locations
teacher_imputer <- function(df) {
  
  # Make a copy of the dataframe 
  df_copy <- data.frame(df)
  # Get all indices in which the teacher locations are NA 
  idx_list <- which(is.na(df_copy[,"location"]))
  
  # Save the indices that have NA locations before and after; these are special cases. Thus, look for consecutive values
  # and save them.
  # Iterate over the list, skipping the last element and the first element (these are confirmed non-consecutive)
  special_idx <- c()
  for (i in 2:(length(idx_list)-1)) {
    if ((idx_list[i-1] == idx_list[i] - 1) | (idx_list[i+1] == idx_list[i] + 1)) {
      special_idx <- append(special_idx, idx_list[i])
    }
  }
  
  # Remove the special indices from the idx_list 
  idx_list <- idx_list[-which(idx_list %in% special_idx)]
  
  # First, impute the rows from idx_list 
  for (j in idx_list) {
    df_copy$V1[j] <- (df_copy$V1[j-1] + df_copy$V1[j+1])/2
    df_copy$V2[j] <- (df_copy$V1[j-1] + df_copy$V2[j+1])/2
    df_copy$location[j] <- str_c(as.character(df_copy$V1[j]), ", ", as.character(df_copy$V2[j]))
  }
  
  # Then, impute the rows from special_idx
    # Split special_idx into consecutive sequences. Data wasn't too big, hardcoded this for time's sake
    sequence_1 <- list(269, 270)
    sequence_2 <- list(829, 830, 831, 832, 833, 834)
    sequence_3 <- list(988, 989)
    sequence_4 <- list(1465, 1466, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475)
    sequence_5 <- list(1711, 1712)
    sequence_6 <- list(1805, 1806)
    sequence_7 <- list(2144, 2145)
    sequence_8 <- list(2154, 2155)
    sequence_9 <- list(2240, 2241)
    sequence_10 <- list(2275, 2276)
    sequence_11 <- list(2765, 2766, 2767)
    list_sequences <- list(sequence_1, sequence_2, sequence_3, 
                          sequence_4, sequence_5, sequence_6,
                          sequence_7, sequence_8, sequence_9, sequence_10, sequence_11)
    # For all sequences
    for (x in list_sequences) {
      # Get first idx in the sequence
      first_idx <- x[[1]]
      # Get last idx in the sequence
      last_idx <- x[[length(x)]]
      # Find previous row in teacher dataframe that isn't NA
      previous_x <- df_copy$V1[first_idx - 1]
      previous_y <- df_copy$V2[first_idx - 1]
      # Find next row that isn't NA 
      next_x <-  df_copy$V1[last_idx + 1]
      next_y <- df_copy$V2[last_idx + 1]
      # Take the average of those two locations, x and y
      x_result <- (previous_x + next_x) / 2
      y_result <- (previous_y + next_y) / 2
      location_result <- str_c(as.character(x_result), ", ", as.character(y_result))
      # Impute those locations for everything in that sequence, x and y 
      for (i in x) {
        df_copy$location[i] <- location_result
      }
    }
      
  # Return new dataframe
  return(df_copy)
}

# To join later 
df_not_teacher <- df %>%
  filter(actor != "teacher")

# Split teacher df away from old df
df_teacher <- df %>%
  filter(actor == "teacher") 

# Before calculating the means and using the teacher_imputer, split location into its x and y coords 
df_teacher[, 11:12] <- str_split_fixed(df_teacher$location, ", ", n = 2)

# Make sure V1 and V2 split are numeric
df_teacher <- df_teacher %>%
  mutate(V1 = as.numeric(V1), V2 = as.numeric(V2)) 

# Impute values and deselect irrelevant columns
df_teacher <- teacher_imputer(df_teacher) %>%
  select(-c(V1, V2))

# Merge teacher and non_teacher data
df <- full_join(df_not_teacher, df_teacher) %>%
  arrange(dayID, periodID, start)

# Read in CSVs containing student locations and student metdata; use only the learning markers for the student metadata
student_locations <- read_csv("./datasets/student_position_sprint1_shou.csv") %>%
  select(-SeatNum) 
student_learning <- read_csv("./datasets/meta-data-aied.csv") %>%
  select(anon_student_id, ck_pre, ck_lg, pk_pre, pk_lg)

# Join the student locations and student metadata to see which student IDs have missing data in either dataset
check_df <- full_join(student_locations, student_learning, by = c("actual_user_id" = "anon_student_id"))

# Make a dataframe with all the distinct students in the original dataframe 
students_df <- df %>%
  select(c(actor, subject)) %>%
  filter(actor != "teacher" & actor != "tutor") %>%
  select(-subject) %>%
  distinct() %>%
  mutate(bool = str_detect(actor, "Stu")) %>%
  filter(bool) %>%
  select(-bool) %>%
  rename(students = actor)

# Strip check_df of unnecessary columns
check_df <- check_df %>%
  select(-c(actual_user_id))

# Join check_df with students_df
students_df <- left_join(students_df, check_df, by = c("students" = "anon_user_id"), multiple ="all") %>%
  distinct() %>%
  arrange(students, `DayID`, `PeriodID`) %>%
  mutate(location_dataset = case_when((!is.na(X) & !is.na(Y)) ~ TRUE, (is.na(X) | is.na(Y)) ~ FALSE)) %>%
  mutate(metadata_dataset = case_when((!is.na(ck_pre) & !is.na(ck_lg) & !is.na(pk_pre) & !is.na(pk_lg)) ~ TRUE, 
                                      (is.na(ck_pre) | is.na(ck_lg) | is.na(pk_pre) | is.na(pk_lg)) ~ FALSE)) %>%
  select(students, location_dataset, metadata_dataset) %>%
  distinct()

# Output csv, arranged by dayID, periodID, and timestamp.
write.csv(df, "./datasets/collapsed_AI_classroom_data.csv", row.names = FALSE)

# Run other R script, that depends on above written CSV
source("./code/distill_feature.R")

# Output csv for students_df, a dataframe that checks whether or not individual students in df are in the student location df
# and/or the student metadata df
write.csv(students_df, "./datasets/students.csv")

# Unused experimental code for bug fixes and notes
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

# df["location"][is.na(df["location"])] <- "None"

#df["subject"][is.na(df["subject"])] <- "None"
#df["content"][is.na(df["content"])] <- "None"

# Note: There are some teacher positions not included in the location dataset.
