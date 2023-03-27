library(readr)
df <- read_csv("./datasets/collapsed_AI_classroom_data.csv")
colnames(df)
unique(df$event)
df$distilled_event <- ""
df$distilled_event[df$event == "Moving" | df$event == "Monitoring class: Moving" ] <- "Moving" 
df$distilled_event[df$event == "Stopping"] <- "Stopping"
df$distilled_event[df$event == "Talking to class: ON-task" |
                     df$event == "Talking to student: ON-task" | 
                     df$event == "Talking to student: OFF-task" |
                     df$event == "Talking to class: ON-task" |
                     df$event == "Talking to small group: ON-task" |
                     df$event == "Talking to small group: OFF-task"|
                     df$event == "Talking to class: OFF-task"] <- "Talking"
df$distilled_event[df$event == "Questioning: Off-Task"|
                     df$event == "Questioning: On-Task"]  <- "Questioning"
df$distilled_event[df$event == "Monitoring student" | df$event == "Monitoring class: Fixed"] <- "Monitoring"                      
df$distilled_event[df$event == "Inactive"] <- "Inactive"  
df$distilled_event[df$event == "Incorrect attempt"] <- "Incorrect_attempt"    
df$distilled_event[df$event == "Correct attempt"] <- "Correct_attempt"
df$distilled_event[df$event == "Raising hand"   ] <- "Raising_hand"   
df$distilled_event[df$event == "Hint request"] <- "Hint_request"
df$distilled_event[df$event == "Gaming State"] <- "Gaming_State"
df$distilled_event[df$event == "Misuse State"] <- "Misuse_State"
df$distilled_event[df$event == "Idle State"] <- "Idle_State"
df$distilled_event[df$event == "Struggle State"] <- "Struggle_State"
unique(df$distilled_event)  
View(df)
# 12 codes useful
# Write result to csv
write.csv(df, "./datasets/collapsed_AI_classroom_data.csv", row.names = FALSE)

