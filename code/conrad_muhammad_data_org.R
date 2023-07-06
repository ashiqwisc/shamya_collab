# Set working directory as the shamya_collab folder on your local machine after pulling repo 

df <- read.csv("./datasets/collapsed_AI_classroom_data.csv")

# TODO: Convert screenalignment into binary column
# Muhammad: This is my first attempt at doing this. Please let me know if this doesn't work well 

df <- df %>%
  mutate(
    screenalignment_binary = ifelse(teacher_screenalignment >= 0.5, 1, 0)
  )

# TODO: Join learning gain data. "students.csv" matches the animal_id to anon "Stu" id. Learning gain data can be found at 
# "meta-data-aied.csv" Note: 26 students missing conceptual knowledge pre, post and procedural knowledge pre. 32 students 
# (including the 26) missing procedural knowledge post. This count can be found at "metadata_student_counts.csv"
# TODO: Calculate learning rates 
# TODO: Split students by learning gain/learning rates into high vs. low