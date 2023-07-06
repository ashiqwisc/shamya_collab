# Set working directory as the shamya_collab folder on your local machine after pulling repo 

df <- read.csv("./datasets/collapsed_AI_classroom_data.csv")

# TODO: Convert screenalignment into binary column
# TODO: Join learning gain data. "students.csv" matches the animal_id to anon "Stu" id. Learning gain data can be found at 
# "meta-data-aied.csv" Note: 26 students missing conceptual knowledge pre, post and prior knowledge pre. 32 students (including the 26)
# missing prior knowledge post. This count can be found at "metadata_student_counts.csv"
# TODO: Calculate learning rates 
# TODO: Split students by learning gain/learning rate into high vs. low