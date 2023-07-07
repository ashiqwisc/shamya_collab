# Set working directory as the shamya_collab folder on your local machine after pulling repo 

library(tidyverse)
library(lme4)

d <- read_csv("./datasets/collapsed_AI_classroom_data.csv")

# Learning gain
d_learn <- read_csv(here::here('datasets', 'meta-data-aied.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(
    ck_gain = (ck - ck_pre) / (16 - ck_pre),
    pk_gain = (pk - pk_pre) / (5 - pk_pre),
    ck_100 = ck/16, ck_pre_100 = ck_pre/16,
    pk_100 = pk/5, pk_pre_100 = pk_pre/5,
    ck_gain_c = ifelse(ck>ck_pre, (ck-ck_pre)/(16-ck_pre), ifelse(ck<ck_pre, (ck-ck_pre)/ck_pre, 0)),
    pk_gain_c = ifelse(pk>pk_pre, (pk-pk_pre)/(5-pk_pre), ifelse(pk<pk_pre, (pk-pk_pre)/pk_pre, 0))
  ) %>% 
  mutate(
    pk_gain = ifelse(is.infinite(pk_gain), 0, pk_gain),
    pk_gain_c = ifelse(is.infinite(pk_gain_c), 0, pk_gain_c),
    ck_gain = ifelse(is.infinite(ck_gain), 0, ck_gain),
    ck_gain_c = ifelse(is.infinite(ck_gain_c), 0, ck_gain_c)
  ) %>% 
  mutate(
    high_ck_pre = ifelse(ck_pre > median(ck_pre, na.rm=TRUE), 1, 0),
    high_pk_pre = ifelse(pk_pre > median(pk_pre, na.rm=TRUE), 1, 0),
    high_pk_gain = ifelse(pk_gain > median(pk_gain, na.rm=TRUE), 1, 0),
    high_ck_gain = ifelse(ck_gain > median(ck_gain, na.rm=TRUE), 1, 0),
    high_pk_gain_c = ifelse(pk_gain_c  > median(pk_gain_c, na.rm=TRUE), 1, 0),
    high_ck_gain_c = ifelse(ck_gain_c > median(ck_gain_c, na.rm=TRUE), 1, 0)
  )

ref <- read_csv(here::here('datasets', 'students.csv')) %>% 
  select(students, animal_id)

join_this <- d_learn %>% 
  select(anon_student_id, pk_100, ck_100, pk_gain, ck_gain, pk_gain_c, ck_gain_c, matches('high_')) %>% 
  left_join(ref, by = c('anon_student_id'='animal_id')) %>% 
  filter(!is.na(students)) %>% 
  select(-anon_student_id) %>% 
  select(subject=students, everything())


# Split screenalignment into binary column. 1 if cosine similarity is >= 0.5, else 0
df <- read.csv("./datasets/collapsed_AI_classroom_data.csv")
df_out <- df %>%
  mutate(
    screenalignment_binary = ifelse(teacher_screenalignment >= 0.5, 1, 0)
  ) %>% 
  left_join(join_this, by = 'subject')

# Learning rates

d <- read_delim(here::here('datasets', 'tutor_log.tsv'), delim ='\t') %>% 
  janitor::clean_names() %>% 
  arrange(anon_student_id, problem_name, step_name, desc(time)) %>% 
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>% 
  mutate(outcome_bin = case_when(
    outcome == "CORRECT" ~ 1,
    outcome == "INCORRECT" ~ 0,
    outcome == "HINT" ~ 0
  )) %>% 
  filter(!is.na(outcome_bin)) %>% 
  arrange(anon_student_id, time)

combine_kc_default <- function(row) {
  kc_columns <- grep("^kc_default", names(row), value = TRUE)
  kc_values <- row[kc_columns]
  kc_values <- kc_values[!is.na(kc_values)] %>% as.character()
  return(kc_values)
}

d_afm <- d %>% 
  filter(attempt_at_step == 1) %>% 
  arrange(anon_student_id, problem_name, step_name, desc(time)) %>% 
  distinct(anon_student_id, problem_name, step_name, .keep_all = TRUE) %>% 
  mutate(outcome_bin = case_when(
    outcome == "CORRECT" ~ 1,
    outcome == "INCORRECT" ~ 0,
    outcome == "HINT" ~ 0
  )) %>% 
  filter(!is.na(outcome_bin)) %>% 
  mutate(kcs = apply(., 1, combine_kc_default)) %>% 
  mutate(kc_length = kcs %>% map_int(length)) %>% 
  filter(kc_length > 0) %>% 
  select(anon_student_id, kcs, time, outcome_bin) %>% 
  unchop(kcs) %>% 
  group_by(anon_student_id, kcs) %>% 
  arrange(time) %>% 
  reframe(time = time, n_opportunity = 1:n(), outcome_bin = outcome_bin) %>% 
  select(-time)

m_tutor <- glmer(outcome_bin ~ kcs*n_opportunity + (1 + n_opportunity | anon_student_id), 
                 d_afm, family = 'binomial', nAGQ=0, verbose=2)

#saveRDS(m_tutor, 'm_tutor.rds')

join_this <- ranef(m_tutor)$anon_student_id %>% 
  rownames_to_column('anon_student_id') %>% 
  janitor::clean_names() %>% 
  rename(learning_rate = n_opportunity) %>% 
  tibble() %>% 
  mutate(
    high_learning_rate = ifelse(learning_rate > median(learning_rate, na.rm=TRUE), 1, 0),
    high_intercept = ifelse(intercept > median(intercept, na.rm=TRUE), 1, 0)
  )

df_out_final <- df_out %>% 
  left_join(join_this, by = c('subject' = 'anon_student_id'))

write_csv(df_out_final, 'final-sample-cb-lak24.csv')
