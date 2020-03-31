library(data.table) # used for reading in csv files
library(tidyverse)
library(lubridate)
library(scales)

# https://rud.is/b/2019/06/30/make-refreshing-segmented-column-charts-with-ggchicklet/
library(ggplot2)
library(hrbrthemes) # used for scale_y_percent

# Summary stats
library(janitor)

# Latex output
library(knitr)
library(kableExtra)

all_codings_anon <- readRDS("all_codings_anon.rds")
setDT(all_codings_anon)

#
# Define colours for the plots
#

fillplus <- c(
  # non-interactive
  "AD" = "grey",
  "LT" = "peachpuff",
  
  # vicarious interactive
  "LQ" = "#c7e9c0", # mid green
  "LR" = "#edf8e9", # pale green
  "SQ" = "#cbc9e2", # pale purple
  "SR" = "#bdd7e7", # pale blue
  
  # interactive
  "CQ" = "#238b45", # dark green
  "ST" = "#41b6c4", # mid blue / teal
  "SD" = "#54278f", # dark purple
  "FB" = "#fd8d3c" # orange
)
interactivity_levels <- c(
  "NON" = "peachpuff",
  "VIC" = "#a1d99b",
  "INT" = "#225ea8",
  "NA" = "white"
)

#
# Data summary table (Table 2 in the paper)
#

tab_number_of_lecturers = all_codings_anon %>% 
  group_by(discipline, lecturer_anonid) %>% 
  tally() %>% 
  group_by(discipline) %>% 
  summarise(
    num_lecturers = n()
  ) %>%
  adorn_totals("row") 

tab_coding_summary = all_codings_anon %>% 
  filter(!discipline %in% c("BIO", "CHE")) %>% 
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  tally() %>% 
  group_by(discipline) %>% 
  summarise(
    count = n(),
    num_lectures = sum(n)
  ) %>%
  adorn_totals("row") %>% 
  left_join(tab_number_of_lecturers) %>% 
  select(discipline, num_lecturers, count, num_lectures)


tab_coding_summary

tab_coding_summary %>% 
  kable("latex", booktabs = T)




# Produce a table where each row is an assigned code, with a given duration
codes_with_durations = all_codings_anon %>% 
  unnest(codes_with_duration) %>% 
  mutate(
    # Define groupings of codes by interactivity level (as in Wood et al.)
    code_grp = as.factor(case_when(
      code %in% c("AD", "LT")             ~ "NON",
      code %in% c("LQ", "SR", "SQ", "LR") ~ "VIC",
      code %in% c("CQ", "ST", "SD", "FB") ~ "INT",
      TRUE                                ~ "NA"
    )),
    # Put the factors into a sensible order
    code = fct_relevel(code, "AD", "LT", "LQ", "LR", "SQ", "SR", "CQ", "ST", "SD", "FB"),
    code_grp = fct_relevel(code_grp, "NON", "VIC", "INT"),
    duration_int = ifelse(code_grp %in% c("INT"), duration, 0)
    #    course = fct_reorder(course, course, .desc = TRUE),
    #    course = fct_reorder(course, discipline, .desc = TRUE)
  ) %>% 
  select(-raw_coding, -coding, -codes_by_second)

write_csv(codes_with_durations, "anon_codes_with_durations.csv")

#
# Full detailed dataset
#
codes_with_durations %>%
  filter(!is.na(duration)) %>% # remove rows corresponding to "END", which have null duration
  mutate(
    title = simple_title,
    title = lecturer_and_session,
    # order by date
    title = fct_reorder(title, lecture_date, .desc = TRUE)
  ) %>% 
  ggplot(aes(title, duration, group = time, fill = code)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(rows = vars(discipline, course_anonid),
             scales = "free",
             space = "free",
             shrink = TRUE) +
  scale_y_time(breaks = 60*5*(0:12),
               labels = 5*(0:12),
               limits = c(0, 60*60)) +
  scale_fill_manual("FILL+ code", values = fillplus) +
  coord_flip() +
  labs(x = NULL, y = "Time") +
  theme_minimal(base_size = 11.5) +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        #        panel.spacing=grid::unit(2, "lines"),
        strip.text=element_text(hjust=0, size=12))
ggsave("Paper1_FILLplus_timeline_allcourses.pdf",width=30,height=60,units="cm",dpi=300)

lecture_numbers = codes_with_durations %>%
  select(discipline, course_anonid, lecturer_anonid, lecture_id, lecture_date) %>% 
  distinct() %>% 
  arrange(discipline, course_anonid, lecturer_anonid, lecture_date) %>% 
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  mutate(lecture_number = row_number()) %>% 
  ungroup()

codes_with_durations %>% 
  left_join(lecture_numbers %>% select(lecture_id, lecture_number)) %>% 
  mutate(
    title = simple_title,
    title = lecture_number
    # order by date
    #title = fct_reorder(title, title, .desc = TRUE)
  ) %>% 
  ggplot(aes(title, duration, group = time, fill = code)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(rows = vars(discipline, course_anonid, lecturer_anonid),
             scales = "free",
             space = "free",
             shrink = TRUE) +
  scale_y_time(breaks = 60*5*(0:12),
               labels = 5*(0:12),
               limits = c(0, 60*60)) +
  scale_fill_manual("FILL+ code", values = fillplus) +
  coord_flip() +
  labs(x = NULL, y = "Time") +
  theme_minimal(base_size = 11.5) +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        #        panel.spacing=grid::unit(2, "lines"),
        strip.text=element_text(hjust=0, size=12))
ggsave("Paper1_FILLplus_timeline_allcourses_alt.pdf",width=30,height=60,units="cm",dpi=300)

plot_all_timelines = function(discipline_to_plot, width_cm=30, height_cm=60) {
  data_to_plot = codes_with_durations %>% 
    filter(!is.na(duration)) %>% # remove rows corresponding to "END", which have null duration
    filter(discipline == discipline_to_plot) %>% 
    left_join(lecture_numbers %>% select(lecture_id, lecture_number)) %>% 
    mutate(
      title = simple_title,
      title = as.factor(lecture_number),
      # order by date
      title = fct_reorder(title, lecture_number, .desc = TRUE)
    )
  
  data_to_plot %>% 
    ggplot(aes(title, duration, group = time, fill = code)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    facet_grid(rows = vars(course_anonid, lecturer_anonid),
               scales = "free",
               space = "free",
               shrink = TRUE,
               switch = "y") +
    scale_y_time(breaks = 60*5*(0:12),
                 labels = 5*(0:12),
                 limits = c(0, 60*60)) +
    scale_fill_manual("FILL+ code", values = fillplus) +
    coord_flip() +
    #scale_x_reverse() +
    labs(x = NULL, y = "Time") +
    theme_minimal(base_size = 11.5) +
    theme(strip.text.y = element_text(angle = 180),
          strip.placement = "outside",
          panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(),
          #        panel.spacing=grid::unit(2, "lines"),
          strip.text=element_text(hjust=0, size=12))
  ggsave(paste0("Paper1_FILLplus_timeline_",discipline_to_plot,".pdf"),width=width_cm,height=height_cm,units="cm",dpi=300)
}

plot_all_timelines("MATH", height_cm = 40)
plot_all_timelines("PHYS", height_cm = 20)
plot_all_timelines("VET", height_cm = 17)


#
# Summaries for lecturer/course combinations
#
# Levels of interactivity
interactivity_proportions_by_lecturer = codes_with_durations %>% 
  group_by(discipline, course_anonid, lecturer_anonid, code_grp) %>% 
  summarise(
    mins = sum(duration)
  ) %>% 
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  mutate(
    sum = sum(mins, na.rm = TRUE),
    prop = mins/sum,
    # prepare to order by the proportio of NON
    prop_NON = if_else(code_grp == "NON", prop, 0),
    prop_NON = max(prop_NON)
  ) %>% 
  ungroup() %>% 
  filter(!code_grp == "NA")

interactivity_proportions_by_lecturer %>%
  mutate(
    course_lecturer = paste(course_anonid, lecturer_anonid),
    course_lecturer = fct_reorder(course_lecturer, prop_NON, .desc = FALSE)
  ) %>%
  ggplot(aes(course_lecturer, prop, fill = code_grp)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(rows = vars(discipline),
             scales = "free",
             space = "free",
             shrink = TRUE) +
  scale_y_percent(breaks = 0.2*c(0:5)) +
  scale_fill_manual("Interactivity", values = interactivity_levels) +
  coord_flip() +
  labs(x = NULL, y = "Proportion of time") +
  #  theme_ipsum_rc() +
  theme_minimal(base_size = 11.5) +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        #        panel.spacing=grid::unit(2, "lines"),
        strip.text=element_text(hjust=0, size=12))
ggsave("Paper1_FILLplus_interactivity_proportions.pdf",width=20,height=14,units="cm",dpi=300)



# Use of each code
code_proportions_by_lecturer = codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  group_by(discipline, course_anonid, lecturer_anonid, code) %>% 
  summarise(
    mins = sum(duration)
  ) %>% 
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  mutate(
    sum = sum(mins, na.rm = TRUE),
    prop = mins/sum
  ) %>% 
  ungroup() %>% 
  # add on their NON proportion so we can sort by it (and therefore keep the same order as the previous plot)
  left_join(interactivity_proportions_by_lecturer %>%
              select(course_anonid, lecturer_anonid, prop_NON) %>%
              distinct(),
            by = c("course_anonid", "lecturer_anonid"))

code_proportions_by_lecturer %>%
  mutate(
    course_lecturer = paste(course_anonid, lecturer_anonid),
    course_lecturer = fct_reorder(course_lecturer, prop_NON, .desc = FALSE)
  ) %>%
  ggplot(aes(course_lecturer, prop, fill = code)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(rows = vars(discipline),
             scales = "free",
             space = "free",
             shrink = TRUE) +
  scale_y_percent(breaks = 0.2*c(0:5)) +
  scale_fill_manual("FILL+ code", values = fillplus) +
  coord_flip() +
  labs(x = NULL, y = "Proportion of time") +
  #  theme_ipsum_rc() +
  theme_minimal(base_size = 11.5) +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        #        panel.spacing=grid::unit(2, "lines"),
        strip.text=element_text(hjust=0, size=12))
ggsave("Paper1_FILLplus_code_proportions.pdf",width=20,height=14,units="cm",dpi=300)


# Analysis of lecturer questions
codes_with_durations %>% 
  filter(code == "LQ") %>% 
  group_by(course_anonid, lecturer_anonid, session_number) %>% 
  summarise(
    num_qs = n()
  ) %>% 
  ungroup() %>% 
  summarise(
    min_qs = min(num_qs),
    mean_qs = mean(num_qs),
    max_qs = max(num_qs),
    median_qs = median(num_qs)
  )
codes_with_durations %>% 
  group_by(discipline, course_anonid, lecturer_anonid, session_number) %>% 
  summarise(
    num_qs = sum(code == "LQ", na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # add the mean for each course-lecturer combo
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  mutate(
    mean_qs = mean(num_qs, na.rm = TRUE),
    se_qs = sd(num_qs, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup() %>% 
  mutate(
    #position = parse_number(course_anonid)*1 + 100*num_qs,
    course_num = parse_number(course_anonid),
    course_and_lecturer = paste(course_anonid, lecturer_anonid),
    #course_and_lecturer = paste(course_acronym, course_anonid, lecturer),
    course_and_lecturer = fct_reorder(course_and_lecturer, mean_qs, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = num_qs, fill = course_anonid)) +
  facet_grid(cols = vars(discipline),
            scales = "free",
            space = "free_x",
             shrink = TRUE) +
  #geom_violin(fill = fillplus["LQ"]) +
  #geom_violin() +
  #ylim(-1, 50) +
  #geom_boxplot(alpha = 0.5, width = 0.3, colour = "#999999") +
  geom_point(aes(color = course_num),
             position = position_jitter(width = 0.1),
             alpha = 0.8) +
  geom_errorbar(aes(ymin=(mean_qs-se_qs), ymax=(mean_qs+se_qs)), width=.2, colour = "black") +
  geom_point(aes(x = course_and_lecturer, y = mean_qs), colour = "black", shape = 4) +
  #coord_flip() +
  labs(x = NULL, y = "Number of questions") +
  theme_minimal(base_size = 16) +
  scale_colour_viridis_c() +
  scale_fill_viridis_d() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),
        #panel.grid.major.x=element_blank(),
        #        panel.spacing=grid::unit(2, "lines"),
        #strip.text=element_text(hjust=0, size=12)
        )
  #ggtitle("Distribution of number of questions asked per lecture")
ggsave("Paper1_FILLplus_LQ_distribution_old.pdf",width=20,height=10,units="cm",dpi=300)


summary_of_LQ_SQ = codes_with_durations %>% 
  group_by(discipline, course_anonid, lecturer_anonid, session_number) %>% 
  summarise(
    num_LQ = sum(code == "LQ", na.rm = TRUE),
    num_SQ = sum(code == "SQ", na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # add the mean for each course-lecturer combo
  group_by(discipline, course_anonid, lecturer_anonid) %>% 
  mutate(
    mean_LQ = mean(num_LQ, na.rm = TRUE),
    se_LQ = sd(num_LQ, na.rm = TRUE) / sqrt(n()),
    mean_SQ = mean(num_SQ, na.rm = TRUE),
    se_SQ = sd(num_SQ, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup() %>% 
  mutate(
    course_num = parse_number(course_anonid),
    course_and_lecturer = paste(course_anonid, lecturer_anonid)
  )

# LQs
summary_of_LQ_SQ %>% 
  mutate(
    course_and_lecturer = fct_reorder(course_and_lecturer, mean_LQ, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = num_LQ, fill = course_anonid)) +
    facet_grid(cols = vars(discipline),
               scales = "free",
               space = "free_x",
               shrink = TRUE) +
    geom_point(aes(color = course_num),
               position = position_jitter(width = 0.1),
               alpha = 0.8) +
    geom_errorbar(aes(ymin=(mean_LQ-se_LQ), ymax=(mean_LQ+se_LQ)), width=.2, colour = "black") +
    geom_point(aes(x = course_and_lecturer, y = mean_LQ), colour = "black", shape = 4) +
    labs(x = NULL, y = "Number of questions") +
    theme_minimal(base_size = 16) +
    scale_colour_viridis_c() +
    scale_fill_viridis_d() +
    theme(strip.text.y = element_text(angle = 0),
          axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
          legend.position = "none",
          panel.grid.minor.y=element_blank(),
    )
ggsave("Paper1_FILLplus_LQ_distribution.pdf",width=20,height=10,units="cm",dpi=300)

# SQs
summary_of_LQ_SQ %>% 
  mutate(
    course_and_lecturer = fct_reorder(course_and_lecturer, mean_SQ, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = num_SQ, fill = course_anonid)) +
  facet_grid(cols = vars(discipline),
             scales = "free",
             space = "free_x",
             shrink = TRUE) +
  geom_point(aes(color = course_num),
             position = position_jitter(width = 0.1),
             alpha = 0.8) +
  geom_errorbar(aes(ymin=(mean_SQ-se_SQ), ymax=(mean_SQ+se_SQ)), width=.2, colour = "black") +
  geom_point(aes(x = course_and_lecturer, y = mean_SQ), colour = "black", shape = 4) +
  labs(x = NULL, y = "Number of student questions") +
  theme_minimal(base_size = 16) +
  scale_colour_viridis_c() +
  scale_fill_viridis_d() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),
  )
ggsave("Paper1_FILLplus_SQ_distribution.pdf",width=20,height=10,units="cm",dpi=300)




# Table for avg durations


codes_with_durations %>% 
  group_by(lecturer_and_session) %>% 
  mutate(endtime = case_when(code == "END" ~ time),
         endtime = zoo::na.locf(endtime, fromLast = TRUE)) %>% 
  ungroup() %>% 
  group_by(lecturer_and_session, code) %>% 
  mutate(session_total = sum (duration),
         session_perc = session_total/endtime) %>% 
  ungroup() %>% 
  group_by(code) %>% 
  summarise(mean = mean(session_perc),
            sd = sd(session_perc))

code_summary_by_session = codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  group_by(discipline, course_anonid, lecturer_anonid, session_number, code) %>% 
  summarise(
    mins = sum(duration)
  ) %>% 
  group_by(discipline, course_anonid, lecturer_anonid, session_number) %>% 
  mutate(
    sum = sum(mins, na.rm = TRUE),
    prop = mins/sum
  )

#This is an improved version that takes account of some codes not being observed
code_summary_by_session = codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  group_by(discipline, course_anonid, lecturer_anonid, session_number, code) %>% 
  # add entries for any codes which are missing
  complete(code,
           nesting(discipline, course_anonid, lecturer_anonid, session_number),
           fill = list(duration = 0)) %>% 
  summarise(
    mins = sum(duration)
  ) %>% 
  group_by(discipline, course_anonid, lecturer_anonid, session_number) %>% 
  mutate(
    sum = sum(mins, na.rm = TRUE),
    prop = mins/sum
  )

code_summary_by_session %>% 
  ggplot(aes(x = lecturer_anonid, y = prop, colour = code)) +
  facet_grid(~code) +
  geom_point() +
  theme_minimal(base_size = 16)

#
# Investigation of LT
#
codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  filter(code == "LT") %>% 
  group_by(course_anonid, lecturer_anonid) %>% 
  mutate(
    duration_LT = case_when(code == "LT" ~ duration),
    longest_LT = max(duration_LT, na.rm = TRUE),
    mean_LT = mean(duration_LT, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    course_and_lecturer = as.factor(paste(course_anonid, lecturer_anonid)),
    course_and_lecturer = fct_reorder(course_and_lecturer, longest_LT, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = duration, colour = code)) +
  facet_grid(~code) +
  geom_point() +
  geom_point(aes(x = course_and_lecturer, y = mean_LT), colour = "red", shape = 4) +
  scale_colour_manual("FILL+ code", values = fillplus) +
  theme_minimal(base_size = 16)

all_LT_durations = codes_with_durations %>% 
  filter(code == "LT") %>% 
  select(duration) %>% 
  mutate(duration = duration)
all_LT_durations %>% summary()
all_LT_durations %>% 
  ggplot(aes(x = duration)) + geom_density() + labs(x = "duration of LT in seconds")
all_LT_durations %>% 
  ggplot(aes(x = log(duration))) + geom_density() + labs(x = "log(duration of LT)")

code_use_summary = codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  group_by(discipline, course_anonid, lecturer_anonid, code) %>% 
  summarise(
    duration_max = max(duration, na.rm = TRUE),
    duration_mean = mean(duration, na.rm = TRUE),
    duration_sd = sd(duration, na.rm = TRUE),
    duration_total = sum(duration, na.rm = TRUE),
    duration_count = n()
  )

code_use_summary_long = code_use_summary %>% 
  pivot_longer(
    cols = contains("duration_"),
    names_to = c(".value", "stat"),
    names_sep = "_"
  )

code_use_summary %>%
  mutate(
    course_and_lecturer = as.factor(paste(course_anonid, lecturer_anonid)),
    #course_and_lecturer = fct_reorder(course_and_lecturer, longest_LT, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = duration_mean, colour = code)) +
  facet_grid(~code) +
  geom_point() +
  #geom_point(aes(x = course_and_lecturer, y = mean_LT), colour = "red", shape = 4) +
  scale_colour_manual("FILL+ code", values = fillplus) +
  theme_minimal(base_size = 16)

code_use_summary %>% 
  group_by(discipline, code) %>% 
  summarise(
    avg_duration_max = mean(duration_max, na.rm = TRUE)/60,
    avg_duration_mean = mean(duration_mean, na.rm = TRUE)/60,
    avg_duration_sd = mean(duration_sd, na.rm = TRUE)/60
  ) %>% 
  filter(code == "LT")


codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  filter(code == "LT") %>% 
  group_by(course_anonid, lecturer_anonid) %>% 
  mutate(
    duration_LT = case_when(code == "LT" ~ duration),
    longest_LT = max(duration_LT, na.rm = TRUE),
    mean_LT = mean(duration_LT, na.rm = TRUE),
    se_LT = sd(duration_LT, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup() %>% 
  mutate(
    course_num = parse_number(course_anonid),
    course_and_lecturer = as.factor(paste(course_anonid, lecturer_anonid)),
    course_and_lecturer = fct_reorder(course_and_lecturer, mean_LT, .desc = TRUE)
  ) %>% 
  ggplot(aes(x = course_and_lecturer, y = duration/60, colour = course_num)) +
  facet_grid(~discipline ,
             scales = "free",
             space = "free_x",
             shrink = TRUE) +
  geom_point() +
  geom_errorbar(aes(ymin=(mean_LT-se_LT)/60, ymax=(mean_LT+se_LT)/60), width=.2, colour = "black") +
  geom_point(aes(x = course_and_lecturer, y = mean_LT/60), colour = "black", shape = 4) +
  #scale_colour_manual("FILL+ code", values = fillplus) +
  scale_colour_viridis_c() +
  theme_minimal(base_size = 16) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),) +
  labs(#x = "Course/lecturer combination",
       x = "",
       y = "Duration of LT (min)")
ggsave("Paper1_FILLplus_LT_distribution.pdf",width=20,height=10,units="cm",dpi=300)

codes_with_durations %>% 
  filter(!is.na(code), !code == "END") %>% # remove these non-interesting codes
  # add the proportion of time spent in LT for each session
  group_by(course_anonid, lecturer_anonid, session_number) %>% 
  mutate(
    lecture_duration = sum(duration),
    duration_LT = case_when(code == "LT" ~ duration),
    prop_LT = sum(duration_LT, na.rm = TRUE) / lecture_duration,
  ) %>% 
  #filter(code == "LT") %>% 
  group_by(course_anonid, lecturer_anonid) %>% 
  mutate(
    longest_LT = max(duration_LT, na.rm = TRUE),
    mean_LT = mean(duration_LT, na.rm = TRUE),
    se_LT = sd(duration_LT, na.rm = TRUE) / sqrt(n()),
    mean_prop_LT = mean(prop_LT),
  ) %>% 
  ungroup() %>% 
  mutate(
    course_and_lecturer = as.factor(paste(course_anonid, lecturer_anonid)),
    course_and_lecturer = fct_reorder(course_and_lecturer, longest_LT, .desc = TRUE),
    course_and_lecturer = fct_reorder(course_and_lecturer, mean_prop_LT, .desc = TRUE)
  ) %>% 
  # restrict the plot to just LT durations
  filter(code == "LT") %>% 
  ggplot(aes(x = course_and_lecturer, y = duration/60, colour = code)) +
  facet_grid(~discipline ,
             scales = "free",
             space = "free_x",
             shrink = TRUE) +
  geom_point() +
  geom_errorbar(aes(ymin=(mean_LT-se_LT)/60, ymax=(mean_LT+se_LT)/60), width=.2, colour = "black") +
  geom_point(aes(x = course_and_lecturer, y = mean_LT/60), colour = "black", shape = 4) +
  geom_point(aes(x = course_and_lecturer, y = mean_prop_LT*50), colour = "red", shape = 4) +
  #scale_colour_manual("FILL+ code", values = fillplus) +
  scale_colour_viridis_c() +
  theme_minimal(base_size = 16) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "none",
        panel.grid.minor.y=element_blank(),) +
  labs(x = "Course/lecturer combination",
       y = "Duration of LT (min)")
ggsave("Paper1_FILLplus_LT_distribution_withprops.pdf",width=20,height=10,units="cm",dpi=300)

# proportions in each session sum to 1
code_summary_by_session %>% 
  group_by(discipline, lecturer_anonid, session_number) %>% 
  summarise(tot_prop = sum(prop))

code_summary_table = code_summary_by_session %>% 
  group_by(discipline, code) %>%
  summarise(
    mean_prop = mean(prop),
    sd_prop = sd(prop)
  ) %>% 
  bind_rows(
    # produce a version for all the disciplines combined
    code_summary_by_session %>% 
      group_by(code) %>%
      summarise(
        mean_prop = mean(prop),
        sd_prop = sd(prop)
      ) %>% 
      mutate(discipline = "OVERALL")
  ) %>% 
  filter(!is.na(code), !code == "END") # remove these non-interesting codes
  
# check: the proportions should sum to 1!
code_summary_table %>% 
  group_by(discipline) %>% 
  summarise(tot_prop = sum(mean_prop))

# Table with all the gory details
code_summary_table %>%  
  mutate(
    pretty_cell_value = glue::glue("{format(mean_prop*100, digits = 1)}\\% ($\\pm${format(sd_prop*100, digits = 1)}\\%)")
  ) %>% 
  select(discipline, code, pretty_cell_value) %>% 
  pivot_wider(
    names_from = code,
    values_from = pretty_cell_value,
    values_fill = list(pretty_cell_value = "-")
  ) %>% 
  kable(format = "latex", escape = FALSE, booktabs = T)
  
# More compact version - Table 4 in the paper
code_summary_table %>%  
  mutate(
    pretty_cell_value = glue::glue("{format(mean_prop*100, digits = 1)} ($\\pm${format(sd_prop*100, digits = 1)})"),
    # even more compact - remove the sd's
    #pretty_cell_value = glue::glue("{format(mean_prop*100, digits = 1)}")
  ) %>% 
  select(discipline, code, pretty_cell_value) %>% 
  pivot_wider(
    names_from = code,
    values_from = pretty_cell_value,
    values_fill = list(pretty_cell_value = "-")
  ) %>% 
  # transpose the table
  t %>% 
  kable(format = "latex", escape = FALSE, booktabs = T, linesep = c(""))
