library(data.table) # used for reading in csv files
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)

data_location = "CSV Files FILL+ 1.1/"

# Read in the master spreadsheet "courses_and_lecturers.csv" which lists details of
# each of the course/lecturer combinations we observed
courses_and_lecturers = read_csv(paste0(data_location, "courses_and_lecturers_SS.csv"),
                                 col_types = cols()) %>% 
  mutate(
    file_path = paste0(data_location, course_acronym, "_", lecturer, "/")
  )

# helper function used in purrr::map call below
list_csv_in_path = function(path) {
  df = path %>%
    list.files() %>%
    .[str_detect(., ".csv")]#
  return(df)
}

# Look for the csv files which correspond to the course/lecturer combos in courses_and_lecturers.csv
#  - note that this relies on csv files being stored in directories with consistent naming pattern
csv_file_details = courses_and_lecturers %>% 
  mutate(
    # create a column to store the names of all the csv files in each path
    csv_file_names = purrr::map(file_path, list_csv_in_path)
  ) %>% 
  # flatten out the nested tables and tidy up
  unnest(cols = c(csv_file_names)) %>% 
  dplyr::rename(file_name = csv_file_names)

# read in all of the CSV files - note that this generates some warnings due to the CSV files
# having empty columns (only the first 2 are used)
raw_codes = csv_file_details %>%
  mutate(
    raw_coding = purrr::map2(file_path, file_name, function(file_path, file_name){ # iterate through each file name
      # df = read_csv(paste0(file_path, file_name),
      #               col_types = "ccc") %>%
      #   select(1:2)  # only keep the first two columns
      # names(df) = c("start", "code")
      
      # read in the csv, keeping only the first two columns
      df = fread(
        paste0(file_path, file_name),
        select = c(1:2),
        header = TRUE,
        blank.lines.skip = TRUE,
        col.names = c("start", "code")
      ) %>% 
        # convert timestamps to number of seconds
        mutate(
          time = as.integer(seconds(hms(start))),
          code = str_trim(code) # str_trim removes whitespace, e.g. "LT "->"LT"
        ) %>% 
        select(time, code) %>% 
        drop_na(time) # to get rid of empty rows from the CSV
      return(df)
    })
  )

# Helper functions to process the raw codings
get_codes_by_second = function(df) {
  data.frame(time = 1:(2*60*60)) %>% # start with a "plain" 2 hour period
    left_join(df, by = "time") %>% 
    fill(code) %>% # this fills in the times between codes with the last assigned code
    mutate(
      code = na_if(code, "END"),
      code_grp = as.factor(case_when(
        code %in% c("AD", "LT")             ~ "NON",
        code %in% c("LQ", "SR", "SQ", "LR") ~ "VIC",
        code %in% c("CQ", "ST", "SD", "FB") ~ "INT",
        TRUE                                ~ "NA"
      )),
      y_pos = case_when(
        code_grp == "NON" ~ 0,
        code_grp == "VIC" ~ 0.5,
        code_grp == "INT" ~ 1,
        TRUE ~ 0
      )
    )
}

get_codes_with_duration = function(df){
  df %>% mutate(
    duration = lead(time,1) - time
  )
}

# process the file names to extract details like the date and the coder,
# then add extra columns containing the codings
# Note - this will generate some warnings:
#  * "Expected 5 pieces" coming from the course/year/month/day/slot,
#       since most files will not specify a "slot"
#  * "Expected 2 pieces" coming from the final _coder_IRR piece,
#       since most files don't have the IRR part
all_codings = raw_codes %>% 
  mutate(name = str_replace(file_name, ".csv", "")) %>% 
  # separate the file name into pieces:
  # 1. the lecture details in the form COURSECODE_YEAR_MONTH_DAY,
  #    or COURSECODE_YEAR_MONTH_DAY_SESSION (if there is a SESSION,
  #    it must begin with a non alphabetic character, e.g. a number)
  # 2. the coder's name
  # 3. whether the coding is for IRR
  # so examples would be: ILA_2019_10_19_George_forIRR.csv
  #                       CAP_2019_01_12_1PM_Thomas.csv
  separate(name,
           sep = "_(?=[:alpha:])",
           into = c("lecture_id", "coder", "IRR")) %>% 
  # now break apart the lecture_id into its pieces
  separate(lecture_id,
           sep = "_",
           into = c("course", "year", "month", "day", "slot"),
           remove = FALSE) %>% 
  # add columns with transformations applied to the raw codes
  mutate(
    coding = raw_coding,
    codes_by_second = purrr::map(coding, get_codes_by_second),
    codes_with_duration = purrr::map(coding, get_codes_with_duration)
  )

# check that the IRR codings have been identified successfully
all_codings %>% 
  filter(str_detect(file_name, "IRR")) %>% 
  select(file_name, IRR)

# summary of contributions by different coders
all_codings %>% 
  group_by(coder) %>% 
  tally()

# Separate out the codings that were done for IRR
# Note - the ending _IRR indicates a 2nd coding done to allow IRR comparison
#      - the ending _forIRR indicates a 1st coding of a lecture which included pauses,
#         and codes for the missing segments have been appended after the end of the lecture
#         (while the actual timeline appears in the version without the _forIRR extension)
IRR_codings = all_codings %>% 
  filter(!is.na(IRR))

# Retain only the non-IRR codings for analysis
all_codings = all_codings %>% 
  filter(is.na(IRR)) %>%
  # Also drop all the ILA 1PM codings for the main plot
  # (but note that we need to keep the 1PM CAP L2 since no 12pm ones are available)
  #filter((course_acronym == "ILA" & !slot == "1PM") | is.na(slot)) %>% 
  filter(!(course_acronym == "ILA" & slot == "1PM") | is.na(slot)) %>% 
  select(-IRR)

# Produce a table where each row is an assigned code, with a given duration
codes_with_durations = all_codings %>% 
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
    duration_int = ifelse(code_grp %in% c("INT"), duration, 0),
    anon_course_code = paste0("C", as.integer(as.factor(course_code))),
    title = paste0(course, " ", year,"-",month,"-",day, " (", coder, ")"),
    lecture_date = lubridate::ymd(paste(year, month, day)),
    simple_title = case_when(
      # if there are two lectures on the same day, add slot info
      !is.na(slot) ~ paste0(coder, " ", day, "/", month, " (", slot, ")"),
      # otherwise just coder and short date
      TRUE ~ paste0(coder, " ", day, "/", month)
    )
    #    course = fct_reorder(course, course, .desc = TRUE),
    #    course = fct_reorder(course, discipline, .desc = TRUE)
  )

#
# Sanity checks
#

# total lecture time coded up
codes_with_durations %>% 
  summarise(
    total_duration = sum(duration, na.rm = TRUE),
    total_duration_min = total_duration / 60
  )

# how many lectures in each lecturer/course combo? (should be at least 4)
all_codings %>% 
  group_by(course, lecturer) %>% 
  summarise(
    n_lectures = n()
  ) %>% 
  arrange(n_lectures)

# check for any typos in coding
codes_with_durations %>% 
  filter(duration < 0) %>% 
  mutate(timestamp = hms::as_hms(time)) %>% 
  select(course_acronym, lecturer, file_name, timestamp, code, duration)

# check for unusually long periods of student input
codes_with_durations %>% 
  filter(duration >60,
         code %in% c("SR", "SQ")) %>% 
  select(file_name, time, code, duration)

# Export data for use in the ILA interactivity/exam results question
codes_with_durations %>% 
  filter(course_acronym == "ILA") %>% 
  saveRDS(file = "ILA1819-FILL_codes_with_durations.rds")


# Summary stats
library(janitor)

all_codings %>% 
  group_by(discipline, course_name, lecturer_name) %>% 
  tally() %>% 
  group_by(discipline) %>% 
  summarise(
    count = n(),
    num_lectures = sum(n)
  ) %>%
  adorn_totals("row") 

# Export data so that some of the previous steps can be skipped
all_codings %>% 
  saveRDS(file = "From L Drive/all_codings.rds")


#
# Produce anonymised dataset
#

lecturer_ids = all_codings %>% 
  select(discipline, lecturer_name) %>% 
  distinct() %>% 
  arrange(desc(discipline)) %>% # fortunately BIO and CHEM come last in this ordering!
  mutate(lecturer_anonid = paste0("L", row_number()))

course_ids = all_codings %>% 
  select(discipline, course_code, course_name) %>% 
  distinct() %>% 
  arrange(desc(discipline)) %>% # fortunately BIO and CHEM come last in this ordering!
  mutate(course_anonid = paste0("C", row_number()))

all_codings_anon = all_codings %>% 
  left_join(lecturer_ids) %>% 
  left_join(course_ids) %>% 
  unite(lecture_id, course_anonid, year, month, day, slot, sep = "_", remove = FALSE, na.rm = TRUE) %>% 
  select(course_anonid, lecturer_anonid, lecture_id, everything()) %>% 
  select(-course_code, -course_name, -lecturer, -lecturer_name,
         -course_acronym, -course, -unique_code, -file_path, -file_name) %>% 
  # add a session identifier, which obscures the dates
  mutate(lecture_date = lubridate::ymd(paste(year, month, day))) %>% 
  arrange(lecture_date, slot) %>% 
  group_by(course_anonid) %>% 
  mutate(
    session_number = row_number(), # number lectures within a course
    lecturer_and_session = paste0(lecturer_anonid, " S", session_number),
    title = paste0(course_anonid, " ", year,"-",month,"-",day, " (", coder, ")"),
    simple_title = case_when(
      # if there are two lectures on the same day, add slot info
      !is.na(slot) ~ paste0(coder, " ", day, "/", month, " (", slot, ")"),
      # otherwise just coder and short date
      TRUE ~ paste0(coder, " ", day, "/", month)
    )
  ) %>% 
  ungroup() %>% 
  filter(!discipline %in% c("BIO", "CHE"))


#
# Further filtering and sanity checks
#
tab_number_of_lecturers = all_codings_anon %>% 
  group_by(discipline, lecturer_anonid) %>% 
  tally() %>% 
  group_by(discipline) %>% 
  summarise(
    num_lecturers = n()
  ) %>%
  adorn_totals("row") 
tab_number_of_lecturers

tab_coding_summary = all_codings_anon %>% 
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

#
# Write the anonymised data
#
all_codings_anon %>% 
  saveRDS(file = "all_codings_anon.rds")
