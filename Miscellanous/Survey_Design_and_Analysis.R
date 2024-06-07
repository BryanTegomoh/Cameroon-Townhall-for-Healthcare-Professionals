# FormR API access ---------------------------------------------------------
library(httr)
library(tidyverse)

access_token <- list(
  client_id = Sys.getenv("FORMR_CLIENT_ID"),
  client_secret = Sys.getenv("FORMR_CLIENT_SECRET"),
  grant_type = "client_credentials") %>%
  POST("https://api.formr.org/oauth/access_token", body = ., encode = "form") %>%
  content() %>%
  .$access_token

results <- list(
  access_token = access_token,
  "run[name]" = "survey-cameroontownhall-2024",
  "surveys[Survey_CameroonTownHall_2024]" = "") %>%
  GET("https://api.formr.org/get/results", query=.) %>%
  content()

# Load Packages -----------------------------------------------------------
pacman::p_load(tidyverse, 
               here, 
               rio,
               janitor, 
               formr, 
               googlesheets4 # using this to import my formr googlesheet
               )

# Change dplyr settings so I can see all columns 
options(dplyr.width = Inf)

# Data importation -------------------------------------------------------------

gs4_auth() # this takes away the need for authentication on my google acount ( with 1, I need to re-authenticate, otherwise 2)
gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly") 


## Importing the Online Googlesheet with survey questions and choices --------

googlesheet_survey_formr <- read_sheet("https://docs.google.com/spreadsheets/d/1O0fbMkwMflDxyodoj1TOakEVja1JXfzWoPig9yk4x_I/edit#gid=1611481919") #%>% 
  select(name, label, starts_with('choice')) %>%
  filter(!is.na(name)) %>% 
  pivot_longer(cols = starts_with('choice'), 
               names_to = 'choice_name', 
               values_to = 'choice_value',
               # drop NA values while pivoting
               values_drop_na = TRUE) %>% 
  # extract number from choice columns
  mutate(choice_number = readr::parse_number(choice_name))
  

# Importing, parsing and joining the Survey Results from formR API ----------------------------

survey_api_results <- bind_rows(results) %>% 
    pivot_longer(cols = -c(session, created, current_position),
                 names_to = "item_name",
                 values_to = "responses",
                 cols_vary = "slowest") %>% 
    select(-created, -current_position) %>% 
    # separate multiple choice answers into rows where commas separate digits
    tidyr::separate_rows(responses, sep = "(?<=\\d), (?=\\d)") %>% 
    dplyr::left_join(
      # minor cleanup to make the join work
      googlesheet_survey_formr %>% 
        mutate(choice_number = as.character(choice_number)) %>% 
        select(choice_number, choice_value, name), 
      by = c("item_name" = "name", "responses" = "choice_number")) 

# Convert to wide format ---------------------------------------------------

# survey_api_results_wide <- survey_api_results %>% 
#   group_by(session, item_name) %>% 
#   summarize(responses = paste(choice_value, collapse = " , ")) %>%
#   pivot_wider(names_from = item_name, 
#               values_from = responses) %>% 
#   mutate(across(everything(), ~ ifelse(. == "NA", "", .)))


# Data Export  -----------------------------------------

export(survey_api_results, "api_results.xlsx")

