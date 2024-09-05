# load packages
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(tidygeocoder)
library(sf)
library(modeldb)

# load data
new_data <- read_csv("data/new_MCMF_data.csv",
                     show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(id = row_number())
chicago_sf <- read_sf("data/CommAreas.geojson")

# skim data
skim_without_charts(new_data)

# clean data ----
## turn date columns into datetime
new_data <- new_data %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y"))

## filter 2024 data
new_data <- new_data %>% 
  filter(start_date >= as.Date("2024-01-01") | end_date >= as.Date("2024-01-01"))

## clean names, get rid of unneeded columns and columns w/ missingness issue
new_data <- new_data %>%
  select(-c("registration_open", "registration_deadline",
            "online_address", "program_url", "image",
            "registration_url","contact_name", "contact_email", 
            "contact_phone", "start_time", "end_time", "custom_categories")) 

## clean category_name values
new_data <- new_data %>%
  mutate(category_name = str_replace(category_name, '&', 'And'),
         category_name = str_replace_all(category_name, '\\.', '')) 

## get rid of observations with min_age < 25 and NA category
new_data <- new_data %>% 
  filter(min_age < 25, !is.na(category_name)) 

## get rid of non-Chicago observations
new_data <- new_data %>%
  filter(city == "Chicago" | is.na(city))

## fix max_age: anything >= 24 is set to 24
new_data <- new_data %>%
  mutate(max_age = case_when(max_age > 24 ~ 24,
                             max_age <= 24 ~ max_age))

## create min_grade and max_grade variables
new_data <- new_data %>%
  mutate(min_grade = case_when(
    min_age <= 5 ~ 0, min_age == 6 ~ 1, min_age == 7 ~ 2, min_age == 8 ~ 3,
    min_age == 9 ~ 4, min_age == 10 ~ 5, min_age == 11 ~ 6, min_age == 12 ~ 7,
    min_age == 13 ~ 8, min_age == 14 ~ 9, min_age == 15 ~ 10, min_age == 16 ~ 11,
    min_age == 17 ~ 12, min_age >= 18 ~ 13),
    max_grade = case_when(
      max_age <= 5 ~ 0, max_age == 6 ~ 1, max_age == 7 ~ 2, max_age == 8 ~ 3,
      max_age == 9 ~ 4, max_age == 10 ~ 5, max_age == 11 ~ 6, max_age == 12 ~ 7,
      max_age == 13 ~ 8, max_age == 14 ~ 9, max_age == 15 ~ 10, max_age == 16 ~ 11,
      max_age == 17 ~ 12, max_age >= 18 ~ 13)) 

## fix capacity: anything >= 500 is set to 500
new_data <- new_data %>%
  mutate(capacity = case_when(capacity >= 500 ~ 500,
                              capacity < 500 ~ capacity))

## impute values for capacity = 0 (online: 500, face to face: mean)
new_data <- new_data %>%
  # convert all 0 capacities to NA
  mutate(capacity = na_if(capacity, 0)) %>%
  # replace all online NA capacities to 500
  mutate(capacity = case_when(
    meeting_type == "online" & is.na(capacity) ~ 500,
    .default = capacity
  )) %>%
  # impute face to face NA capacities with median
  mutate(capacity = replace_na(capacity, median(capacity, na.rm = TRUE)))

## replace free food and transportation NA with FALSE
new_data <- new_data %>%
  mutate(has_free_food = replace_na(has_free_food, FALSE)) %>%
  mutate(transport_provided = replace_na(transport_provided, FALSE))

## hand impute some communities
new_data <- new_data %>%
  mutate(geographic_cluster_name = case_when(
    id == 249838 ~ "WEST LAWN",
    id == 251263 ~ "WEST LAWN",
    id == 285124 ~ "SOUTH DEERING",
    .default = geographic_cluster_name
  ))

## get rid of incorrect community names
new_data <- new_data %>%
  mutate(geographic_cluster_name = case_when(
    geographic_cluster_name == "Far South Equity Zone" ~ NA,
    geographic_cluster_name == "Near South Equity Zone" ~ NA,
    geographic_cluster_name == "North/Central Equity Zone" ~ NA,
    geographic_cluster_name == "Northwest Equity Zone" ~ NA,
    geographic_cluster_name == "Southwest Equity Zone" ~ NA,
    geographic_cluster_name == "West Equity Zone" ~ NA,
    geographic_cluster_name == "Greater Englewood" ~ NA,
    geographic_cluster_name == "Greater Roseland" ~ NA,
    .default = geographic_cluster_name
  ))

## get rid of face to face observations that don't have addresses and lon/lat data
new_data <- new_data %>%
  filter(!(is.na(address) & 
             is.na(latitude) & 
             meeting_type == "face_to_face" &
             is.na(geographic_cluster_name))) %>%
  filter(!(meeting_type == "face_to_face" & 
             is.na(geographic_cluster_name) &
             !is.na(address) &
             is.na(zip_code)))

## impute missing geographic_cluster_name for face to face rows
### use geocode data to match geographic cluster 
sf_coords <- new_data %>% 
  filter(meeting_type == "face_to_face" & is.na(geographic_cluster_name)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84")
sf <- st_join(chicago_sf, sf_coords, left = FALSE) %>%
  select(-c("area", "shape_area", "perimeter", "area_num_1", "area_numbe",
            "comarea_id", "comarea", "shape_len")) %>%
  st_drop_geometry()
new_data <- merge(x = new_data,
                  y = sf[ , c("id", "community")],
                  by = "id", all.x = TRUE) %>%
  mutate(community_name = coalesce(geographic_cluster_name, community)) %>%
  select(-c(geographic_cluster_name, community))

## replace all online community with NA
new_data <- new_data %>%
  mutate(community_name = case_when(
    meeting_type == "face_to_face" ~ community_name,
    meeting_type == "online" ~ NA
  ))

## fix community names
new_data <- new_data %>%
  mutate(community_name = case_when(
    community_name == "Little Village" ~ "SOUTH LAWNDALE",
    community_name == "Garfield Park" ~ "EAST GARFIELD PARK",
    community_name == "Bronzeville/South Lakefront" ~ "GRAND BOULEVARD",
    community_name == "Back of the Yards" ~ "NEW CITY",
    .default = community_name
  ))

## create priority region factor variable 
list_priority_areas = c("Austin", "North Lawndale", "Humboldt Park", 
                        "East Garfield Park", "Englewood", "Auburn Gresham",
                        "West Garfield Park", "Roseland", "Greater Grand Crossing",
                        "West Englewood", "South Shore", "New City", "Chicago Lawn",
                        "South Lawndale", "West Pullman"
)
new_data <- new_data %>%
  mutate(priority = tolower(community_name) %in% 
           tolower(list_priority_areas)) 

## combine categories to general categories, drop category_name column
new_data <- new_data %>%
  mutate(general_category = case_when(
    category_name %in% c("Music And Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure and Arts",
    category_name %in% c("Academic Support", "Math", "Reading And Writing", "Science", "Science And Math","Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building And Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service")) %>%
  select(-category_name)

## get rid of duplicate observations
new_data <- new_data %>% 
  distinct(id, general_category, .keep_all = TRUE) %>%
  distinct(program_name, general_category, .keep_all = TRUE)

## change general_category into dummy variables to combine multiple category observations
dat_dummy <- new_data %>% 
  add_dummy_variables(x = general_category,
                      values = c(NA, "Leisure and Arts", "Academics", 
                                 "Professional Skill Building", 
                                 "Community Service"))
dat_dummy_combine <- dat_dummy %>%
  group_by(id) %>%
  summarize(`general_category_Leisure and Arts` = sum(`general_category_Leisure and Arts`),
            general_category_Academics = sum(general_category_Academics),
            `general_category_Community Service` = sum(`general_category_Community Service`),
            `general_category_Professional Skill Building` = sum(`general_category_Professional Skill Building`))
new_data <- inner_join(dat_dummy, dat_dummy_combine, by = 'id') %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-c("general_category_Academics.x", 
            "general_category_Community Service.x",
            "general_category_Professional Skill Building.x",
            "general_category_Leisure and Arts.x")) %>%
  rename(academics_dum = "general_category_Academics.y", 
         community_service_dum = "general_category_Community Service.y",
         professional_skill_dum = "general_category_Professional Skill Building.y",
         leisure_arts_dum = "general_category_Leisure and Arts.y")

## deselect location variables
new_data <- new_data %>%
  select(-c("address", "city", "state", "zip_code", "longitude", "latitude"))


## save data
save(new_data, file = "data/wrangle/new_data_clean.rda")


# new time series data ----
time_data_all <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days) %>%
  summarize(total_count = n()) 
## meeting type counts
time_data_meeting_type <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days, meeting_type) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = meeting_type, values_from = count) %>%
  replace_na(list(face_to_face = 0, online = 0)) 
## community counts
time_data_community <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days, community_name) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = community_name, values_from = count) %>%
  select(-"NA") %>%
  replace(is.na(.), 0)
# priority community counts 
time_data_priority <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days, priority) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = priority, values_from = count) %>% 
  select("TRUE") %>%
  rename("priority_TRUE" = "TRUE") %>%
  replace(is.na(.), 0)
# program price counts
time_data_price <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days, program_price) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = program_price, values_from = count) %>%
  rename("price_free" = "Free",
         "price_less_50" = "$50 or Less",
         "price_more_50" = "More Than $50") %>%
  replace(is.na(.), 0)
## program pays counts
time_data_pay <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days, participants_paid) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = participants_paid, values_from = count) %>%
  select("Paid, Type Unknown") %>%
  rename("pay_TRUE" = "Paid, Type Unknown") %>%
  replace(is.na(.), 0)
## program category counts
time_data_category <- new_data %>%
  rowwise() %>%
  mutate(days = list(seq(start_date, end_date, by = 1))) %>%
  unnest_longer(col = days) %>%
  group_by(days) %>%
  summarize(cat_academics = sum(academics_dum),
            cat_comm = sum(community_service_dum),
            cat_leisure = sum(leisure_arts_dum),
            cat_prof = sum(professional_skill_dum))
## join datasets
new_time_data <- time_data_all %>%
  inner_join(y = time_data_category, by = "days") %>%
  inner_join(y = time_data_community, by = "days") %>%
  inner_join(y = time_data_meeting_type, by = "days") %>%
  inner_join(y = time_data_pay, by = "days") %>%
  inner_join(y = time_data_price, by = "days") %>%
  inner_join(y = time_data_priority, by = "days") %>%
  filter(days >= as.Date("2024-02-01") & days <= as.Date("2024-08-30"))

## save data
save(new_time_data, file = "data/wrangle/new_time_series.rda")
