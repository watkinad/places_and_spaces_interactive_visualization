#This script prepares the venues data by performing basic cleaning and categorizing each venue

pacman::p_load(rjson,dplyr,stringr,jsonlite,lubridate)

json_file <- "venues.json"
json_data <- fromJSON(json_file)

#performs minor data cleaning and standardization to venue data
venue_df <- json_data %>%
  select(-venueImages) %>% 
  mutate(venue = coalesce(na_if(venue,''),title),
         country = case_when(trimws(toupper(country)) %in% c('US','USA') ~ 'United States',
                             country == 'GA' & venue == 'Kennesaw State University' ~ 'United States',
                             state == 'Canada' ~ 'Canada',
                             .default = country),
         state = case_when(country == 'GA' & venue == 'Kennesaw State University' ~ 'GA',
                           state == 'Canada' ~ NA,
                           .default = state),
         year = year(dateStart)) 

#classifies venues by type based on keywords
venue_df <- venue_df %>%
  mutate(venue_classification = case_when(
    str_detect(tolower(venue),"university") | 
      str_detect(tolower(venue),"college") |
      str_detect(tolower(venue),"school") |
      str_detect(tolower(venue),"luddy") |
      str_detect(tolower(venue),"universite") |
      str_detect(tolower(venue),"université") |
      str_detect(tolower(venue),"universität") |
      str_detect(tolower(venue),"universidad") ~ "University",
    str_detect(tolower(venue),"workshop") ~ "Workshop",
    str_detect(tolower(venue),"conference") |
      str_detect(tolower(venue),"meeting") |
      str_detect(tolower(venue),"convention") |
      str_detect(tolower(venue),"symposium") |
      str_detect(tolower(venue),"forum")~ "Conference",
    str_detect(tolower(venue),"library") ~ "Library",
    str_detect(tolower(venue),"museum") ~ "Museum",
    str_detect(tolower(venue),"institution") |
    str_detect(tolower(venue),"institute") ~ "Institute",
    .default = "Other"))

table(venue_df$venue_classification)

write.csv(venue_df,"venue_classifications_update_03_30_2025.csv",fileEncoding = "latin1")