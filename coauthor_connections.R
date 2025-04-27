#This script identifies pairs of coauthors based on the people and submissions data

pacman::p_load(dplyr,stringr,tidyr)

people_df <- read.csv('Places & Spaces people - submissions-person_P&S_V1.2.csv')

#gets location names for each person
locations_df <- people_df %>% 
  filter(location_name != '') %>%
  select(name,location_name) 

#gets connections between authors
connections_df <- people_df %>%
  filter(makers != '') %>%
  select(name,makers) %>%
  mutate(makers = str_replace_all(makers,'/readme','')) %>%
  mutate(makers = str_replace_all(makers,'-',' ')) %>%
  separate_rows(makers,sep=';') %>%
  mutate(coauthor = str_to_title(trimws(makers)),.keep = "unused") %>%
  mutate(coauthor = case_when(coauthor == 'Sam'~'Sam Learner',
                              .default = coauthor)) %>%
  filter(name != coauthor)
  
#removes duplicate pairs
unique_connections_df <- connections_df %>%
  rowwise() %>%
  mutate(pair_min = min(c(name, coauthor)),
         pair_max = max(c(name, coauthor))) %>%
  ungroup() %>%
  distinct(pair_min, pair_max) %>%
  rename(name = pair_min, coauthor = pair_max)

#joins locations to authors and coauthors
coauthor_df <- unique_connections_df %>%
  left_join(.,locations_df,by="name") %>%
  dplyr::rename(author_location = location_name) %>%
  left_join(.,locations_df,by=c("coauthor"="name")) %>%
  dplyr::rename(coauthor_location = location_name)

write.csv(coauthor_df,"coauthorship_connections.csv")