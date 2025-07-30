library(tidyverse)
library(lubridate)
install.packages("leaflet")
library(leaflet)

NYPD <- read.csv("~/Desktop/NYPD/NYPD/NYPD_Arrest_Data__Year_to_Date_.csv")

# Clean the data & Felony = 1,0 (binary)
clean_NYPD = NYPD %>%   mutate(LAW_CAT_CD = str_trim(as.character(LAW_CAT_CD))) %>% 
                        filter(LAW_CAT_CD %in% c("F","M","V","I")) %>% 
                        mutate(Felony = case_when(
                        LAW_CAT_CD == "F" ~ 1,
                        LAW_CAT_CD != "F" ~ 0),
                        Misdemeanor = case_when(
                        LAW_CAT_CD == "M" ~ 1,
                        LAW_CAT_CD != "M" ~ 0),
                        Violation = case_when(
                        LAW_CAT_CD == "V" ~ 1,
                        LAW_CAT_CD != "V" ~ 0),
                        Infraction = case_when(
                        LAW_CAT_CD == "I" ~ 1,
                        LAW_CAT_CD != "I" ~ 0),
                        PERP_RACE = case_when(
                        PERP_RACE == "AMERICAN INDIAN/ALASKAN NATIVE" ~
                          "AMERICAN INDIAN/\nALASKAN NATIVE",
                        PERP_RACE == "ASIAN / PACIFIC ISLANDER" ~
                          "ASIAN /\n PACIFIC ISLANDER ",
                        PERP_RACE == "BLACK" ~
                          "BLACK",
                        PERP_RACE == "WHITE" ~
                          "WHITE",
                        PERP_RACE == "WHITE HISPANIC" ~
                          "WHITE HISPANIC",
                        PERP_RACE == "BLACK HISPANIC" ~
                          "BLACK HISPANIC",
                        PERP_RACE == "UNKNOWN" ~
                          "UNKNOWN"),
                        ARREST_DATE = mdy(ARREST_DATE),
                        Month = month(ARREST_DATE, label = TRUE),
                        Month = as.factor(Month),
                        weekday = wday(ARREST_DATE, label = TRUE),
                        weekday = as.factor(weekday)
                        ) %>% 
                        select(-ARREST_KEY, -PD_CD, -PD_DESC, -LAW_CODE,
                                    -ARREST_PRECINCT, -X_COORD_CD,
                                    -JURISDICTION_CODE, -Y_COORD_CD,-KY_CD,
                                    -New.Georeferenced.Column)

# felony % for each borough
felony_boro_pct = clean_NYPD %>% 
  group_by(ARREST_BORO) %>% 
  summarise(
    n_total = n(),
    n_felony = sum(Felony),
    pct_felony = (n_felony/n_total)*100
  )

# felony % for each age group
felony_age_pct = clean_NYPD %>% 
  group_by(AGE_GROUP) %>% 
  summarise(
    n_total = n(),
    n_felony = sum(Felony),
    pct_felony = (n_felony/n_total)*100
  )

# felony % for each sex
felony_gender_pct = clean_NYPD %>% 
  group_by(PERP_SEX) %>% 
  summarise(
    n_total = n(),
    n_felony = sum(Felony),
    pct_felony = (n_felony/n_total)*100
  )

# felony % for each race
felony_race_pct = clean_NYPD %>% 
  group_by(PERP_RACE) %>% 
  summarise(
    n_total = n(),
    n_felony = sum(Felony),
    pct_felony = (n_felony/n_total)*100
  )

# plot count of law category
ggplot(data = clean_NYPD, aes(x = LAW_CAT_CD)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of arrest borough
ggplot(data = clean_NYPD, aes(x = ARREST_BORO)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of age group
ggplot(data = clean_NYPD, aes(x = AGE_GROUP)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of sex
ggplot(data = clean_NYPD, aes(x = PERP_SEX)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of race
ggplot(data = clean_NYPD, aes(x = PERP_RACE)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of month
ggplot(data = clean_NYPD, aes(x = Month)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot count of day
ggplot(data = clean_NYPD, aes(x = weekday)) +
  geom_bar() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot felony % for each borough
ggplot(data = felony_boro_pct, aes(x = ARREST_BORO, y = pct_felony)) +
  geom_col(width = 0.1) +
  labs(title = "Felony Percentage for Each Borough",
       x = "Borough",
       y = "Felony(%)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot felony % for age group
ggplot(data = felony_age_pct, aes(x = AGE_GROUP, y = pct_felony)) +
  geom_col(width = 0.1) +
  labs(title = "Felony Percentage for Age Group",
       x = "Age Group",
       y = "Felony(%)") +  
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot felony % for each sex
ggplot(data = felony_gender_pct, aes(x = PERP_SEX, y = pct_felony)) +
  geom_col(width = 0.1) +
  labs(title = "Felony Percentage for Each Sex",
       x = "Sex",
       y = "Felony(%)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank())

# plot felony % for each race
ggplot(data = felony_race_pct, aes(x = PERP_RACE, y = pct_felony)) +
  geom_col(width = 0.1) +
  labs(title = "Felony Percentage for Each Race",
       x = "Race",
       y = "Felony(%)") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 14),
    axis.line.x  = element_line(colour = "black"),
    axis.line.y  = element_line(colour = "black"),
    axis.ticks   = element_line(colour = "black"),
    panel.grid.major = element_blank(),   # strip out grey grid & background
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
  )

# Fit logistic regression
fit = glm(Felony ~  Month + ARREST_BORO + AGE_GROUP + PERP_SEX + PERP_RACE + weekday,
           data = clean_NYPD,
           family = binomial)

summary(fit)

colnames(clean_NYPD)

leaflet(clean_NYPD) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 2, stroke = FALSE,
    fillOpacity = 0.4,
    color = ~ifelse(Felony == 1, "#d62728", "#1f77b4"),
    clusterOptions = markerClusterOptions()
  )

