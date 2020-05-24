# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data preparation - Sample 2
# Date: May 24, 2020
# Author: Florian Keusch
# -----------------------------------------------------------------
# Load required packages
library(dplyr)
library(purrr)

# -----------------------------------------------------------------
# Get data from https://doi.org/10.4232/1.13247 (ZA6978_v1-0-0.csv)

# Set working directory
# setwd("")

# Load data
sample2 <- read.csv("ZA6978_v1-0-0.csv")

# -----------------------------------------------------------------
# Inspect data
str(sample2)
View(sample2)

# -----------------------------------------------------------------
# Rename variables
names(sample2)[names(sample2) == 'lfdn'] <- 'id'
names(sample2)[names(sample2) == 'v_99'] <- 'gender'
names(sample2)[names(sample2) == 'v_101'] <- 'age'
names(sample2)[names(sample2) == 'v_65'] <- 'smartphone_use'
names(sample2)[names(sample2) == 'v_18'] <- 'smartphone_surf'
names(sample2)[names(sample2) == 'v_19'] <- 'smartphone_email'
names(sample2)[names(sample2) == 'v_20'] <- 'smartphone_photo'
names(sample2)[names(sample2) == 'v_21'] <- 'smartphone_sm_look'
names(sample2)[names(sample2) == 'v_22'] <- 'smartphone_sm_post'
names(sample2)[names(sample2) == 'v_23'] <- 'smartphone_shop'
names(sample2)[names(sample2) == 'v_24'] <- 'smartphone_obanking'
names(sample2)[names(sample2) == 'v_25'] <- 'smartphone_inst_apps'
names(sample2)[names(sample2) == 'v_26'] <- 'smartphone_GPS'
names(sample2)[names(sample2) == 'v_27'] <- 'smartphone_bluetooth'
names(sample2)[names(sample2) == 'v_28'] <- 'smartphone_games'
names(sample2)[names(sample2) == 'v_29'] <- 'smartphone_stream'
names(sample2)[names(sample2) == 'v_66'] <- 'smartphone_skills'
names(sample2)[names(sample2) == 'v_45'] <- 'privacy_general'
names(sample2)[names(sample2) == 'v_88'] <- 'concern_websurvey'
names(sample2)[names(sample2) == 'v_90'] <- 'concern_trackingapp'
names(sample2)[names(sample2) == 'v_92'] <- 'concern_camera'
names(sample2)[names(sample2) == 'v_93'] <- 'concern_sensor'
names(sample2)[names(sample2) == 'v_94'] <- 'concern_gps'
names(sample2)[names(sample2) == 'v_62'] <- 'edu_school'

# -----------------------------------------------------------------
# Keep only relevant variables
sample2 <- select(sample2, id, gender, age, smartphone_use,
                  smartphone_surf, smartphone_email, smartphone_photo,
                  smartphone_sm_look, smartphone_sm_post, smartphone_shop,
                  smartphone_obanking, smartphone_inst_apps,
                  smartphone_GPS, smartphone_bluetooth, smartphone_games,
                  smartphone_stream, smartphone_skills, privacy_general,
                  concern_websurvey, concern_trackingapp, concern_camera,
                  concern_sensor, concern_gps, edu_school,
                  participant_device, duration)

# -----------------------------------------------------------------
# Recode device variable
table(sample2$participant_device)
sample2$participant_device <- factor(sample2$participant_device,
                                     levels = c(0, 1, 2, 3),
                                     labels = c("0" = "desktop",
                                                "1" = "smartphone",
                                                "2" = "tablet",
                                                "3" = "unknown"))

# -----------------------------------------------------------------
# Recode values
table(sample2$gender)
sample2$gender <- factor(sample2$gender,
                         levels = c(1, 2),
                         labels = c("1"="male", "2"="female"))

table(sample2$age)
sample2$age <- factor(sample2$age,
                      levels = c(2, 3, 4, 5, 6, 7),
                      labels = c("2" = "18-19",
                                 "3" = "20-29",
                                 "4" = "30-39",
                                 "5" = "40-49",
                                 "6" = "50-59",
                                 "7" = "60+"))

table(sample2$smartphone_use)
sample2$smartphone_use <- factor(sample2$smartphone_use,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("1" = "Several times a day",
                                            "2" = "Every day",
                                            "3" = "Several times a week",
                                            "4" = "Several times a month",
                                            "5" = "Once a month or less"))

table(sample2$smartphone_surf)
table(sample2$smartphone_email)
table(sample2$smartphone_photo)
table(sample2$smartphone_sm_look)
table(sample2$smartphone_sm_post)
table(sample2$smartphone_shop)
table(sample2$smartphone_obanking)
table(sample2$smartphone_inst_apps)
table(sample2$smartphone_GPS)
table(sample2$smartphone_bluetooth)
table(sample2$smartphone_games)
table(sample2$smartphone_stream)

make.factor.yn <- function(x) {
  x <- factor(x,
              levels = c(1, 2),
              labels = c("1" = "Yes",
                         "2" = "No"))
  x
}

sample2[5:16] <- map(sample2[5:16], make.factor.yn)

table(sample2$smartphone_skills)
sample2$smartphone_skills <- factor(sample2$smartphone_skills,
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("1" = "Beginner",
                                               "2" = "2",
                                               "3" = "3",
                                               "4" = "4",
                                               "5" = "Advanced"))

table(sample2$privacy_general)
table(sample2$concern_websurvey)
table(sample2$concern_trackingapp)
table(sample2$concern_camera)
table(sample2$concern_sensor)
table(sample2$concern_gps)

make.factor.14 <- function(x) {
  x <- factor(x,
              levels = c(1, 2, 3, 4),
              labels = c("1" = "not at all concerned",
                         "2" = "a little concerned",
                         "3" = "somewhat concerned",
                         "4" = "a lot concerned"))
  x
}

sample2[18:23] <- map(sample2[18:23], make.factor.14)

# -----------------------------------------------------------------
# Create new age and edu variable
summary(sample2$age)
age3 <- NA
age3 <- factor(age3,
               levels = c('<30', '30-49', '50+'))
age3[sample2$age == "18-19" | sample2$age == "20-29"] <- '<30'
age3[sample2$age == "30-39" | sample2$age == "40-49"] <- '30-49'
age3[sample2$age == "50-59" | sample2$age == "60+"] <- '50+'
sample2$age <- age3

summary(sample2$edu_school)
edu <- NA
edu <- factor(edu, levels = c("with hs", "without hs"))
edu[sample2$edu_school == 1 | sample2$edu_school == 2 |
      sample2$edu_school == 3 | sample2$edu_school == 4] <- "without hs"
edu[sample2$edu_school == 5 | sample2$edu_school == 6] <- "with hs"
sample2$edu_school <- edu

# -----------------------------------------------------------------
# Create variable for no. of different smartphone activities 
sample2$no_smartphone_act <- rowSums(sample2[5:16] == "Yes", na.rm = T)

# -----------------------------------------------------------------
# Dichotomize concern variables
dichotomize.concern <- function(x) {
  y <- NA
  y <- factor(y, levels = c('Low', 'High'))
  y[x == "not at all concerned" | x == "a little concerned"] <- "Low"
  y[x == "somewhat concerned" | x == "a lot concerned"] <- "High"
  return(y)
}

z <- map(sample2[18:23], dichotomize.concern)

names(z) <- paste0(names(sample2[18:23]), "_di")

sample2 <- cbind(sample2, z)

create.table <- function(x) {
  prop.table(table(x))
}

map(z, summary)
map(z, create.table)

# -----------------------------------------------------------------
# Dichotomized smartphone use
sample2$smartphone_use_di <- NA
sample2$smartphone_use_di <- factor(sample2$smartphone_use_di,
                                    levels = c("Less then every day", "Every day"))
sample2$smartphone_use_di[sample2$smartphone_use == "Several times a day" |
                            sample2$smartphone_use == "Every day"] <- "Every day"
sample2$smartphone_use_di[sample2$smartphone_use == "Several times a week" |
                            sample2$smartphone_use == "Several times a month" |
                            sample2$smartphone_use == "Once a month or less"] <- "Less then every day"
summary(sample2$smartphone_use_di)
prop.table(table(sample2$smartphone_use_di))

# -----------------------------------------------------------------
# Dichotomized smartphone skills
sample2$smartphone_skills_di <- NA
sample2$smartphone_skills_di <- factor(sample2$smartphone_skills_di,
                                       levels = c("Low", "High"))
sample2$smartphone_skills_di[sample2$smartphone_skills == "Beginner" |
                               sample2$smartphone_skills == "2" |
                               sample2$smartphone_skills == "3"] <- "Low"
sample2$smartphone_skills_di[sample2$smartphone_skills == "4" |
                               sample2$smartphone_skills == "Advanced"] <- "High"
summary(sample2$smartphone_skills_di)
prop.table(table(sample2$smartphone_skills_di))

# -----------------------------------------------------------------
# Save data set
write.csv(sample2, file = "sample2.csv", row.names=FALSE)
