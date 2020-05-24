# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data preparation - Sample 1
# Date: May 24, 2020
# Author: Florian Keusch
# -----------------------------------------------------------------
# Load required packages
library(dplyr)
library(purrr)

# -----------------------------------------------------------------
# Get data from https://doi.org/10.4232/1.13246
# Only Wave 1 data set (ZA6977_W1_v1-0-0.csv)

# Set working directory
# setwd("")

# Load data
sample1 <- read.csv("WTP_W1.csv")

# -----------------------------------------------------------------
#Inspect data
str(sample1)
View(sample1)

# -----------------------------------------------------------------
#Rename variables
names(sample1)[names(sample1) == 'lfdn'] <- 'id'
names(sample1)[names(sample1) == 'v_99'] <- 'gender'
names(sample1)[names(sample1) == 'v_101'] <- 'age'
names(sample1)[names(sample1) == 'v_65'] <- 'smartphone_use'
names(sample1)[names(sample1) == 'v_18'] <- 'smartphone_surf'
names(sample1)[names(sample1) == 'v_19'] <- 'smartphone_email'
names(sample1)[names(sample1) == 'v_20'] <- 'smartphone_photo'
names(sample1)[names(sample1) == 'v_21'] <- 'smartphone_sm_look'
names(sample1)[names(sample1) == 'v_22'] <- 'smartphone_sm_post'
names(sample1)[names(sample1) == 'v_23'] <- 'smartphone_shop'
names(sample1)[names(sample1) == 'v_24'] <- 'smartphone_obanking'
names(sample1)[names(sample1) == 'v_25'] <- 'smartphone_inst_apps'
names(sample1)[names(sample1) == 'v_26'] <- 'smartphone_GPS'
names(sample1)[names(sample1) == 'v_27'] <- 'smartphone_bluetooth'
names(sample1)[names(sample1) == 'v_28'] <- 'smartphone_games'
names(sample1)[names(sample1) == 'v_29'] <- 'smartphone_stream'
names(sample1)[names(sample1) == 'v_66'] <- 'smartphone_skills'
names(sample1)[names(sample1) == 'v_45'] <- 'privacy_general'
names(sample1)[names(sample1) == 'v_88'] <- 'concern_websurvey'
names(sample1)[names(sample1) == 'v_90'] <- 'concern_trackingapp'
names(sample1)[names(sample1) == 'v_92'] <- 'concern_camera'
names(sample1)[names(sample1) == 'v_93'] <- 'concern_sensor'
names(sample1)[names(sample1) == 'v_94'] <- 'concern_gps'
names(sample1)[names(sample1) == 'v_62'] <- 'edu_school'

# -----------------------------------------------------------------
# Keep only relevant variables
sample1 <- select(sample1, id, gender, age, smartphone_use,
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
table(sample1$participant_device)
sample1$participant_device <- factor(sample1$participant_device,
                                     levels = c(0, 1, 2, 3),
                                     labels = c("0" = "desktop",
                                                "1" = "smartphone",
                                                "2" = "tablet",
                                                "3" = "unknown"))

# -----------------------------------------------------------------
# Recode other variables
table(sample1$gender)
sample1$gender <- factor(sample1$gender,
                         levels = c(1, 2),
                         labels = c("1"="male", "2"="female"))
                         
table(sample1$age)
sample1$age <- factor(sample1$age,
                      levels = c(2, 3, 4, 5, 6, 7),
                      labels = c("2" = "18-19",
                                 "3" = "20-29",
                                 "4" = "30-39",
                                 "5" = "40-49",
                                 "6" = "50-59",
                                 "7" = "60+"))
summary(sample1$age)

table(sample1$smartphone_use)
sample1$smartphone_use <- factor(sample1$smartphone_use,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("1" = "Several times a day",
                                            "2" = "Every day",
                                            "3" = "Several times a week",
                                            "4" = "Several times a month",
                                            "5" = "Once a month or less"))
summary(sample1$smartphone_use)

table(sample1$smartphone_surf)
table(sample1$smartphone_email)
table(sample1$smartphone_photo)
table(sample1$smartphone_sm_look)
table(sample1$smartphone_sm_post)
table(sample1$smartphone_shop)
table(sample1$smartphone_obanking)
table(sample1$smartphone_inst_apps)
table(sample1$smartphone_GPS)
table(sample1$smartphone_bluetooth)
table(sample1$smartphone_games)
table(sample1$smartphone_stream)

make.factor.yn <- function(x) {
  x <- factor(x,
              levels = c(1, 2),
              labels = c("1" = "Yes",
                         "2" = "No"))
  x
}

sample1[5:16] <- map(sample1[5:16], make.factor.yn)

table(sample1$smartphone_skills)
sample1$smartphone_skills <- factor(sample1$smartphone_skills,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("1" = "Beginner",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4",
                                            "5" = "Advanced"))
summary(sample1$smartphone_skills)

table(sample1$privacy_general)
table(sample1$concern_websurvey)
table(sample1$concern_trackingapp)
table(sample1$concern_camera)
table(sample1$concern_sensor)
table(sample1$concern_gps)

make.factor.14 <- function(x) {
  x <- factor(x,
              levels = c(1, 2, 3, 4),
              labels = c("1" = "not at all concerned",
                         "2" = "a little concerned",
                         "3" = "somewhat concerned",
                         "4" = "a lot concerned"))
  x
}

sample1[18:23] <- map(sample1[18:23], make.factor.14)

# -----------------------------------------------------------------
# Create new age and edu variable
age3 <- NA
age3 <- factor(age3,
              levels = c('<30', '30-49', '50+'))
age3[sample1$age== '18-19' | sample1$age == '20-29'] <- '<30'
age3[sample1$age== '30-39' | sample1$age == '40-49'] <- '30-49'
age3[sample1$age== '50-59' | sample1$age == '60+'] <- '50+'
sample1$age <- age3

edu <- NA
edu <- factor(edu,
              levels = c("with hs", "without hs"))
edu[sample1$edu_school == 1 | sample1$edu_school == 2 |
      sample1$edu_school == 3 | sample1$edu_school == 4] <- "without hs"
edu[sample1$edu_school == 5 | sample1$edu_school == 6] <- "with hs"
sample1$edu_school <- edu

# -----------------------------------------------------------------
# Create variable for no. of different smartphone activities 
sample1$no_smartphone_act <- rowSums(sample1[5:16] == "Yes", na.rm = T)

# -----------------------------------------------------------------
# Dichotomize concern variables
dichotomize.concern <- function(x) {
  y <- NA
  y <- factor(y, levels = c('Low', 'High'))
  y[x == "not at all concerned" | x == "a little concerned"] <- "Low"
  y[x == "somewhat concerned" | x == "a lot concerned"] <- "High"
  return(y)
}

z <- map(sample1[18:23], dichotomize.concern)

names(z) <- paste0(names(sample1[18:23]), "_di")

sample1 <- cbind(sample1, z)

create.table <- function(x) {
  prop.table(table(x))
}

map(z, summary)
map(z, create.table)

# -----------------------------------------------------------------
# Dichotomized smartphone use
sample1$smartphone_use_di <- NA
sample1$smartphone_use_di <- factor(sample1$smartphone_use_di,
                                    levels = c("Less then every day", "Every day"))
sample1$smartphone_use_di[sample1$smartphone_use == "Several times a day" |
                            sample1$smartphone_use == "Every day"] <- "Every day"
sample1$smartphone_use_di[sample1$smartphone_use == "Several times a week" |
                            sample1$smartphone_use == "Several times a month" |
                            sample1$smartphone_use == "Once a month or less"] <- "Less then every day"
summary(sample1$smartphone_use_di)
prop.table(table(sample1$smartphone_use_di))

# -----------------------------------------------------------------
# Dichotomized smartphone skills
sample1$smartphone_skills_di <- NA
sample1$smartphone_skills_di <- factor(sample1$smartphone_skills_di,
                                       levels = c("Low", "High"))
sample1$smartphone_skills_di[sample1$smartphone_skills == "Beginner" |
                               sample1$smartphone_skills == "2" |
                               sample1$smartphone_skills == "3"] <- "Low"
sample1$smartphone_skills_di[sample1$smartphone_skills == "4" |
                               sample1$smartphone_skills == "Advanced"] <- "High"
summary(sample1$smartphone_skills_di)
prop.table(table(sample1$smartphone_skills_di))

# -----------------------------------------------------------------
# Save data set
write.csv(sample1, file = "sample1x.csv", row.names=FALSE)