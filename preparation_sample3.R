# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data preparation - Sample 3
# Date: May 24, 2020
# Author: Florian Keusch
# -----------------------------------------------------------------
# Load required packages
library(foreign)
library(dplyr)
library(purrr)

# -----------------------------------------------------------------
# Get data from https://doi.org/10.4232/1.13043 (ZA6906_v1-0-0.sav)

# Set working directory
# setwd("")

# Load data
sample3 <- read.spss("ZA6906_v1-0-0.sav", to.data.frame=TRUE)

# -----------------------------------------------------------------
# Inspect data
str(sample3)
View(sample3)

# -----------------------------------------------------------------
# Drop non-smartphone owners
table(sample3$AJ32004)
sample3 <- sample3 %>%
  filter(AJ32004 == "1. ja, ein Smartphone")

# -----------------------------------------------------------------
# Rename variables
names(sample3)[names(sample3) == 'id_g'] <- 'id'
names(sample3)[names(sample3) == 'gender_17'] <- 'gender'
names(sample3)[names(sample3) == 'year_of_birth_cat_17'] <- 'age'
names(sample3)[names(sample3) == 'AM32001'] <- 'smartphone_use'
names(sample3)[names(sample3) == 'AM32002_a'] <- 'smartphone_surf'
names(sample3)[names(sample3) == 'AM32002_b'] <- 'smartphone_email'
names(sample3)[names(sample3) == 'AM32002_c'] <- 'smartphone_photo'
names(sample3)[names(sample3) == 'AM32002_d'] <- 'smartphone_sm_look'
names(sample3)[names(sample3) == 'AM32002_e'] <- 'smartphone_sm_post'
names(sample3)[names(sample3) == 'AM32002_f'] <- 'smartphone_shop'
names(sample3)[names(sample3) == 'AM32002_g'] <- 'smartphone_obanking'
names(sample3)[names(sample3) == 'AM32002_h'] <- 'smartphone_inst_apps'
names(sample3)[names(sample3) == 'AM32002_i'] <- 'smartphone_GPS'
names(sample3)[names(sample3) == 'AM32002_j'] <- 'smartphone_bluetooth'
names(sample3)[names(sample3) == 'AM32002_k'] <- 'smartphone_games'
names(sample3)[names(sample3) == 'AM32002_l'] <- 'smartphone_stream'
names(sample3)[names(sample3) == 'AM32003'] <- 'smartphone_skills'
names(sample3)[names(sample3) == 'AM32004'] <- 'privacy_general'
names(sample3)[names(sample3) == 'AM32005'] <- 'concern_websurvey'
names(sample3)[names(sample3) == 'AM32006'] <- 'concern_trackingapp'
names(sample3)[names(sample3) == 'AM32007'] <- 'concern_camera'
names(sample3)[names(sample3) == 'AM32008'] <- 'concern_sensor'
names(sample3)[names(sample3) == 'AM32009'] <- 'concern_gps'
names(sample3)[names(sample3) == 'educ_school_17'] <- 'edu_school'

# -----------------------------------------------------------------
# Keep only relevant variables
sample3 <- select(sample3, id, gender, age, smartphone_use,
                  smartphone_surf, smartphone_email, smartphone_photo,
                  smartphone_sm_look, smartphone_sm_post, smartphone_shop,
                  smartphone_obanking, smartphone_inst_apps,
                  smartphone_GPS, smartphone_bluetooth, smartphone_games,
                  smartphone_stream, smartphone_skills, privacy_general,
                  concern_websurvey, concern_trackingapp, concern_camera,
                  concern_sensor, concern_gps, edu_school)

# -----------------------------------------------------------------
# Make -90's in NA's
sample3[sample3 == "-90. item nonresponse"] <- NA

#Drop unused factor levels
sample3 <- droplevels(sample3)

# -----------------------------------------------------------------
# Recode values
table(sample3$gender)
levels(sample3$gender) <- c("male","female")

table(sample3$age)
levels(sample3$age) <- c("1935-1939", "1940-1944", "1945-1949", "1950-1954",
                         "1955-1959", "1960-1964", "1965-1969", "1970-1974",
                         "1975-1979", "1980-1984", "1985-1989", "1990-1994",
                         "1995-1999")

table(sample3$smartphone_use)
levels(sample3$smartphone_use) <- c("Several times a day",
                                    "Every day",
                                    "Several times a week",
                                    "Several times a month",
                                    "Once a month or less")

table(sample3$smartphone_surf)
table(sample3$smartphone_email)
table(sample3$smartphone_photo)
table(sample3$smartphone_sm_look)
table(sample3$smartphone_sm_post)
table(sample3$smartphone_shop)
table(sample3$smartphone_obanking)
table(sample3$smartphone_inst_apps)
table(sample3$smartphone_GPS)
table(sample3$smartphone_bluetooth)
table(sample3$smartphone_games)
table(sample3$smartphone_stream)

make.factor.yn <- function(x) {
  levels(x) <- c("No", "Yes")
  x
}

sample3[5:16] <- map(sample3[5:16], make.factor.yn)

table(sample3$smartphone_skills)
levels(sample3$smartphone_skills) <- c("Beginner", "2", "3", "4", "Advanced")

table(sample3$privacy_general)
table(sample3$concern_websurvey)
table(sample3$concern_trackingapp)
table(sample3$concern_camera)
table(sample3$concern_sensor)
table(sample3$concern_gps)

make.factor.14 <- function(x) {
  levels(x) = c("not at all concerned",
                "a little concerned",
                "somewhat concerned",
                "a lot concerned")
  x
}

sample3[18:23] <- map(sample3[18:23], make.factor.14)

# -----------------------------------------------------------------
# Create new age and edu variable
age3 <- NA
age3 <- factor(age3,
              levels = c('<33', '33-52', '53+'))
age3[sample3$age== '1995-1999' | sample3$age == '1990-1994' | sample3$age == '1985-1989'] <- '<33'
age3[sample3$age== '1980-1984' | sample3$age == '1975-1979' | sample3$age == '1970-1974' | sample3$age == '1965-1969'] <- '33-52'
age3[sample3$age== '1960-1964' | sample3$age == '1955-1959' | sample3$age == '1950-1954' | sample3$age == '1945-1949' |
       sample3$age== '1940-1944' | sample3$age == '1935-1939'] <- '53+'
sample3$age <- age3

summary(sample3$edu_school)
edu <- NA
edu <- factor(edu, levels = c("with hs", "without hs"))
edu[sample3$edu == "1. Noch Schüler/-in" |
      sample3$edu == "2. Schule beendet ohne Abschluss" |
      sample3$edu == "3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse" |
      sample3$edu == "4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse"]  <- "without hs"
edu[sample3$edu == "5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)" |
      sample3$edu == "6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)"] <- "with hs"
sample3$edu_school <- edu

# -----------------------------------------------------------------
# Create variable for no. of different smartphone activities 
sample3$no_smartphone_act <- rowSums(sample3[5:16] == "Yes", na.rm = T)

# -----------------------------------------------------------------
# Dichotomize concern variables
dichotomize.concern <- function(x) {
  y <- NA
  y <- factor(y, levels = c('Low', 'High'))
  y[x == "not at all concerned" | x == "a little concerned"] <- "Low"
  y[x == "somewhat concerned" | x == "a lot concerned"] <- "High"
  return(y)
}

z <- map(sample3[18:23], dichotomize.concern)

names(z) <- paste0(names(sample3[18:23]), "_di")

sample3 <- cbind(sample3, z)

create.table <- function(x) {
  prop.table(table(x))
}

map(z, summary)
map(z, create.table)

# -----------------------------------------------------------------
# Dichotomized smartphone use
sample3$smartphone_use_di <- NA
sample3$smartphone_use_di <- factor(sample3$smartphone_use_di,
                                    levels = c("Less then every day", "Every day"))
sample3$smartphone_use_di[sample3$smartphone_use == "Several times a day" |
                            sample3$smartphone_use == "Every day"] <- "Every day"
sample3$smartphone_use_di[sample3$smartphone_use == "Several times a week" |
                            sample3$smartphone_use == "Several times a month" |
                            sample3$smartphone_use == "Once a month or less"] <- "Less then every day"
summary(sample3$smartphone_use_di)
prop.table(table(sample3$smartphone_use_di))

# -----------------------------------------------------------------
# Dichotomized smartphone skills
sample3$smartphone_skills_di <- NA
sample3$smartphone_skills_di <- factor(sample3$smartphone_skills_di,
                                       levels = c("Low", "High"))
sample3$smartphone_skills_di[sample3$smartphone_skills == "Beginner" |
                               sample3$smartphone_skills == "2" |
                               sample3$smartphone_skills == "3"] <- "Low"
sample3$smartphone_skills_di[sample3$smartphone_skills == "4" |
                               sample3$smartphone_skills == "Advanced"] <- "High"
summary(sample3$smartphone_skills_di)
prop.table(table(sample3$smartphone_skills_di))

# -----------------------------------------------------------------
# Save data set
write.csv(sample3, file = "sample3.csv", row.names=FALSE)