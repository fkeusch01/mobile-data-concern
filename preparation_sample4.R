# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data preparation - Sample 4
# Date: May 24, 2020
# Author: Florian Keusch
# -----------------------------------------------------------------
# Load required packages
library(foreign)
library(dplyr)
library(purrr)

# -----------------------------------------------------------------
# Get data from https://doi.org/10.11587/NHWVOO (10038_da_de_v1_0.zsav)

# Set working directory
# setwd("")

# Load data
sample4 <- read.spss("10038_da_de_v1_0.zsav", to.data.frame=TRUE)

# -----------------------------------------------------------------
# Inspect data
str(sample4)
View(sample4)

# -----------------------------------------------------------------
#Drop non-smartphone owners
table(sample4$P5S2M302)
sample4 <- sample4 %>%
  filter(P5S2M302 == "ja, ein Smartphone ")

# -----------------------------------------------------------------
# Rename variables
names(sample4)[names(sample4) == 'lfnr'] <- 'id'
names(sample4)[names(sample4) == 'P5SD101'] <- 'gender'
names(sample4)[names(sample4) == 'P5SD102'] <- 'age'
names(sample4)[names(sample4) == 'P5S2M303'] <- 'smartphone_use'
names(sample4)[names(sample4) == 'P5S2M304_A1'] <- 'smartphone_surf'
names(sample4)[names(sample4) == 'P5S2M304_A2'] <- 'smartphone_email'
names(sample4)[names(sample4) == 'P5S2M304_A3'] <- 'smartphone_photo'
names(sample4)[names(sample4) == 'P5S2M304_A4'] <- 'smartphone_sm_look'
names(sample4)[names(sample4) == 'P5S2M304_A5'] <- 'smartphone_sm_post'
names(sample4)[names(sample4) == 'P5S2M304_A6'] <- 'smartphone_shop'
names(sample4)[names(sample4) == 'P5S2M304_A7'] <- 'smartphone_obanking'
names(sample4)[names(sample4) == 'P5S2M304_A8'] <- 'smartphone_inst_apps'
names(sample4)[names(sample4) == 'P5S2M304_A9'] <- 'smartphone_GPS'
names(sample4)[names(sample4) == 'P5S2M304_A10'] <- 'smartphone_bluetooth'
names(sample4)[names(sample4) == 'P5S2M304_A11'] <- 'smartphone_games'
names(sample4)[names(sample4) == 'P5S2M304_A12'] <- 'smartphone_stream'
names(sample4)[names(sample4) == 'P5S2M305'] <- 'smartphone_skills'
names(sample4)[names(sample4) == 'P5S2M307'] <- 'privacy_general'
names(sample4)[names(sample4) == 'P5S2M308'] <- 'concern_websurvey'
names(sample4)[names(sample4) == 'P5S2M309'] <- 'concern_trackingapp'
names(sample4)[names(sample4) == 'P5S2M310'] <- 'concern_camera'
names(sample4)[names(sample4) == 'P5S2M311'] <- 'concern_sensor'
names(sample4)[names(sample4) == 'P5S2M312'] <- 'concern_gps'
names(sample4)[names(sample4) == 'P5SD103'] <- 'edu_school'

# -----------------------------------------------------------------
# Selecting only relevant variables
sample4 <- select(sample4, id, gender, age, smartphone_use,
                  smartphone_surf, smartphone_email, smartphone_photo,
                  smartphone_sm_look, smartphone_sm_post, smartphone_shop,
                  smartphone_obanking, smartphone_inst_apps,
                  smartphone_GPS, smartphone_bluetooth, smartphone_games,
                  smartphone_stream, smartphone_skills, privacy_general,
                  concern_websurvey, concern_trackingapp, concern_camera,
                  concern_sensor, concern_gps, edu_school)

# -----------------------------------------------------------------
# Make missing values in NA's
sample4[sample4 == -4 | sample4 == -3 | sample4 == -2 | sample4 == -1] <- NA

# -----------------------------------------------------------------
# Rename factor levels to English
table(sample4$gender)
levels(sample4$gender) <- c("male","female")

table(sample4$smartphone_use)
levels(sample4$smartphone_use) <- c("Several times a day", "Every day",
                                   "Several times a week", "Several times a month",
                                   "Once a month or less")

table(sample4$smartphone_surf)
table(sample4$smartphone_email)
table(sample4$smartphone_photo)
table(sample4$smartphone_sm_look)
table(sample4$smartphone_sm_post)
table(sample4$smartphone_shop)
table(sample4$smartphone_obanking)
table(sample4$smartphone_inst_apps)
table(sample4$smartphone_GPS)
table(sample4$smartphone_bluetooth)
table(sample4$smartphone_games)
table(sample4$smartphone_stream)

make.factor.yn <- function(x) {
  levels(x) <- c("Yes", "No")
  x
}

sample4[5:16] <- map(sample4[5:16], make.factor.yn)

table(sample4$smartphone_skills)
levels(sample4$smartphone_skills) <- c("Beginner", "2", "3", "4", "Advanced")

table(sample4$privacy_general)
table(sample4$concern_websurvey)
table(sample4$concern_trackingapp)
table(sample4$concern_camera)
table(sample4$concern_sensor)
table(sample4$concern_gps)

make.factor.14 <- function(x) {
  levels(x) = c("not at all concerned",
                "a little concerned",
                "somewhat concerned",
                "a lot concerned")
  x
}

sample4[18:23] <- map(sample4[18:23], make.factor.14)

# -----------------------------------------------------------------
# Create new age and edu variable
summary(sample4$age)
sample4$age <- as.numeric(levels(sample4$age))[sample4$age]
age3 <- NA
age3 <- factor(age3,
              levels = c('<30', '30-49', '50+'))
age3[sample4$age < 30] <- '<30'
age3[sample4$age > 29 & sample4$age < 50] <- '30-49'
age3[sample4$age > 49] <- '50+'
sample4$age <- age3

summary(sample4$edu_school)
edu <- NA
edu <- factor(edu, levels = c("with hs", "without hs"))
edu <- ifelse(sample4$edu_school == "Doktor / PhD" |
    sample4$edu_school == "Magister / Master / Diplomingenieur / Fachhochschule" |
    sample4$edu_school == "Bachelor" |
    sample4$edu_school == "Kollege" |
    sample4$edu_school == "Hochschulverwandte Lehranstalt (berufsbildende oder pädagogische Akademie, z.B. Gesundheits- und Sozialakademien" |
    sample4$edu_school == "BHS mit Matura (z.B. HTL, HAK, HBLA, BAKIPÄD)" |
    sample4$edu_school == "AHS mit Matura",
    "with hs", "without hs")
sample4$edu_school <- edu

# -----------------------------------------------------------------
# Create variable for no. of different smartphone activities 
sample4$no_smartphone_act <- rowSums(sample4[5:16] == "Yes", na.rm = T)

# -----------------------------------------------------------------
# Dichotomize concern variables
dichotomize.concern <- function(x) {
  y <- NA
  y <- factor(y, levels = c('Low', 'High'))
  y[x == "not at all concerned" | x == "a little concerned"] <- "Low"
  y[x == "somewhat concerned" | x == "a lot concerned"] <- "High"
  return(y)
}

z <- map(sample4[18:23], dichotomize.concern)

names(z) <- paste0(names(sample4[18:23]), "_di")

sample4 <- cbind(sample4, z)

create.table <- function(x) {
  prop.table(table(x))
}

map(z, summary)
map(z, create.table)

# -----------------------------------------------------------------
# Dichotomized smartphone use
sample4$smartphone_use_di <- NA
sample4$smartphone_use_di <- factor(sample4$smartphone_use_di,
                                    levels = c("Less then every day", "Every day"))
sample4$smartphone_use_di[sample4$smartphone_use == "Several times a day" |
                            sample4$smartphone_use == "Every day"] <- "Every day"
sample4$smartphone_use_di[sample4$smartphone_use == "Several times a week" |
                            sample4$smartphone_use == "Several times a month" |
                            sample4$smartphone_use == "Once a month or less"] <- "Less then every day"
summary(sample4$smartphone_use_di)
prop.table(table(sample4$smartphone_use_di))

# -----------------------------------------------------------------
# Dichotomized smartphone skills
sample4$smartphone_skills_di <- NA
sample4$smartphone_skills_di <- factor(sample4$smartphone_skills_di,
                                       levels = c("Low", "High"))
sample4$smartphone_skills_di[sample4$smartphone_skills == "Beginner" |
                               sample4$smartphone_skills == "2" |
                               sample4$smartphone_skills == "3"] <- "Low"
sample4$smartphone_skills_di[sample4$smartphone_skills == "4" |
                               sample4$smartphone_skills == "Advanced"] <- "High"
summary(sample4$smartphone_skills_di)
prop.table(table(sample4$smartphone_skills_di))

# -----------------------------------------------------------------
# Save data set
write.csv(sample4, file = "sample4.csv", row.names=FALSE)