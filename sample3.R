# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data analysis - Sample 3
# Date: May 24, 2020
# Author: Florian Keusch
# -----------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(ggplot2)
library(margins)

# -----------------------------------------------------------------
# Set working directory
# setwd("")

# Load data
sample3 <- read.csv("sample3.csv", stringsAsFactors = T)

# -----------------------------------------------------------------
#Inspect data
str(sample3)
View(sample3)

# -----------------------------------------------------------------
#Response time not available

#Proportion of smartphone completes not available

# -----------------------------------------------------------------
# Descriptives - Table 22.1 Sample 3
# Concern
summary(sample3$concern_websurvey_di)
sample3$concern_websurvey_di <- factor(sample3$concern_websurvey_di,
                                       levels = c("Low", "High"))
prop.table(table(sample3$concern_websurvey_di))

summary(sample3$concern_trackingapp_di)
sample3$concern_trackingapp_di <- factor(sample3$concern_trackingapp_di,
                                         levels = c("Low", "High"))
prop.table(table(sample3$concern_trackingapp_di))

summary(sample3$concern_camera_di)
sample3$concern_camera_di <- factor(sample3$concern_camera_di,
                                    levels = c("Low", "High"))
prop.table(table(sample3$concern_camera_di))

summary(sample3$concern_sensor_di)
sample3$concern_sensor_di <- factor(sample3$concern_sensor_di,
                                    levels = c("Low", "High"))
prop.table(table(sample3$concern_sensor_di))

summary(sample3$concern_gps_di)
sample3$concern_gps_di <- factor(sample3$concern_gps_di,
                                 levels = c("Low", "High"))
prop.table(table(sample3$concern_gps_di))

# Frequency of smartphone use
summary(sample3$smartphone_use_di)
sample3$smartphone_use_di <- factor(sample3$smartphone_use_di,
                                    levels = c("Less then every day",
                                               "Every day"))
prop.table(table(sample3$smartphone_use_di))

# Number of smartphone activities
summary(sample3$no_smartphone_act)
sd(sample3$no_smartphone_act)

# Smartphone skills
summary(sample3$smartphone_skills_di)
sample3$smartphone_skills_di <- factor(sample3$smartphone_skills_di,
                                       levels = c("Low", "High"))
prop.table(table(sample3$smartphone_skills_di))

# General privacy concern
summary(sample3$privacy_general_di)
sample3$privacy_general_di <- factor(sample3$privacy_general_di,
                                     levels = c("Low", "High"))
prop.table(table(sample3$privacy_general_di))

# Socio-demographics
summary(sample3$gender)
sample3$gender <- factor(sample3$gender, levels = c("male", "female"))
prop.table(table(sample3$gender))

summary(sample3$age)
prop.table(table(sample3$age))

summary(sample3$edu_school)
prop.table(table(sample3$edu_school))

# -----------------------------------------------------------------
# Figure 22.1: Concern
# Create data frame that includes % of concern
create.data.frame.table <- function(x) {
  a <- as.data.frame(prop.table(table(x)))
  return(a)
}

sample3_concern <- map_df(sample3[19:23], create.data.frame.table)
sample3_concern$type <- factor(rep(1:5, each = 4))
levels(sample3_concern$type) <- c("Online survey", "Smartphone usage", "Camera",
                                  "Activity data", "GPS")
sample3_concern$type <- factor(sample3_concern$type,
                               levels = c("Smartphone usage", "GPS",
                                          "Activity data", "Camera",
                                          "Online survey"))
names(sample3_concern) <- c("concern", "freq", "type")
sample3_concern$concern <- factor(sample3_concern$concern,
                                  levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))
sample3_concern$freq <- sample3_concern$freq * 100
sample3_concern$sample <- factor("Sample 3")
sample3_concern

#Create figure
ggplot(sample3_concern, aes(x = type, y = freq, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample3_concern$type))) +
  scale_fill_manual(values = c("#458B00", "yellow", "orange", "#FF0000")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = rel(2), colour = "black"),
        axis.text.x = element_text(size = rel(2), colour = "black"),
        legend.text = element_text(size = rel(1.5)),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))

# -----------------------------------------------------------------
# Logistic regression predicting concern
log.regression <- function(x) {
  y <- glm(x ~ privacy_general_di + smartphone_use_di + smartphone_skills_di + no_smartphone_act +
             age + gender + edu_school, data = sample3, family = "binomial")
  y
}

sample3_logit <- map(sample3[27:31],log.regression)

log.regression.result <- function(y) {
  print(summary(y))
  print(with(y, null.deviance - deviance))
  print(with(y, df.null - df.residual))
  print(with(y, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
}

map(sample3_logit,log.regression.result)

# Calculate AMEs
sample3_AME <- as.data.frame(map_df(sample3_logit, function(x) {
  summary(margins(x))
}
))

sample3_AME$concern <- as.factor(rep(1:5, each = 8))
levels(sample3_AME$concern) <- c("Online Survey",
                                 "Smartphone \nusage",
                                 "Camera",
                                 "Activity data",
                                 "GPS")
sample3_AME$sample <- as.factor("Sample 3")
sample3_AME$factor <- factor(sample3_AME$factor,
                             levels = c("edu_schoolwithout hs",
                                        "age53+",
                                        "age33-52",
                                        "genderfemale",
                                        "privacy_general_diHigh",
                                        "no_smartphone_act",
                                        "smartphone_skills_diHigh",
                                        "smartphone_use_diEvery day"))

levels(sample3_AME$factor) <- c("Without HS degree",
                                "50 years and older",
                                "30-49 years",
                                "Female",
                                "High general privacy concern",
                                "No. of smartphone activities",
                                "High smartphone skills",
                                "Use smartphone every day")

# Plot AMEs
ggplot(sample3_AME, aes(y = AME, x = factor)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2, size = 0.5) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sample 3") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = rel(1.5), colour = "black"),
        axis.text.x = element_text(size = rel(1.5), colour = "black"),
        axis.title.x = element_text( size = rel(1.2), color = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = rel(1.5), face="bold", color = "black"),
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5)) +
  facet_wrap(~ concern, ncol = 5)

# -----------------------------------------------------------------
# Analyzing effect of no. of smartphone activities
# Make no. of smartphone activities into factor
sample3$no_smartphone_act_factor <- as.factor(sample3$no_smartphone_act)
summary(sample3$no_smartphone_act_factor)

# Create data frame with concern levels for all no. of activities
create.table.concern <- function(x) {
  a <- as.data.frame(prop.table(table(x, sample3$no_smartphone_act_factor), 2))
  return(a)
}

sample3_activ <- map_df(sample3[19:23], create.table.concern)
sample3_activ$sample <- factor("Sample 3")
sample3_activ$type <- factor(rep(1:5, each = 52))
levels(sample3_activ$type) <- c("Online survey", "Smartphone usage", "Camera",
                                "Activity data", "GPS")
names(sample3_activ) <- c("concern", "no_smartphone_act_factor", "percentage", "Sample", "type")
sample3_activ$no_smartphone_act_factor <- as.factor(sample3_activ$no_smartphone_act_factor)
sample3_activ$concern <- factor(sample3_activ$concern,
                                levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))

str(sample3_activ$no_smartphone_act_factor)

# Create figure
ggplot(sample3_activ, aes(x = no_smartphone_act_factor, y = percentage, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample3_activ$no_smartphone_act_factor))) +
  scale_fill_manual(values = c("#458B00", "yellow", "orange", "#FF0000")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = rel(2), colour = "black"),
        axis.text.x = element_text(size = rel(2), colour = "black"),
        legend.text = element_text(size = rel(1.5)),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_wrap(~ type, ncol = 5)