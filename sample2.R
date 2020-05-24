# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data analysis - Sample 2
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
sample2 <- read.csv("sample2.csv", stringsAsFactors = T)

# -----------------------------------------------------------------
# Inspect data
str(sample2)
View(sample2)

# -----------------------------------------------------------------
#Response time
summary(sample2$duration)

#Proportion of smartphone completes
summary(sample2$participant_device)
prop.table(table(sample2$participant_device, useNA = "always"))

# -----------------------------------------------------------------
# Descriptives - Table 22.1 Sample 2
# Concern
summary(sample2$concern_websurvey_di)
sample2$concern_websurvey_di <- factor(sample2$concern_websurvey_di,
                                       levels = c("Low", "High"))
prop.table(table(sample2$concern_websurvey_di))

summary(sample2$concern_trackingapp_di)
sample2$concern_trackingapp_di <- factor(sample2$concern_trackingapp_di,
                                         levels = c("Low", "High"))
prop.table(table(sample2$concern_trackingapp_di))

summary(sample2$concern_camera_di)
sample2$concern_camera_di <- factor(sample2$concern_camera_di,
                                    levels = c("Low", "High"))
prop.table(table(sample2$concern_camera_di))

summary(sample2$concern_sensor_di)
sample2$concern_sensor_di <- factor(sample2$concern_sensor_di,
                                    levels = c("Low", "High"))
prop.table(table(sample2$concern_sensor_di))

summary(sample2$concern_gps_di)
sample2$concern_gps_di <- factor(sample2$concern_gps_di,
                                 levels = c("Low", "High"))
prop.table(table(sample2$concern_gps_di))

# Frequency of smartphone use
summary(sample2$smartphone_use_di)
sample2$smartphone_use_di <- factor(sample2$smartphone_use_di,
                                    levels = c("Less then every day",
                                               "Every day"))
prop.table(table(sample2$smartphone_use_di))

# Number of smartphone activities
summary(sample2$no_smartphone_act)
sd(sample2$no_smartphone_act)

# Smartphone skills
summary(sample2$smartphone_skills_di)
sample2$smartphone_skills_di <- factor(sample2$smartphone_skills_di,
                                       levels = c("Low", "High"))
prop.table(table(sample2$smartphone_skills_di))

# General privacy concern
summary(sample2$privacy_general_di)
sample2$privacy_general_di <- factor(sample2$privacy_general_di,
                                     levels = c("Low", "High"))
prop.table(table(sample2$privacy_general_di))

# Socio-demographics
summary(sample2$gender)
sample2$gender <- factor(sample2$gender, levels = c("male", "female"))
prop.table(table(sample2$gender))

summary(sample2$age)
prop.table(table(sample2$age))

summary(sample2$edu_school)
prop.table(table(sample2$edu_school))


# -----------------------------------------------------------------
# Figure 22.1: Concern
# Create data frame that includes % of concern
create.data.frame.table <- function(x) {
  a <- as.data.frame(prop.table(table(x)))
  return(a)
}

sample2_concern <- map_df(sample2[19:23], create.data.frame.table)
sample2_concern$type <- factor(rep(1:5, each = 4))
levels(sample2_concern$type) <- c("Online survey", "Smartphone usage", "Camera",
                                  "Activity data", "GPS")
sample2_concern$type <- factor(sample2_concern$type,
                               levels = c("Smartphone usage", "GPS",
                                          "Activity data", "Camera",
                                          "Online survey"))
names(sample2_concern) <- c("concern", "freq", "type")
sample2_concern$concern <- factor(sample2_concern$concern,
                                  levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))
sample2_concern$freq <- sample2_concern$freq * 100
sample2_concern$sample <- factor("Sample 2")
sample2_concern


# Create figure
ggplot(sample2_concern, aes(x = type, y = freq, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample2_concern$type))) +
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
             age + gender + edu_school, data = sample2, family = "binomial")
  y
}

sample2_logit <- map(sample2[29:33],log.regression)

log.regression.result <- function(y) {
  print(summary(y))
  print(with(y, null.deviance - deviance))
  print(with(y, df.null - df.residual))
  print(with(y, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
}

map(sample2_logit,log.regression.result)

# Calculate AMEs
sample2_AME <- as.data.frame(map_df(sample2_logit, function(x) {
  summary(margins(x))
}
))

sample2_AME$concern <- as.factor(rep(1:5, each = 8))
levels(sample2_AME$concern) <- c("Online Survey",
                                 "Smartphone \nusage",
                                 "Camera",
                                 "Activity data",
                                 "GPS")
sample2_AME$sample <- as.factor("Sample 2")
sample2_AME$factor <- factor(sample2_AME$factor,
                             levels = c("edu_schoolwithout hs",
                                        "age50+",
                                        "age30-49",
                                        "genderfemale",
                                        "privacy_general_diHigh",
                                        "no_smartphone_act",
                                        "smartphone_skills_diHigh",
                                        "smartphone_use_diEvery day"))

levels(sample2_AME$factor) <- c("Without HS degree",
                                "50 years and older",
                                "30-49 years",
                                "Female",
                                "High general privacy concern",
                                "No. of smartphone activities",
                                "High smartphone skills",
                                "Use smartphone every day")

# Plot AMEs
ggplot(sample2_AME, aes(y = AME, x = factor)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2, size = 0.5) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sample 2") +
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
sample2$no_smartphone_act_factor <- as.factor(sample2$no_smartphone_act)
summary(sample2$no_smartphone_act_factor)

# Create data frame with concern levels for all no. of activities
create.table.concern <- function(x) {
  a <- as.data.frame(prop.table(table(x, sample2$no_smartphone_act_factor), 2))
  return(a)
}

sample2_activ <- map_df(sample2[19:23], create.table.concern)
sample2_activ$sample <- factor("Sample 2")
sample2_activ$type <- factor(rep(1:5, each = 52))
levels(sample2_activ$type) <- c("Online survey", "Smartphone usage", "Camera",
                                "Activity data", "GPS")
names(sample2_activ) <- c("concern", "no_smartphone_act_factor", "percentage", "Sample", "type")
sample2_activ$no_smartphone_act_factor <- as.factor(sample2_activ$no_smartphone_act_factor)
sample2_activ$concern <- factor(sample2_activ$concern,
                                levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))

str(sample2_activ$no_smartphone_act_factor)

# Create figure
ggplot(sample2_activ, aes(x = no_smartphone_act_factor, y = percentage, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample2_activ$no_smartphone_act_factor))) +
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