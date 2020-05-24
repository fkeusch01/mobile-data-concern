# -----------------------------------------------------------------
# Code for: Keusch, Florian, Bella Struminskaya, Frauke Kreuter, &
# Martin Weichbold. (2020). Combining active
# and passive mobile data collection: A survey of concerns. 
# In Hill, C.A., et al. (Eds.) Big Data Meets Survey Science.
# Hoboken, NJ: Wiley.
# Data analysis - Sample 4
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
sample4 <- read.csv("sample4.csv", stringsAsFactors = T)

# -----------------------------------------------------------------
#Inspect data
str(sample4)
View(sample4)

# -----------------------------------------------------------------
# Response time not available

# Proportion of smartphone completes not available

# -----------------------------------------------------------------
# Descriptives - Table 22.1 Sample 4
# Concern
summary(sample4$concern_websurvey_di)
sample4$concern_websurvey_di <- factor(sample4$concern_websurvey_di,
                                       levels = c("Low", "High"))
prop.table(table(sample4$concern_websurvey_di))

summary(sample4$concern_trackingapp_di)
sample4$concern_trackingapp_di <- factor(sample4$concern_trackingapp_di,
                                         levels = c("Low", "High"))
prop.table(table(sample4$concern_trackingapp_di))

summary(sample4$concern_camera_di)
sample4$concern_camera_di <- factor(sample4$concern_camera_di,
                                    levels = c("Low", "High"))
prop.table(table(sample4$concern_camera_di))

summary(sample4$concern_sensor_di)
sample4$concern_sensor_di <- factor(sample4$concern_sensor_di,
                                    levels = c("Low", "High"))
prop.table(table(sample4$concern_sensor_di))

summary(sample4$concern_gps_di)
sample4$concern_gps_di <- factor(sample4$concern_gps_di,
                                 levels = c("Low", "High"))
prop.table(table(sample4$concern_gps_di))

# Frequency of smartphone use
summary(sample4$smartphone_use_di)
sample4$smartphone_use_di <- factor(sample4$smartphone_use_di,
                                    levels = c("Less then every day",
                                               "Every day"))
prop.table(table(sample4$smartphone_use_di))

# Number of smartphone activities
summary(sample4$no_smartphone_act)
sd(sample4$no_smartphone_act)

# Smartphone skills
summary(sample4$smartphone_skills_di)
sample4$smartphone_skills_di <- factor(sample4$smartphone_skills_di,
                                       levels = c("Low", "High"))
prop.table(table(sample4$smartphone_skills_di))

# General privacy concern
summary(sample4$privacy_general_di)
sample4$privacy_general_di <- factor(sample4$privacy_general_di,
                                     levels = c("Low", "High"))
prop.table(table(sample4$privacy_general_di))

# Socio-demographics
summary(sample4$gender)
sample4$gender <- factor(sample4$gender, levels = c("male", "female"))
prop.table(table(sample4$gender))

summary(sample4$age)
prop.table(table(sample4$age))

summary(sample4$edu_school)
prop.table(table(sample4$edu_school))

# -----------------------------------------------------------------
# Figure 1: Concern
# Create data frame that includes % of concern
create.data.frame.table <- function(x) {
  a <- as.data.frame(prop.table(table(x)))
  return(a)
}

sample4_concern <- map_df(sample4[19:23], create.data.frame.table)
sample4_concern$type <- factor(rep(1:5, each = 4))
levels(sample4_concern$type) <- c("Online survey", "Smartphone usage", "Camera",
                                  "Activity data", "GPS")
sample4_concern$type <- factor(sample4_concern$type,
                               levels = c("Smartphone usage", "GPS",
                                          "Activity data", "Camera",
                                          "Online survey"))
names(sample4_concern) <- c("concern", "freq", "type")
sample4_concern$concern <- factor(sample4_concern$concern,
                                  levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))
sample4_concern$freq <- sample4_concern$freq * 100
sample4_concern$sample <- factor("Sample 4")
sample4_concern

# Create figure
ggplot(sample4_concern, aes(x = type, y = freq, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample4_concern$type))) +
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
# Combine all four samples in one figure for concern
all <- rbind(sample1_concern, sample2_concern, sample3_concern, sample4_concern)
levels(all$concern) <- c('not at all \nconcerned', 'a little \nconcerned', 'somewhat \nconcerned', 'a lot \nconcerned')

ggplot(all, aes(x = type, y = freq, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample4_concern$type))) +
  scale_fill_manual(values = c("grey64", "grey44", "grey24", "grey4")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = rel(1.2), colour = "black"),
        axis.text.x = element_text(size = rel(1.2), colour = "black"),
        legend.text = element_text(size = rel(1)),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = rel(1), face="bold", color = "black")) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_wrap(~ sample, ncol = 1)

# Safe figure
ggsave("Figure22.1.png", width = 6.5, height = 8.5, units = "in")

# -----------------------------------------------------------------
# Logistic regression predicting concern
log.regression <- function(x) {
  y <- glm(x ~ privacy_general_di + smartphone_use_di + smartphone_skills_di + no_smartphone_act +
             age + gender + edu_school, data = sample4, family = "binomial")
  y
}

sample4_logit <- map(sample4[27:31],log.regression)

log.regression.result <- function(y) {
  print(summary(y))
  print(with(y, null.deviance - deviance))
  print(with(y, df.null - df.residual))
  print(with(y, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
}

map(sample4_logit,log.regression.result)

# Calculate AMEs
sample4_AME <- as.data.frame(map_df(sample4_logit, function(x) {
  summary(margins(x))
}
))

sample4_AME$concern <- as.factor(rep(1:5, each = 8))
levels(sample4_AME$concern) <- c("Online Survey",
                                 "Smartphone \nusage",
                                 "Camera",
                                 "Activity data",
                                 "GPS")
sample4_AME$sample <- as.factor("Sample 4")
sample4_AME$factor <- factor(sample4_AME$factor,
                             levels = c("edu_schoolwithout hs",
                                        "age50+",
                                        "age30-49",
                                        "genderfemale",
                                        "privacy_general_diHigh",
                                        "no_smartphone_act",
                                        "smartphone_skills_diHigh",
                                        "smartphone_use_diEvery day"))

levels(sample4_AME$factor) <- c("Without HS degree",
                                "50 years and older",
                                "30-49 years",
                                "Female",
                                "High general privacy concern",
                                "No. of smartphone activities",
                                "High smartphone skills",
                                "Use smartphone every day")

# Plot AMEs
ggplot(sample4_AME, aes(y = AME, x = factor)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2, size = 0.5) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Sample 4") +
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
# Combine all four samples in one figure for AMEs
all_AME <- rbind(sample1_AME, sample2_AME, sample3_AME, sample4_AME)
all_AME$sample <- factor(all_AME$sample,
                         levels = c("Sample 4",
                                    "Sample 3",
                                    "Sample 2",
                                    "Sample 1"))

write.csv(all_AME, file = "all_AME.csv", row.names=FALSE)

ggplot(all_AME, aes(y = AME, x = factor, color = sample)) +
  geom_point(aes(shape = sample), size = 2, position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                size = 0.5, width = 0.2, position = position_dodge(0.7)) +
  scale_shape_manual(values = c(15, 16, 17, 18),
                     guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = c("grey4", "grey24", "grey44", "grey64"),
                     guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2, size = 0.5) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = rel(1.2), colour = "black"),
        axis.text.x = element_text(size = rel(1.2), colour = "black"),
        axis.title.x = element_text( size = rel(1), color = "black"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.2)),
        strip.text = element_text(size = rel(1.2), face="bold", color = "black"),
        legend.position = "bottom") +
  facet_wrap(~ concern, ncol = 5)

# Safe figure
ggsave("Figure22.2.png", width = 9, height = 6, units = "in")

# -----------------------------------------------------------------
# Analyzing effect of no. of smartphone activities
# Make no. of smartphone activities into factor
sample4$no_smartphone_act_factor <- as.factor(sample4$no_smartphone_act)
summary(sample4$no_smartphone_act_factor)

# Create data frame with concern levels for all no. of activities
create.table.concern <- function(x) {
  a <- as.data.frame(prop.table(table(x, sample4$no_smartphone_act_factor), 2))
  return(a)
}

sample4_activ <- map_df(sample4[19:23], create.table.concern)
sample4_activ$sample <- factor("Sample 4")
sample4_activ$type <- factor(rep(1:5, each = 52))
levels(sample4_activ$type) <- c("Online survey", "Smartphone usage", "Camera",
                                "Activity data", "GPS")
names(sample4_activ) <- c("concern", "no_smartphone_act_factor", "percentage", "Sample", "type")
sample4_activ$no_smartphone_act_factor <- as.factor(sample4_activ$no_smartphone_act_factor)
sample4_activ$concern <- factor(sample4_activ$concern,
                                levels = c('not at all concerned', 'a little concerned', 'somewhat concerned', 'a lot concerned'))

# Create figure
ggplot(sample4_activ, aes(x = no_smartphone_act_factor, y = percentage, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(sample4_activ$no_smartphone_act_factor))) +
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

# Put everything in one figur
# Create large dataset
all_activ <- rbind(sample1_activ, sample2_activ, sample3_activ, sample4_activ)
all_activ$percentage <- all_activ$percentage * 100

# Create figure
ggplot(all_activ, aes(x = no_smartphone_act_factor, y = percentage, fill = concern)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(all_activ$no_smartphone_act_factor))) +
  scale_fill_manual(values = c("grey64", "grey44", "grey24", "grey4")) +
  theme_minimal() +
  labs(x = "Number of Smartphone Activities") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = rel(0.75), colour = "black"),
        axis.text.x = element_text(size = rel(1.2), colour = "black"),
        legend.text = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1), face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = rel(1), face="bold", color = "black")) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_grid(Sample ~ type)

# Safe figure
ggsave("Figure22.3.png", width = 9, height = 6, units = "in")