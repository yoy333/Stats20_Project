# Investigating Healthcare Inequalities in LA County

library(ggplot2)
library(readxl)

#loading the excel data
health_data <- read_excel("data/LACHS2023_Adult_DiffAccCare.xlsx", skip = 6, col_names = FALSE)

names(health_data) <- c("col1", "group", "flag", "proportion", "ci_low", "dash", "ci_high", "estimated_n")

# keep only useful columns
health_data <- health_data[, c("group", "flag", "proportion", "ci_low", "ci_high", "estimated_n")]

head(health_data)
#noticed a lot of nas ! rip
str(health_data)

#cleaning up a bit
health_clean <- health_data[!is.na(health_data$proportion), ]
health_clean <- health_clean[!is.na(health_data$group), ]

#convering to %
health_clean$percent <- health_clean$proportion * 100
health_clean

# one thing we can do is plot % difficulty by race/ethnicity
race_groups <- c("Latinx", "NH White", "NH Black or African American", 
                 "NH Asian", "NH Native Hawaiian or Pacific Islander",
                 "NH American Indian or Alaska Native", "NH Multi-Racial or Other Race")

#check for this in our clean data
race_data <- health_clean[health_clean$group %in% race_groups, ]

#plotting :D
ggplot(race_data, aes(x = group, y = percent, fill = group)) +
  geom_bar(stat = "identity") +
  labs(title = "Difficulty Accessing Healthcare By Race/Ethnicity", 
       subtitle = "LA County Health Survey 2023", 
       x = "Race/Ethnicity", y = "Percent (%)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Latinx most

# we could also plot by age group
age_groups <- c("18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "65 or over")

age_data <- health_clean[health_clean$group %in% age_groups, ]
ggplot(age_data, aes(x = group, y = percent, fill = group)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Difficulty Accessing Healthcare By Age Group",
       subtitle = "LA County Health Survey 2023", 
       x = "Age Group", y = "Percent (%)") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 25-29 most
print(health_clean, n = 83) 

#oh also an income plot
poverty_groups <- c("0-99% FPL", "100%-199% FPL", "200%-299% FPL", "300% or above FPL")

poverty_data <- health_clean[health_clean$group %in% poverty_groups, ]

ggplot(poverty_data, aes(x = group, y = percent, fill = group))+
  geom_bar(stat = "identity")+
  labs(title = "Difficulty Accessing Healthcare by Income Level", 
       subtitle = "LA County Health Survey 2023", 
       x = "Federal Poverty Level", y = "Percent (%)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# seems like 100% - 199% federal poverty level struggles most, as they're just above the poverty line, so they don't receive as much support as 0-99%

#inequalities by region too, lastly
region_groups <- c("Antelope Valley", "San Fernando", "San Gabriel", 
                   "Metro", "West", "South", "East", "South Bay")

region_data <- health_clean[health_clean$group %in% region_groups, ]
ggplot(region_data, aes(x = group, y = percent, fill = group)) +
  geom_bar(stat = "identity") + 
  labs(title = "Difficulty Accessing Healthcare by Region", 
       subtitle = "LA County Health Survey 2023",
       x = "Region", y = "Percent (%)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#South seems highest

race_model <- lm(percent ~ group, data = race_data)
summary(race_model)
print(health_clean[38:46, ])
# check out lm results, what yes/no refer to