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
health_clean <- health_clean[!is.na(health_clean$group), ]

#convering to %
health_clean$percent <- health_clean$proportion * 100
health_clean

# one thing we can do is plot % difficulty by race/ethnicity
race_groups <- c("Latinx", "NH White", "NH Black or African American", 
                 "NH Asian", "NH Native Hawaiian or Pacific Islander",
                 "NH American Indian or Alaska Native", "NH Multi-Racial or Other Race")

#check for this in our clean data
race_data <- health_clean[health_clean$group %in% race_groups, ]

clean_theme <- theme_minimal(base_size = 13) + 
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title.y       = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "gray50", size = 11),
    plot.caption       = element_text(color = "gray60", size = 9)
  )

#plotting :D
ggplot(race_data, aes(x = reorder(group, percent), y = percent, fill = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            hjust = -0.15, size = 3.5, color = "gray20") +
  scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(title = "Difficulty Accessing Healthcare By Race/Ethnicity", 
       subtitle = "LA County Health Survey 2023", 
       x = "Race/Ethnicity", y = "Percent (%)") +
  clean_theme

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
poverty_data$group <- factor(poverty_data$group, levels = poverty_groups)

ggplot(poverty_data, aes(x = group, y = percent, fill = group))+
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorRampPalette(c("#b2182b","#f4a582","#92c5de","#2166ac"))(length(poverty_groups))) +
  labs(title = "Difficulty Accessing Healthcare by Income Level", 
       subtitle = "LA County Health Survey 2023", 
       x = "Federal Poverty Level", y = "Percent (%)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# seems like 100% - 199% federal poverty level struggles most, as they're just above the poverty line, so they don't receive as much support as 0-99%


district_groups <- c("Alhambra", "Antelope Valley", "Bellflower", "Central", "Compton", 
                     "East LA", "East Valley", "El Monte", "Foothill", "Glendale", 
                     "Harbor", "Hollywood-Wilshire", "Inglewood", "Long Beach", 
                     "Northeast", "Pasadena", "Pomona", "San Antonio", "San Fernando", 
                     "South", "Southeast", "Southwest", "Torrance", "West", 
                     "West Valley", "Whittier")
district_data <- health_clean[health_clean$group %in% district_groups, ]

# geographic groups of all the districts for a North South East West and Central La split
district_data$geo_group <- NA
district_data$geo_group[district_data$group %in% c("Antelope Valley", "San Fernando", "East Valley", "West Valley", "Glendale", "Foothill")] <- "North"
district_data$geo_group[district_data$group %in% c("Central", "Northeast")]                                                                   <- "Central"
district_data$geo_group[district_data$group %in% c("East LA", "El Monte", "San Antonio", "Pomona", "Pasadena", "Whittier", "Alhambra")]        <- "East"
district_data$geo_group[district_data$group %in% c("South", "Southeast", "Southwest", "Inglewood", "Compton", "Harbor", "Long Beach", "Torrance", "Bellflower")] <- "South"
district_data$geo_group[district_data$group %in% c("West", "Hollywood-Wilshire")]                                                             <- "West"

district_data$group <- factor(district_data$group, levels = c(
  "Antelope Valley", "San Fernando", "East Valley", "West Valley", "Glendale", "Foothill",  # North
  "Central", "Northeast",                                                                     # Central
  "Alhambra", "El Monte", "East LA", "San Antonio", "Pomona", "Pasadena", "Whittier",        # East
  "South", "Southeast", "Southwest", "Inglewood", "Compton", "Harbor", "Long Beach", "Torrance", "Bellflower", # South
  "Hollywood-Wilshire", "West"                                                                # West
))
geo_colors <- c(
  # North  blue shades
  "Antelope Valley" = "#08306b", "San Fernando" = "#2171b5",
  "East Valley"     = "#4292c6", "West Valley"  = "#6baed6",
  "Glendale"        = "#9ecae1", "Foothill"     = "#c6dbef",
  # Central  green shades
  "Central"  = "#238b45", "Northeast" = "#74c476",
  # East  orange shades
  "Alhambra"    = "#7f2704", "El Monte"   = "#a63603",
  "East LA"     = "#d94801", "San Antonio"= "#f16913",
  "Pomona"      = "#fd8d3c", "Pasadena"   = "#fdae6b",
  "Whittier"    = "#fdd0a2",
  # South - red shades
  "South"       = "#67001f", "Southeast"  = "#a50026",
  "Southwest"   = "#d73027", "Inglewood"  = "#f46d43",
  "Compton"     = "#fdae61", "Harbor"     = "#fee090",
  "Long Beach"  = "#d7191c", "Torrance"   = "#e85d5d",
  "Bellflower"  = "#f5a0a0",
  # West - purple
  "Hollywood-Wilshire" = "#4d004b", "West" = "#9970ab"
)

ggplot(district_data, aes(x = group, y = percent, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = geo_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + 
  facet_wrap(~ geo_group, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(title = "Difficulty Accessing Healthcare by Health District", 
       subtitle = "% of adults reporting difficulty",
       x = "Region", y = "Percent (%)") +
  clean_theme
#South seems highest

geo_summary <- aggregate(percent ~ geo_group, data = district_data, FUN = mean)

ggplot(geo_summary, aes(x = geo_group, y = percent, fill = geo_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("North" = "#2171b5", "Central" = "#238b45", 
                               "East"  = "#d94801", "South"   = "#a50026", 
                               "West"  = "#4d004b")) +
  labs(title = "Difficulty Accessing Healthcare by Region",
       x = "Region", y = "Percent (%)") +
  theme_minimal() +
  theme(legend.position = "none")

race_model <- lm(percent ~ group, data = race_data)
summary(race_model)
print(health_clean[38:46, ])
# check out lm results, what yes/no refer to