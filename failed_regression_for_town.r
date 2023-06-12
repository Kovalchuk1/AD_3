library(tidyverse)
library(ggplot2)
library(dplyr)
library(latex2exp)
library(gridExtra)
library(lmtest)
library(stargazer)
library(car)
library(GGally)
library(multcomp)
library(dplyr)

Populations <- read_csv("C:/Users/user/Desktop/AD/population_ratio.csv")
guns <- read_csv("C:/Users/user/Desktop/AD/guns.csv")
incidents <- read_csv("C:/Users/user/Desktop/AD/incidents.csv")
participants <- read_csv("C:/Users/user/Desktop/AD/participants.csv")
population <- read_csv("C:/Users/user/Desktop/AD/sub-est2019_all.csv")
population <- read_csv("C:/Users/user/Desktop/AD/sub-est2019_all.csv")




population$population_count <- rowMeans(population[, c("POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017")])

library(dplyr)

df <- population[, c("NAME", "STNAME", "population_count")]

unique(gsub(".+\\s(\\w+)$", "\\1", df$NAME))

df_city <- df[grepl("city", df$NAME), c("NAME", "STNAME", "population_count")]
df_town <- df[grepl("town", df$NAME), c("NAME", "STNAME", "population_count")]
df_village <- df[grepl("village", df$NAME), c("NAME", "STNAME", "population_count")]

df_city$NAME <- sub(" city", "", df_city$NAME)
df_city$NAME <- sub(" \\(pt\\.\\)", "", df_city$NAME)

df_town$NAME <- sub(" town", "", df_town$NAME)
df_town$NAME <- sub(" \\(pt\\.\\)", "", df_town$NAME)

df_village$NAME <- sub(" village", "", df_village$NAME)
df_village$NAME <- sub(" \\(pt\\.\\)", "", df_village$NAME)

unique(gsub(".+\\s(\\w+)$", "\\1", df_village$NAME))
unique(gsub(".+\\s(\\w+)$", "\\1", df_town$NAME))
unique(gsub(".+\\s(\\w+)$", "\\1", df_city$NAME))

df_city_unique <- subset(df_city, !duplicated(df_city))



joined_df <- inner_join(participants, incidents, by = "incident_id")
grouped_df <- joined_df %>%
  group_by(city_or_county, state) %>%
  summarize(female = sum(participant_gender == "Female", na.rm = TRUE),
            male = sum(participant_gender == "Male", na.rm = TRUE))
grouped_df <- grouped_df %>%
  rename(name = city_or_county)


df_count <- incidents %>%
  group_by(city_or_county, state) %>%
  summarize(incident_count = n())
df_count <- df_count %>%
  rename(NAME = city_or_county,
         STNAME = state)

df_village_count <- merge(df_village, df_count, by=c("NAME", "STNAME"))
df_village_count$result <-  df_village_count$incident_count / df_village_count$population_count 

df_town_count <- merge(df_town, df_count, by=c("NAME", "STNAME"))
df_town_count$result <-  df_town_count$incident_count / df_town_count$population_count

df_city_count <- merge(df_city, df_count, by=c("NAME", "STNAME"))
df_city_count$result <-  df_city_count$incident_count / df_city_count$population_count
df_city_count <- df_city_count %>% distinct()

x <- rbind(df_village_count, df_city_count, df_town_count)

x <- x %>%
  rename(state = STNAME, name = NAME, total = result)

x <- merge(x, grouped_df, by = c("state", "name"))

x


state1 <- c("Pennsylvania", "California","Ohio", "Colorado", "North Carolina", "Oklahoma", "New Mexico", "Louisiana", "Maryland","Tennessee",          
            "Missouri","District of Columbia", "Illinois", "Delaware", "Utah", "Michigan", "Georgia", "Indiana", "Mississippi","New York", "Florida", "Washington", "South Carolina", "Arizona", "Kentucky",            
            "New Jersey", "Virginia", "Wisconsin", "Rhode Island", "Texas", "Alabama", "Kansas", "Connecticut", "West Virginia", "Minnesota",           
            "Nevada", "Nebraska", "Massachusetts", "Hawaii", "New Hampshire", "Iowa", "Alaska", "Arkansas", "Idaho", "Oregon", "Wyoming", "Maine","North Dakota", "Montana","Vermont","South Dakota")

count <-  table(incidents$state[incidents$state %in% state1])
y <- as.data.frame(count)
numeric_values <- c(0.889, 0.939, 0.916, 0.889, 0.939, 0.951, 0.958, 0.939, 0.949, 0.919, 0.913, 0.949,
                    0.915, 0.937, 0.916, 0.938, 0.930, 0.893, 0.896, 0.925, 0.944, 0.959, 0.921, 0.956,
                    0.874, 0.916, 0.924, 0.942, 0.912, 0.952, 0.951, 0.909, 0.947, 0.915, 0.949, 0.922,
                    0.904, 0.939, 0.931, 0.934, 0.901, 0.935, 0.904, 0.920, 0.938, 0.943, 0.939, 0.949,
                    0.885, 0.937, 0.941)

y$hdi <- numeric_values


#відсоток темношрікого населення
y$black_person <- c(26.8, 3.8, 4.9, 15.7, 16.5, 4.3, 10.1, 1.4, 10.7, 16.8, 31.4, 2.6, 0.8, 14.7, 9.1, 
                    3.4, 6.2, 8.2, 32.5, 1.3, 20.8, 8.6, 14.2, 5.4, 37.3, 11.7, 0.6, 4.6, 9.1, 1.1, 13.7, 
                    2.7, 17.6, 21.5, 1.2, 12.1, 7.6, 2.0, 10.8, 7.2, 27.9, 1.7, 17.0, 12.9, 1.9, 1.3, 19.6, 
                    13.6, 3.5, 6.5, 1.8)

#кількість поліції відсоток
y$police <-  c(2.42, 1.89, 1.92, 2.07, 1.72, 2.21, 2.55, 2.84, 1.92, 2.30, 2.49, 1.52, 2.09, 2.71, 1.74, 1.69,
               2.33, 1.92, 2.60, 1.98, 2.76, 2.59, 1.79, 1.53, 2.58, 2.23, 1.77, 1.95, 1.88, 2.19, 2.63, 2.06, 
               2.27, 2.31, 1.43, 1.97, 2.29, 1.69, 2.29, 1.99, 2.50, 1.50, 2.42, 2.05, 1.29, 1.92, 2.14, 1.39, 
               1.95, 1.86, 1.81)

#оцінка на складність отримання зброї
y$guns <- c(2, 2, 2,2, 9, 6, 9, 8, 2, 2, 2, 8.5, 2, 8.5, 3, 5, 2, 2, 2, 2, 9, 9, 6, 6, 2, 2, 2, 4, 2, 3, 9, 2, 
            9, 2, 2, 4, 2, 4.5, 6, 8.5, 2, 2, 2, 2, 2, 2, 4, 7, 2, 5, 2)

#індекс зарплати в штатах
y$income  <- c(0.923, 1.000, 0.928, 0.919, 0.992, 0.976, 1.000, 1.000, 1.000, 0.932, 0.950, 0.971, 0.915, 0.980, 
               0.952, 0.968, 0.958, 0.929, 0.948, 0.929, 0.983, 1.000, 0.946, 0.978, 0.899, 0.946, 0.931, 0.977, 
               0.947, 0.973, 0.988, 0.938, 1.000, 0.949, 1.000, 0.960, 0.950, 0.969, 0.971, 0.960, 0.922, 0.961,
               0.945, 0.979, 0.950, 0.948, 0.973, 0.987, 0.919, 0.959, 0.992)

#відсоток наркотиків
y$drugs <- c(9.42, 20.34, 12.68, 10.25, 14.62, 19.83, 13.46, 14.22, 20,22, 10.64, 9.78, 11.33, 10.44, 11.78, 14.58,
             8.62, 9.98, 11.47, 7.73, 18.67, 16.32, 19.52, 15.71, 12.39, 8.21, 9.98, 14.86, 9.23, 21.13, 15.23, 10.24, 
             16.32, 11.45, 10.11, 7.52, 11.64, 12.03, 19.45, 13.41, 9.88, 8.03, 10.31, 9.38, 8.74, 17.63, 8.67, 20.18,
             11.35, 12.08, 7.95)

#розміщення штатів у частиних
north <- c("Vermont", "Connecticut", "Massachusetts", "Maine", "New Hampshire", "Rhode Island", "New Jersey", "New York", "Pennsylvania") 

midwest <- c("Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") 

south <- c("Virginia", "Delaware", "Georgia", "West Virginia", "Maryland", "North Carolina", "Florida", "South Carolina", "Alabama", "Kentucky", "Mississippi", 
           "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas", "District of Columbia") 

west <- c("Idaho", "Arizona", "Wyoming", "Colorado", "Montana", "Nevada", "New Mexico", "Utah", "Alaska", "Washington", "Hawaii", "California", "Oregon")


find_location <- function(state_name) {
  if (state_name %in% north) {
    return("north")
  } else if (state_name %in% midwest) {
    return("midwest")
  } else if (state_name %in% south) {
    return("south")
  } else if (state_name %in% west) {
    return("west")
  } else {
    return(NA)
  }
}

y$location <- sapply(y$Var1, find_location)


#сконвертуємо окремі змінні в логічні  west, north, midwest, south
y <- y %>% mutate(south = ifelse(location == "south", 1, 0),
                  west = ifelse(location == "west", 1, 0),
                  midwest = ifelse(location == "midwest", 1, 0),
                  north = ifelse(location == "north", 1, 0),)

y <- y[, -which(names(y) == "Freq")]
y <- y %>%
  rename(state = Var1)
y

df <- merge(x, y, by = "state")
df


sum(rowSums(sapply(df, is.infinite)) > 0)

df <- df[!is.na(df$total) & is.finite(df$total), ]
df <- df[complete.cases(df[, c("north", "south", "income", "hdi", "black_person", "police", "west", "midwest", "guns", "drugs")]), ]


ggcorr(df %>% dplyr::select(incident_count,male, female, population_count, north, south, income, hdi, black_person, police, west, midwest ,guns , drugs), label = TRUE)



model_3 <- lm(incident_count ~population_count+ north + south + income + hdi + black_person + police + west + midwest + guns + drugs, data = df)
model_3_hc1 <- coeftest(model_3, vcov. = hccm(model_3, type = "hc1"))


ci <- coefci(model_3, vcov. = hccm(model_3, type = "hc1"))
ci

stargazer(model_3, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_3_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model_3)

