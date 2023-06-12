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
library(fixest)
install.packages("modelsummary")
library(latex2exp)
library(kableExtra)
library(modelsummary)

Populations <- read_csv("C:/Users/user/Desktop/AD/population_ratio.csv")
guns <- read_csv("C:/Users/user/Desktop/AD/guns.csv")
incidents <- read_csv("C:/Users/user/Desktop/AD/incidents.csv")
participants <- read_csv("C:/Users/user/Desktop/AD/participants.csv")
population <- read_csv("C:/Users/user/Desktop/AD/sub-est2019_all.csv")
population <- read_csv("C:/Users/user/Desktop/AD/sub-est2019_all.csv")


x <- incidents %>%
  group_by(state) %>%
  summarise(total = sum(n_killed) + sum(n_injured))


#створення таблиці даних які потрібні для повного аналізу
state1 <- c("Pennsylvania", "California","Ohio", "Colorado", "North Carolina", "Oklahoma", "New Mexico", "Louisiana", "Maryland","Tennessee",          
             "Missouri","District of Columbia", "Illinois", "Delaware", "Utah", "Michigan", "Georgia", "Indiana", "Mississippi","New York", "Florida", "Washington", "South Carolina", "Arizona", "Kentucky",            
             "New Jersey", "Virginia", "Wisconsin", "Rhode Island", "Texas", "Alabama", "Kansas", "Connecticut", "West Virginia", "Minnesota",           
             "Nevada", "Nebraska", "Massachusetts", "Hawaii", "New Hampshire", "Iowa", "Alaska", "Arkansas", "Idaho", "Oregon", "Wyoming", "Maine","North Dakota", "Montana","Vermont","South Dakota")

count <-  table(incidents$state[incidents$state %in% state1])
x <- as.data.frame(count)

joined_df <- inner_join(participants, incidents, by = "incident_id")


# Групування даних за штатом та підрахунок суми Females та Males
grouped_df <- joined_df %>%
  group_by(state) %>%
  summarize(female = sum(participant_gender == "Female", na.rm = TRUE),
            male = sum(participant_gender == "Male", na.rm = TRUE))



#Populations <- Populations[, c(1, 3)]
x <- x %>%
  rename(state = Var1)
x <- merge(x, Populations, by = "state")


x <- x %>%
  rename(count_population = '2018 Population')

x <- subset(x, select = -per_capita)
x$male <-grouped_df$male
x$female <-grouped_df$female
#соціально-економічний статус на злочинність у штатах
numeric_values <- c(0.889, 0.939, 0.916, 0.889, 0.939, 0.951, 0.958, 0.939, 0.949, 0.919, 0.913, 0.949,
                    0.915, 0.937, 0.916, 0.938, 0.930, 0.893, 0.896, 0.925, 0.944, 0.959, 0.921, 0.956,
                    0.874, 0.916, 0.924, 0.942, 0.912, 0.952, 0.951, 0.909, 0.947, 0.915, 0.949, 0.922,
                    0.904, 0.939, 0.931, 0.934, 0.901, 0.935, 0.904, 0.920, 0.938, 0.943, 0.939, 0.949,
                    0.885, 0.937, 0.941)

x$hdi <- numeric_values

  
#відсоток темношрікого населення
x$black_person <- c(26.8, 17.8, 4.9, 15.7, 16.5, 4.3, 10.1, 19.4, 10.7, 16.8, 31.4, 2.6, 0.8, 14.7, 9.1, 
                    3.4, 6.2, 18.2, 32.5, 18.3, 20.8, 15.6, 14.2, 5.4, 37.3, 11.7, 0.6, 4.6, 9.1, 1.1, 13.7, 
                    2.7, 17.6, 21.5, 1.2, 12.1, 17.6, 2.0, 10.8, 7.2, 27.9, 1.7, 17.0, 12.9, 1.9, 1.3, 19.6, 
                    13.6, 13.5, 6.5, 1.8)

#кількість поліції відсоток
x$police <-  c(2.42, 1.89, 1.92, 2.07, 1.72, 2.21, 2.55, 2.84, 1.92, 2.30, 2.49, 1.52, 2.09, 2.71, 1.74, 1.69,
               2.33, 1.92, 2.60, 1.98, 2.76, 2.59, 1.79, 1.53, 2.58, 2.23, 1.77, 1.95, 1.88, 2.19, 2.63, 2.06, 
               2.27, 2.31, 1.43, 1.97, 2.29, 1.69, 2.29, 1.99, 2.50, 1.50, 2.42, 2.05, 1.29, 1.92, 2.14, 1.39, 
               1.95, 1.86, 1.81)

#оцінка на складність отримання зброї
x$guns <- c(2, 2, 2,2, 9, 6, 9, 2, 2, 2, 2, 9, 2, 3, 3, 5, 2, 2, 2, 2, 9, 9, 6, 6, 2, 2, 2, 4, 8, 3, 9, 2, 
            9, 2, 2, 4, 2, 4.5, 6, 8.5, 2, 2, 2, 2, 2, 2, 4, 7, 2, 5, 2)

#індекс зарплати в штатах
x$income  <- c(0.923, 1.000, 0.928, 0.919, 0.992, 0.976, 1.000, 1.000, 1.000, 0.932, 0.950, 0.971, 0.915, 0.980, 
               0.952, 0.968, 0.958, 0.929, 0.948, 0.929, 0.983, 1.000, 0.946, 0.978, 0.899, 0.946, 0.931, 0.977, 
               0.947, 0.973, 0.988, 0.938, 1.000, 0.949, 1.000, 0.960, 0.950, 0.969, 0.971, 0.960, 0.922, 0.961,
               0.945, 0.979, 0.950, 0.948, 0.973, 0.987, 0.919, 0.959, 0.992)

#відсоток наркотиків
x$drugs <- c(20.42, 20.34, 6.68, 15.25, 18.62, 9.83, 13.46, 14.22, 20,22, 10.64, 9.78, 7.33, 16.44, 11.78, 14.58,
             8.62, 14.98, 11.47, 7.73, 18.67, 16.32, 19.52, 6.71, 12.39, 15.21, 9.98, 14.86, 9.23, 21.13, 15.23, 10.24, 
             16.32, 8.45, 10.11, 11.52, 11.64, 12.03, 19.45, 13.41, 9.88, 8.03, 18.31, 9.38, 8.74, 17.63, 8.67, 9.18,
             11.35, 12.08, 7.95)

#відсоток алкоголю
x$alcohol <- c(12.2, 20.0, 15.0, 15.2, 16.7, 18.1, 18.3, 15.8, 24.4, 17.2, 15.8, 19.8, 14.8, 20.8, 16.6, 21.3, 16.5, 
               16.1, 18.0, 20.2, 14.7, 18.7, 19.8, 20.5, 12.5, 17.7, 21.3, 20.4, 14.5, 17.8, 17.0, 13.6, 17.6, 14.6, 
               24.9, 19.5, 13.6, 17.7, 18.5, 17.0, 16.3, 17.9, 10.9, 16.1, 11.4, 19.0, 17.0, 16.6, 11.8, 24.4, 16.9)

#ув'язнені
x$prisoners <- c(28883, 4434, 42320, 17537, 130390, 19981, 14957, 6585, 56657, 99974, 53627, 5602, 8252, 53657, 25546, 9031, 19920,
                 23022, 35682, 2404, 29994, 19403, 41122, 10592, 19192, 32461, 3814, 15302,13757, 2818, 19786, 7055, 
                 50716, 45697, 1791, 52175, 26871, 15166, 49244, 1, 20858, 3831, 28203, 163703, 6182, 1735, 37813, 
                 19104, 7162, 23377, 2374)
x$prisoners <- x$prisoners/x$count_population
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

x$location <- sapply(x$state, find_location)


#сконвертуємо окремі змінні в логічні  west, nourh, midwest, south
x <- x %>% mutate(south = ifelse(location == "south", 1, 0),
                          west = ifelse(location == "west", 1, 0),
                          midwest = ifelse(location == "midwest", 1, 0),
                          north = ifelse(location == "north", 1, 0),)
subset(x, select = c(state, income))

# Сортування датафрейму за колонкою "Freq"
x <- x %>% arrange(Freq)
#x <- subset(x, select = -c(5))
print(x)
x$female <- x$female /(x$male + x$female) / x$count_population
x$male <- x$male /  (x$male + x$female)/ x$count_population
x$Freq  <-  x$Freq / x$count_population
print(x)


ggcorr(x %>% dplyr::select(Freq , north, south, west, midwest , male ,guns , drugs), label = TRUE)

dev.new(width = 10, height = 5)
hist(x$Freq, main="Гістограма кількості інцидентів у штатах", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(x$female, main="Гістограма кількості учасників жінок", xlab="Freq", ylab="Count", col="blue")
dev.new(width = 10, height = 5)
hist(x$male, main="Гістограма кількості учасників чоловіків", xlab="Freq", ylab="Count", col="blue")

# Прологарифмування колонок
x$Freq <- log(x$Freq)
x$female <- log(x$female)
x$male <- log(x$male)
x$Freq <- abs(x$Freq)
x$male <- abs(x$female)
x$female <- abs(x$male)

#ПОЧАТОК АНАЛІЗУ
#графік регресії для кількості інцидентів і жінками та чоловіками



ggplot(x, aes(x = adult, y = male)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "adult", y = "male") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() 


ggplot(x) +
  geom_point(aes(x = female, y = Freq, color = "Female")) +
  geom_point(aes(x = male, y = Freq, color = "Male")) +
  geom_smooth(data = subset(x, !is.na(female)), aes(x = female, y = Freq, color = "Female"),
              method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(data = subset(x, !is.na(male)), aes(x = male, y = Freq, color = "Male"),
              method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Кількість жінок/чоловік", y = "Кількість інцидентів") +
  scale_color_manual(values = c("blue", "pink")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)) +
  ggtitle("Регресійна модель")



x$h<-x$hdi * x$income
#----------------------------------------------------------------------------------------
#БАЗОВА МОДЕЛЬ
ggcorr(x %>% dplyr::select( Freq, male, female, south, north, west, midwest), label = TRUE)

ggcorr(x %>% dplyr::select( Freq , male, south, north, west), label = TRUE)


model <- lm(Freq ~ male + south + west, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)


model <- lm(Freq ~ south + west + north , data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)

#МОДЕЛЬ З КОНТРОЛЬНИМИ ЗМІНАМИ
ggcorr(x %>% dplyr::select( Freq , male, south, north, west, hdi, black_person, police, guns, income, drugs, alcohol, prisoners), label = TRUE)


model <- lm(Freq ~ male + south + west + north + hdi + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)



ggcorr(x %>% dplyr::select( Freq , male, south, north, west, hdi, black_person, police, guns, income, drugs, alcohol, prisoners), label = TRUE)


model <- lm(Freq ~ hdi + income, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)


model <- lm(Freq ~ male + south + west + north + hdi + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

model_mult <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + drugs, data = x)
model_mult_hc1 <- coeftest(model_mult, vcov. = hccm(model_mult, type = "hc1"))

model_mult_sig <- lm(Freq ~ male + south + west + north + hdi + black_person + police + alcohol + prisoners + guns + drugs, data = x)
model_mult_sig_hc1 <- coeftest(model_mult_sig, vcov. = hccm(model_mult_sig, type = "hc1"))

model_mult_sig2 <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_mult_sig_hc12 <- coeftest(model_mult_sig2, vcov. = hccm(model_mult_sig2, type = "hc1"))

stargazer(model, model_mult, model_mult_sig, model_mult_sig2, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model_mult_hc1[, 2], model_mult_sig_hc1[, 2], model_mult_sig_hc12[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")


ggcorr(x %>% dplyr::select( Freq , male, south, north, west, black_person, police, guns, income, drugs, alcohol, prisoners), label = TRUE)


model <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

model2 <- lm(Freq ~ male + south + west + north + black_person + police + prisoners + guns + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, model2, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2], model_hc12[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)

model <- lm(Freq ~ south + alcohol  + police , data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)



model <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))

ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

stargazer(model, type = "latex",
          title = "Множинна регресія", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list( model_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")

summary(model)

x$Freq <- abs(x$Freq)
x$male <- abs(x$female)
x$female <- abs(x$male)




#-------------ПЕРЕВІРКА МОДЕЛІ НА СТІЙКІСТЬ----------------
plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = Freq)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "кількість інцидентів") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}
plot_regrex(x$male, "male")
plot_regrex(x$black_person, "відсоток темношкірого населення")
plot_regrex(x$police, "кількість поліції")
plot_regrex(x$guns, "оцінка на отримання зброї")
plot_regrex(x$income, "income")
plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")



plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = male)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "кількість чоловіків") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$black_person, "відсоток темношкірого населення")
plot_regrex(x$police, "кількість поліції")
plot_regrex(x$guns, "оцінка на отримання зброї")
plot_regrex(x$income, "income")
plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")


plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = black_person)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "відсоток темношкірого наслення") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$police, "кількість поліції")
plot_regrex(x$guns, "оцінка на отримання зброї")
plot_regrex(x$income, "income")
plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")



plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = police)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "к-сть поліції враховуючи населення") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$guns, "оцінка на отримання зброї")
plot_regrex(x$income, "income")
plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")


plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = guns)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "оцінка на отримання зброї") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$income, "income")
plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")


plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = income)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "індекс income") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$drugs, "оцінка проблеми із наркотиками")
plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")


plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = drugs)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "відсоток споживання наркотиків") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$alcohol, "відсоток споживання алкоголю")
plot_regrex(x$prisoners, "кількість заарештованих відносно населення")



plot_regrex <- function(group1, name1){
  ggplot(x, aes(x = group1, y = alcohol)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    labs(x = name1, y = "відсоток споживання алкоголю") +
    scale_fill_brewer(palette = "Set1") +
    theme_classic() 
  
}

plot_regrex(x$prisoners, "кількість заарештованих відносно населення")


ggcorr(x %>% dplyr::select( Freq , male, south, north, west, black_person, police, guns, income, drugs, alcohol, prisoners), label = TRUE)


#---------------------------------------------------------------------
#ПРАКТИКА ПЕРЕВІРКИ МОДЕЛІ НА СТІЙКІСТЬ
#X^2 I X^3
model <- lm(Freq ~ guns + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc1 <- coeftest(model, vcov. = hccm(model, type = "hc1"))
ci <- coefci(model, vcov. = hccm(model, type = "hc1"))

model2 <- lm(Freq ~ guns + I(drugs^2) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))
ci2 <- coefci(model2, vcov. = hccm(model2, type = "hc1"))

model3 <- lm(Freq ~ guns + I(guns^2) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc13 <- coeftest(model3, vcov. = hccm(model3, type = "hc1"))
ci3 <- coefci(model3, vcov. = hccm(model3, type = "hc1"))

model4 <- lm(Freq ~ guns + I(guns^2) + I(guns^3) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc14 <- coeftest(model4, vcov. = hccm(model4, type = "hc1"))
ci4 <- coefci(model4, vcov. = hccm(model4, type = "hc1"))

model5 <- lm(Freq ~ I(log(guns)) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc15 <- coeftest(model5, vcov. = hccm(model5, type = "hc1"))
ci5 <- coefci(model5, vcov. = hccm(model5, type = "hc1"))

model6 <- lm(Freq ~ I(log(guns)) + I(log(guns)^2) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc16 <- coeftest(model6, vcov. = hccm(model6, type = "hc1"))
ci6 <- coefci(model6, vcov. = hccm(model6, type = "hc1"))

model7 <- lm(Freq ~ I(log(guns)) + I(log(guns)^2) + I(log(guns)^3) + male + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc17 <- coeftest(model7, vcov. = hccm(model7, type = "hc1"))
ci7 <- coefci(model7, vcov. = hccm(model7, type = "hc1"))

stargazer(model, model2, model3, model4, model5, model6, model7,
          type = "latex",
          column.labels = c("1", "2", "3", "4", "5", "6", "7"),
          dep.var.caption = "",
          se = list(model_hc1[, 2], model_hc12[, 2], model_hc13[, 2], model_hc14[, 2], model_hc15[, 2], model_hc16[, 2], model_hc17[, 2], model_hc17[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci, ci2, ci3, ci4, ci5, ci6, ci7),
          no.space = TRUE,
          font.size = "tiny", 
          out="filename.html")


linearHypothesis(model2, c("guns = 0"),
                 vcov = hccm(model2, type = "hc1"))

linearHypothesis(model211, c("drugs = 0", "I(drugs^2) = 0"),
                 vcov = hccm(model211, type = "hc1"))

linearHypothesis(model21, c("guns = 0", "I(guns^2) = 0"),
                 vcov = hccm(model21, type = "hc1"))

linearHypothesis(model3, c("guns = 0", "I(guns^2) = 0", "I(guns^3) = 0"),
                 vcov = hccm(model3, type = "hc1"))

linearHypothesis(modellinlog, c("I(log(guns)) = 0"),
                 vcov = hccm(modellinlog, type = "hc1"))

linearHypothesis(modellinlog2, c("I(log(guns)) = 0", "I(log(guns)^2) = 0"),
                 vcov = hccm(modellinlog2, type = "hc1"))

linearHypothesis(modellinlog3, c("I(log(guns)) =0", "I(log(guns)^2) = 0", "I(log(guns)^3) = 0"),
                 vcov = hccm(modellinlog3, type = "hc1"))







model2 <- lm(Freq ~ male + guns + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))
ci <- coefci(model2, vcov. = hccm(model2, type = "hc1"))

model21 <- lm(Freq ~  male + I(male^2) +  guns + south + west + north + black_person + police + alcohol + prisoners + income + drugs, data = x)
model_hc121 <- coeftest(model21, vcov. = hccm(model21, type = "hc1"))
ci2 <- coefci(model21, vcov. = hccm(model21, type = "hc1"))

stargazer( model2, model21, 
           type = "latex",
           column.labels = c("Linear", "x^2"),
           dep.var.caption = "",
           se = list(model_hc12[, 2], model_hc121[, 2]),
           omit.stat = c("rsq", "f", "ser"),
           ci = TRUE, ci.custom = list(ci, ci2),
           no.space = TRUE,
           font.size = "tiny", 
           out="filename.html")

linearHypothesis(model2, c("male = 0"),
                 vcov = hccm(model2, type = "hc1"))

linearHypothesis(model21, c( "I(male^2) = 0"),
                 vcov = hccm(model21, type = "hc1"))


#------------ВЗАЄМОФАКТОРНІ---------------------------------------
model21 <- lm(Freq ~ south + black_person + police + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model_hc121 <- coeftest(model21, vcov. = hccm(model21, type = "hc1"))
ci2 <- coefci(model21, vcov. = hccm(model21, type = "hc1"))

model2 <- lm(Freq ~ south + black_person + police + south:police + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))
ci <- coefci(model2, vcov. = hccm(model2, type = "hc1"))


model3 <- lm(Freq ~ south + black_person + police + west:income + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model3_hc1 <- coeftest(model3, vcov. = hccm(model3, type = "hc1"))
ci3 <- coefci(model3, vcov. = hccm(model3, type = "hc1"))

model4 <- lm(Freq ~ south + black_person + police + drugs:police + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model4_hc1 <- coeftest(model4, vcov. = hccm(model4, type = "hc1"))
ci4 <- coefci(model4, vcov. = hccm(model4, type = "hc1"))

model5 <- lm(Freq ~ south + black_person + police + alcohol:police + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model5_hc1 <- coeftest(model5, vcov. = hccm(model5, type = "hc1"))
ci5 <- coefci(model5, vcov. = hccm(model5, type = "hc1"))

model6 <- lm(Freq ~ south + black_person + police + alcohol:black_person + male + west + north + guns + alcohol + prisoners + income + drugs, data = x)
model6_hc1 <- coeftest(model6, vcov. = hccm(model6, type = "hc1"))
ci6 <- coefci(model6, vcov. = hccm(model6, type = "hc1"))

stargazer( model21,model2, model3, model4, model5, model6,
           type = "latex",
           column.labels = c("Linear", "1", "2", "3", "4", "5"),
           dep.var.caption = "",
           se = list( model_hc121[, 2], model_hc12[, 2], model3_hc1[, 2], model4_hc1[, 2], model5_hc1[, 2], model6_hc1[, 2]),
           omit.stat = c("rsq", "f", "ser"),
           ci = TRUE, ci.custom = list(ci2, ci, ci3, ci4,ci5, ci6),
           no.space = TRUE,
           font.size = "tiny", 
           out="filename.html")

linearHypothesis(model2,  c("south = 0", "police = 0", "south:police = 0"), vcov = hccm(model2, type = "hc1"))

linearHypothesis(model3,  c("west = 0", "income = 0", "west:income = 0"), vcov = hccm(model2, type = "hc1"))

linearHypothesis(model4,  c("police = 0", "drugs = 0", "police:drugs = 0"), vcov = hccm(model2, type = "hc1"))

linearHypothesis(model5,  c("police = 0", "alcohol = 0", "police:alcohol = 0"), vcov = hccm(model2, type = "hc1"))

linearHypothesis(model6,  c("black_person = 0", "alcohol = 0", "black_person:alcohol	 = 0"), vcov = hccm(model2, type = "hc1"))


#СТВОРЕННЯ НОВИХ ІНДИКАТОРНИХ ЗМІННИХ
x <- x %>% mutate(new_guns = guns >= mean(x$guns), new_alcohol = alcohol >= mean(x$alcohol))

model_basic <- lm(Freq ~ guns + alcohol + south + black_person + police + male + west + north +  prisoners + income + drugs, data = x)
model_inter_hc1_basic <- coeftest(model_basic, vcov. = hccm(model_basic, type = "hc1"))
ci1 <- coefci(model_basic, vcov. = hccm(model_basic, type = "hc1"))

model_inter <- lm(Freq ~ new_guns + new_alcohol + south + black_person + police + male + west + north +  prisoners + income + drugs, data = x)
model_inter_hc1 <- coeftest(model_inter, vcov. = hccm(model_inter, type = "hc1"))
ci2 <- coefci(model_inter, vcov. = hccm(model_inter, type = "hc1"))

model_inter_full <- lm(Freq ~ new_guns + new_alcohol + new_guns:new_alcohol + south + black_person + police + male + west + north +  prisoners + income + drugs, data = x)
model_inter_full_hc1 <- coeftest(model_inter_full, vcov. = hccm(model_inter_full, type = "hc1"))
ci3 <- coefci(model_inter_full, vcov. = hccm(model_inter_full, type = "hc1"))

stargazer(model_basic, model_inter, model_inter_full,
          type = "latex",
          se = list(model_inter_hc1_basic[, 2], model_inter_hc1[, 2], model_inter_full_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci1, ci2, ci3),
          digits = 3, 
          out="filename.html")

linearHypothesis(model_inter,  c("new_gunsTRUE = 0", "new_alcoholTRUE = 0"), vcov = hccm(model_inter, type = "hc1"))
linearHypothesis(model_inter_full,  c("new_gunsTRUE = 0", "new_alcoholTRUE = 0", " new_gunsTRUE:new_alcoholTRUE = 0"), vcov = hccm(model_inter_full, type = "hc1"))



#БЕЗ КОНТРОЛЬНИХ\З КОНТРОЛЬНИМИ ЗМІНВМИ\МОДИФІКОВАНА РЕГРЕСІЙНА МОДЕЛЬ
model1 <- lm(Freq ~ male + south + west + north , data = x)
model_hc11 <- coeftest(model1, vcov. = hccm(model1, type = "hc1"))
ci1 <- coefci(model1, vcov. = hccm(model1, type = "hc1"))

model2 <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))
ci2 <- coefci(model2, vcov. = hccm(model2, type = "hc1"))

model3 <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + income + drugs + I(drugs^2) + I(log(guns)) + west:income, data = x)
model_hc13 <- coeftest(model3, vcov. = hccm(model3, type = "hc1"))
ci3 <- coefci(model3, vcov. = hccm(model3, type = "hc1"))


stargazer( model1,model2, model3, 
          type = "latex",
          se = list(model_hc11[, 2], model_hc12[, 2], model_hc13[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          ci = TRUE, ci.custom = list(ci1, ci2, ci3),
          digits = 3, 
          out="filename.html")

model1 <- lm(Freq ~ male + south + west + north , data = x)
model_hc11 <- coeftest(model1, vcov. = hccm(model1, type = "hc0"))
ci1 <- coefci(model1, vcov. = hccm(model1, type = "hc0"))

model2 <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + guns + income + drugs, data = x)
model_hc12 <- coeftest(model2, vcov. = hccm(model2, type = "hc0"))
ci2 <- coefci(model2, vcov. = hccm(model2, type = "hc0"))

model3 <- lm(Freq ~ male + south + west + north + black_person + police + alcohol + prisoners + income + drugs + I(drugs^2) + I(log(guns)) + west:income, data = x)
model_hc13 <- coeftest(model3, vcov. = hccm(model3, type = "hc0"))
ci3 <- coefci(model3, vcov. = hccm(model3, type = "hc0"))


stargazer( model1,model2, model3, 
           type = "latex",
           se = list(model_hc11[, 2], model_hc12[, 2], model_hc13[, 2]),
           omit.stat = c("rsq", "f", "ser"),
           ci = TRUE, ci.custom = list(ci1, ci2, ci3),
           digits = 3, 
           out="filename.html")


#Кластирризація похибок
model <- feols(Freq ~  male + south + west + north | location, data = x)

model1 <- feols(Freq ~ male + black_person + police + alcohol + prisoners + income + drugs + guns | location, data = x)

model_fe <- feols(Freq ~ male + black_person + police + alcohol + prisoners + income + drugs + I(drugs^2) + I(log(guns)) + west:income  | location, data = x)

modelsummary(list("Модель без контрольних змінних" = model, "З контрольними змінними"=model1, "МОДИФІКОВАНА МОДЕЛЬ" = model_fe),
             stars = TRUE,
             gof_omit = "^(?!Num.Obs.|R2 Adj.)")|>
  kable_styling(font_size = 15)

