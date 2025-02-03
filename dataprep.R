
# PHW interview preparation
# Script 1
# Data importing and formatting
# Jennifer Grant
# 29/01/25

################################################################################
# Load useful packages
library(tidyverse)
library(ggplot2)
library(gganimate)

################################################################################
# Expenditure data

# Load data
data2009to2020 <- read.csv('./data/2009_to_2020.csv', na.strings=c("","NA"))
data2020to2023 <- read.csv('./data/2020_to_2023.csv', na.strings=c("","NA"))

#view(data2009to2020)
#view(data2020to2023)

# Fill in blank labels of 2009-2020 data
# ... for the X.1 column
for (i in 2:nrow(data2009to2020)) {
  if(is.na(data2009to2020$X.1[[i]])) {
    data2009to2020$X.1[i] <- data2009to2020$X.1[i-1]
  } 
}

# ... for the X.2 column
for (i in 1:nrow(data2009to2020)) {
  if(is.na(data2009to2020$X.2[[i]])) {
    data2009to2020$X.2[[i]] <- data2009to2020$X.1[[i]]
  } 
}

# Shorten long category names in X.1 column
for (i in 4:nrow(data2009to2020)) {
  if (data2009to2020$X.1[[i]] == "Genito Urinary system disorders (exc infertility) ") {
    data2009to2020$X.1[[i]] <- "Genitourinary system disorders"
  } 
  else if (data2009to2020$X.1[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    data2009to2020$X.1[[i]] <- "Musculoskeletal system problems"
  }
  else if (data2009to2020$X.1[[i]] == "Healthy individuals (includes Screening) ") {
    data2009to2020$X.1[[i]] <-  "Healthy individuals"
  }
  else if (data2009to2020$X.1[[i]] == "Trauma & injuries (inc burns) ") {
    data2009to2020$X.1[[i]] <- "Trauma & injuries"
  }
  else if (data2009to2020$X.1[[i]] == "Endocrine, nutritional & metabolic problems ") {
    data2009to2020$X.1[[i]] <- "Endocrine, nutrition & metabolism"
  }
}

# Shorten long category names in X.2 column
for (i in 4:nrow(data2009to2020)) {
  if (is.na(data2009to2020$X.2[[i]]) == FALSE & 
      data2009to2020$X.2[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    data2009to2020$X.2[[i]] <- "Musculoskeletal system problems"
  }
  else if (is.na(data2009to2020$X.2[[i]]) == FALSE &
           data2009to2020$X.2[[i]] == "Healthy individuals (includes Screening) ") {
    data2009to2020$X.2[[i]] <-  "Healthy individuals"
  }
  else if (is.na(data2009to2020$X.2[[i]]) == FALSE &
           data2009to2020$X.2[[i]] == "Trauma & injuries (inc burns) ") {
    data2009to2020$X.2[[i]] <- "Trauma & injuries"
  }
  else if (is.na(data2009to2020$X.2[[i]]) == FALSE &
           data2009to2020$X.2[[i]] == "Endocrine, nutritional & metabolic problems ") {
    data2009to2020$X.2[[i]] <- "Endocrine, nutrition & metabolism"
  }
  else if (is.na(data2009to2020$X.2[[i]]) == FALSE &
           data2009to2020$X.2[[i]] == "Genito Urinary system disorders (exc infertility) ") {
    data2009to2020$X.2[[i]] <- "Genitourinary system disorders"
  } 
}


# Fill in blank labels of 2009-2020 data
# ... for the X.1 column
for (i in 4:nrow(data2020to2023)) {
  if(is.na(data2020to2023$X.1[[i]])) {
    data2020to2023$X.1[i] <- data2020to2023$X.1[i-1]
  } 
}

# ... for the X.2 column
for (i in 3:nrow(data2020to2023)) {
  if(is.na(data2020to2023$X.2[[i]])) {
    data2020to2023$X.2[[i]] <- data2020to2023$X.1[[i]] 
  } 
}

# Shorten long category names in X.1 column
for (i in 4:nrow(data2020to2023)) {
  if (is.na(data2020to2023$X.1[[i]]) == FALSE & 
      data2020to2023$X.1[[i]] == "Genito Urinary system disorders (exc infertility) ") {
    data2020to2023$X.1[[i]] <- "Genitourinary system disorders"
  } 
  else if (is.na(data2020to2023$X.1[[i]]) == FALSE & 
           data2020to2023$X.1[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    data2020to2023$X.1[[i]] <- "Musculoskeletal system problems"
  }
  else if (is.na(data2020to2023$X.1[[i]]) == FALSE & 
           data2020to2023$X.1[[i]] == "Healthy individuals (includes Screening) ") {
    data2020to2023$X.1[[i]] <-  "Healthy individuals"
  }
  else if (is.na(data2020to2023$X.1[[i]]) == FALSE & 
           data2020to2023$X.1[[i]] == "Trauma & injuries (inc burns) ") {
    data2020to2023$X.1[[i]] <- "Trauma & injuries"
  }
  else if (is.na(data2020to2023$X.1[[i]]) == FALSE & 
           data2020to2023$X.1[[i]] == "Endocrine, nutritional & metabolic problems ") {
    data2020to2023$X.1[[i]] <- "Endocrine, nutrition & metabolism"
  }
}

# Shorten long category names in X.2 column
for (i in 4:nrow(data2020to2023)) {
  if (is.na(data2020to2023$X.2[[i]]) == FALSE & 
      data2020to2023$X.2[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    data2020to2023$X.2[[i]] <- "Musculoskeletal system problems"
  }
  else if (is.na(data2020to2023$X.2[[i]]) == FALSE &
           data2020to2023$X.2[[i]] == "Healthy individuals (includes Screening) ") {
    data2020to2023$X.2[[i]] <-  "Healthy individuals"
  }
  else if (is.na(data2020to2023$X.2[[i]]) == FALSE &
           data2020to2023$X.2[[i]] == "Trauma & injuries (inc burns) ") {
    data2020to2023$X.2[[i]] <- "Trauma & injuries"
  }
  else if (is.na(data2020to2023$X.2[[i]]) == FALSE & 
           data2020to2023$X.2[[i]] == "Genito Urinary system disorders (exc infertility) ") {
    data2020to2023$X.2[[i]] <- "Genitourinary system disorders"
  } 
  else if (is.na(data2020to2023$X.2[[i]]) == FALSE & 
           data2020to2023$X.2[[i]] == "Endocrine, nutritional & metabolic problems ") {
    data2020to2023$X.2[[i]] <- "Endocrine, nutrition & metabolism"
  }
}

# Format category names for rows containing the total
data2009to2020 <- data2009to2020 %>%
  mutate(X.2=ifelse(is.na(X.1) & (X == "Total "), "Total", X.2)) %>%
  mutate(X.1=ifelse(is.na(X.1) & (X == "Total "), "Total", X.1)) %>%
  select(-X)

data2020to2023 <- data2020to2023 %>%
  mutate(X.2=ifelse(is.na(X.1) & (X == "Total "), "Total", X.2)) %>%
  mutate(X.1=ifelse(is.na(X.1) & (X == "Total "), "Total", X.1)) %>%
  select(-X)

# Link the 2009-2020 data to the 2020-2023 dataset
link <- data2020to2023 %>% select(X.1, X.2, X.5, X.8, X.11)
link <- link[-c(1,2),]
data <- merge(data2009to2020, link, by=c("X.1", "X.2"), all.x=TRUE)
data <- data %>% rename('2009' = X2009.10.,
                        '2010' = X2010.11.,
                        '2011' = X2011.12.,
                        '2012' = X2012.13.,
                        '2013' = X2013.14.,
                        '2014' = X2014.15.,
                        '2015' = X2015.16.,
                        '2016' = X2016.17.,
                        '2017' = X2017.18.,
                        '2018' = X2018.19., 
                        '2019' = X2019.20..4., 
                        '2020' = X.5,
                        '2021' = X.8, 
                        '2022' = X.11)
#view(data)

# Convert wide into long data
datalong <- data %>% gather(year, expenditure, '2009':'2022')
datalong$year <- as.numeric(datalong$year)
datalong$expenditure <- as.numeric(datalong$expenditure)
datalong$expenditure <- (datalong$expenditure*1000) / 1000000

# Remove useless data
rm(data2009to2020, data2020to2023, data, link)

################################################################################
# Expenditure per cent data

# Load data
percentagedata <- read.csv('./data/percentage_all.csv', na.strings=c("","NA"))

# Fill in blank labels in X.1 column
for (i in 2:nrow(percentagedata)) {
  if(is.na(percentagedata$X.1[[i]])) {
    percentagedata$X.1[i] <- percentagedata$X.1[i-1]
  } 
}

# Shorten long category names in X.1 column
for (i in 4:nrow(percentagedata)) {
  if(percentagedata$X.1[[i]] == "Genito Urinary system disorders (exc infertility) ") {
    percentagedata$X.1[[i]] <- "Genitourinary system disorders"
  } 
  else if (percentagedata$X.1[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    percentagedata$X.1[[i]] <- "Musculoskeletal system problems"
  }
  else if (percentagedata$X.1[[i]] == "Healthy individuals (includes Screening) ") {
    percentagedata$X.1[[i]] <-  "Healthy individuals"
  }
  else if (percentagedata$X.1[[i]] == "Trauma & injuries (inc burns) ") {
    percentagedata$X.1[[i]] <- "Trauma & injuries"
  }
  else if (percentagedata$X.1[[i]] == "Endocrine, nutritional & metabolic problems ") {
    percentagedata$X.1[[i]] <- "Endocrine, nutrition & metabolism"
  }
}

# Shorten long category names in X.1 column
for (i in 4:nrow(percentagedata)) {
  if (is.na(percentagedata$X.2[[i]]) == FALSE & 
      percentagedata$X.2[[i]] == "Musculo skeletal system problems (exc Trauma) ") {
    percentagedata$X.2[[i]] <- "Musculoskeletal system problems"
  }
  else if (is.na(percentagedata$X.2[[i]]) == FALSE &
           percentagedata$X.2[[i]] == "Healthy individuals (includes Screening) ") {
    percentagedata$X.2[[i]] <-  "Healthy individuals"
  }
  else if (is.na(percentagedata$X.2[[i]]) == FALSE &
           percentagedata$X.2[[i]] == "Trauma & injuries (inc burns) ") {
    percentagedata$X.2[[i]] <- "Trauma & injuries"
  }
}

# Fill in blank labels in X.2 column
for (i in 2:nrow(percentagedata)) {
  if(is.na(percentagedata$X.2[[i]])) {
    percentagedata$X.2[[i]] <- percentagedata$X.1[[i]]
  } 
}

# Format category names for rows containing the total
percentagedata <- percentagedata %>%
  mutate(X.2=ifelse(is.na(X.1) & (X == "Total "), "Total", X.2)) %>%
  mutate(X.1=ifelse(is.na(X.1) & (X == "Total "), "Total", X.1)) %>%
  select(-X)

# Reformat the data
percentagedata <- percentagedata %>% 
  select(-c(X2012.13., X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  rename("2013" = X2013.14.,
         "2014" = X2014.15.,
         "2015" = X2015.16.,
         "2016" = X2016.17.,
         "2017" = X2017.18.,
         "2018" = X2018.19.,
         "2019" = X2019.20..4.,
         "2020" = X.5,
         "2021" = X.8,
         "2022" = X.11)
percentagedata <- percentagedata[-c(1,2),] 

# Convert wide into long data
percentagelong <- percentagedata %>% gather(year, expenditure, '2013':'2022')
percentagelong$year <- as.numeric(percentagelong$year)
percentagelong$expenditure <- as.numeric(percentagelong$expenditure)

# Remove useless data
rm(percentagedata)

################################################################################
# Health board expenditure

# Load data
aneurin <- read.csv('./data/aneurin.csv', na.strings=c("","NA"))
# Reformat data
aneurin <- aneurin %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Aneurin Bevan") %>% 
  gather(year, expenditure, '2013':'2022')
aneurin$year <- as.integer(aneurin$year)
aneurin$expenditure <- as.numeric(aneurin$expenditure)

# Load data
betsi <- read.csv('./data/betsi.csv', na.strings = c("", "NA"))
# Reformat data
betsi <- betsi %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Betsi Cadwaladr") %>% 
  gather(year, expenditure, '2013':'2022')
betsi$year <- as.integer(betsi$year)
betsi$expenditure <- as.numeric(betsi$expenditure)

# Load data
cardiff <- read.csv('./data/cardiff.csv', na.strings = c("", "NA"))
# Reformat data
cardiff <- cardiff %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Cardiff and Vale") %>% 
  gather(year, expenditure, '2013':'2022')
cardiff$year <- as.integer(cardiff$year)
cardiff$expenditure <- as.numeric(cardiff$expenditure)

# Load data
cwmtaf  <- read.csv('./data/cwmtaf1.csv', na.strings = c("", "NA"))
cwmtaf2 <- read.csv('./data/cwmtaf2.csv', na.strings = c("", "NA"))
# Reformat and link data
cwmtaf  <- cwmtaf %>%
  mutate(X2019.20..4. = cwmtaf2$X2019.20..4.,
         X.5 = cwmtaf2$X.5,
         X.8 = cwmtaf2$X.8,
         X.11 = cwmtaf2$X.11) %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Cwm Taf Morgannwg") %>% 
  gather(year, expenditure, '2013':'2022')
cwmtaf$year <- as.integer(cwmtaf$year)
cwmtaf$expenditure <- as.numeric(cwmtaf$expenditure)
# Remove useless data
rm(cwmtaf2)

# Load data
hywel <- read.csv('./data/hywel.csv', na.strings = c("", "NA"))
# Reformat data
hywel <- hywel %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Hywel Dda") %>% 
  gather(year, expenditure, '2013':'2022')
hywel$year <- as.integer(hywel$year)
hywel$expenditure <- as.numeric(hywel$expenditure)

# Load data
powys <- read.csv('./data/powys.csv', na.strings = c("", "NA"))
# Reformat data
powys <- powys %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Powys") %>% 
  gather(year, expenditure, '2013':'2022')
powys$year <- as.integer(powys$year)
powys$expenditure <- as.numeric(powys$expenditure)

# Load data 
swansea  <- read.csv('./data/swansea1.csv', na.strings = c("", "NA"))
swansea2 <- read.csv('./data/swansea2.csv', na.strings = c("", "NA"))
# Reformat and link data
swansea  <- swansea %>%
  mutate(X2019.20..4. = swansea2$X2019.20..4.,
         X.5 = swansea2$X.5,
         X.8 = swansea2$X.8,
         X.11 = swansea2$X.11) %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Swansea Bay") %>% 
  gather(year, expenditure, '2013':'2022')
swansea$year <- as.integer(swansea$year)
swansea$expenditure <- as.numeric(swansea$expenditure)
# Remove useless data
rm(swansea2)

# Combine all health board expenditure data into 1 data frame
hbspend <- rbind(aneurin, betsi, cardiff, cwmtaf, hywel, powys, swansea)
# Remove  useless data
rm(aneurin, betsi, cardiff, cwmtaf, hywel, powys, swansea)

################################################################################
# Health board expenditure per head

# Load data
ph_aneurin <- read.csv('./data/phaneurin.csv', na.strings=c("","NA"))
# Reformat data
ph_aneurin <- ph_aneurin %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Aneurin Bevan") %>% 
  gather(year, expenditure, '2013':'2022')
ph_aneurin$year <- as.integer(ph_aneurin$year)
ph_aneurin$expenditure <- as.numeric(ph_aneurin$expenditure)

# Load data
ph_betsi <- read.csv('./data/phbetsi.csv', na.strings = c("", "NA"))
# Reformat data
ph_betsi <- ph_betsi %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Betsi Cadwaladr") %>% 
  gather(year, expenditure, '2013':'2022')
ph_betsi$year <- as.integer(ph_betsi$year)
ph_betsi$expenditure <- as.numeric(ph_betsi$expenditure)

# Load data
ph_cardiff <- read.csv('./data/phcardiff.csv', na.strings = c("", "NA"))
# Reformat data
ph_cardiff <- ph_cardiff %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Cardiff and Vale") %>% 
  gather(year, expenditure, '2013':'2022')
ph_cardiff$year <- as.integer(ph_cardiff$year)
ph_cardiff$expenditure <- as.numeric(ph_cardiff$expenditure)

# Load data
ph_cwmtaf  <- read.csv('./data/phcwmtaf1.csv', na.strings = c("", "NA"))
ph_cwmtaf2 <- read.csv('./data/phcwmtaf2.csv', na.strings = c("", "NA"))
# Reformat and link data
ph_cwmtaf  <- ph_cwmtaf %>%
  mutate(X2019.20..4. = ph_cwmtaf2$X2019.20..4.,
         X.5 = ph_cwmtaf2$X.5,
         X.8 = ph_cwmtaf2$X.8,
         X.11 = ph_cwmtaf2$X.11) %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Cwm Taf Morgannwg") %>% 
  gather(year, expenditure, '2013':'2022')
ph_cwmtaf$year <- as.integer(ph_cwmtaf$year)
ph_cwmtaf$expenditure <- as.numeric(ph_cwmtaf$expenditure)
rm(ph_cwmtaf2)

# Load data
ph_hywel <- read.csv('./data/phhywel.csv', na.strings = c("", "NA"))
# Reformat data
ph_hywel <- ph_hywel %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Hywel Dda") %>% 
  gather(year, expenditure, '2013':'2022')
ph_hywel$year <- as.integer(ph_hywel$year)
ph_hywel$expenditure <- as.numeric(ph_hywel$expenditure)

# Load data
ph_powys <- read.csv('./data/phpowys.csv', na.strings = c("", "NA"))
# Reformat data
ph_powys <- ph_powys %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Powys") %>% 
  gather(year, expenditure, '2013':'2022')
ph_powys$year <- as.integer(ph_powys$year)
ph_powys$expenditure <- as.numeric(ph_powys$expenditure)

# Load data
ph_swansea  <- read.csv('./data/phswansea1.csv', na.strings = c("", "NA"))
ph_swansea2 <- read.csv('./data/phswansea2.csv', na.strings = c("", "NA"))
# Reformat and link data
ph_swansea  <- ph_swansea %>%
  mutate(X2019.20..4. = ph_swansea2$X2019.20..4.,
         X.5 = ph_swansea2$X.5,
         X.8 = ph_swansea2$X.8,
         X.11 = ph_swansea2$X.11) %>% 
  select(-c(X.2, X2020.21., X.3, X.4, X2021.22., X.6, X.7, X2022.23., X.9, X.10)) %>%
  filter(X == "Total " & is.na(X.1) == TRUE) %>%
  select(-X.1) %>% 
  rename('2013' = X2013.14.,
         '2014' = X2014.15.,
         '2015' = X2015.16.,
         '2016' = X2016.17.,
         '2017' = X2017.18.,   
         '2018' = X2018.19., 
         '2019' = X2019.20..4., 
         '2020' = X.5,
         '2021' = X.8, 
         '2022' = X.11) %>%
  mutate(X = "Swansea Bay") %>% 
  gather(year, expenditure, '2013':'2022')
ph_swansea$year <- as.integer(ph_swansea$year)
ph_swansea$expenditure <- as.numeric(ph_swansea$expenditure)
rm(ph_swansea2)

# Combine all health board expenditure per head data into 1 data frame
hbperhead <- rbind(ph_aneurin, ph_betsi, ph_cardiff, ph_cwmtaf, ph_hywel, ph_powys, ph_swansea)
# Remove useless data
rm(ph_aneurin, ph_betsi, ph_cardiff, ph_cwmtaf, ph_hywel, ph_powys, ph_swansea)