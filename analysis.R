
# PHW interview preparation
# Script 2
# Analysis of the last 10 years of NHS Wales expenditure
# Jennifer Grant
# 29/01/25

################################################################################
# Slide 6 graph
# Line graph of total expenditure over time

datalong %>% filter(X.1 == "Total" & year >= 2013) %>% 
  ggplot(mapping = aes(x = year, y = (expenditure/1000))) + 
  geom_point(size = 2.5) +
  geom_line(linewidth = 0.75) +
  scale_y_continuous(limits = c(0, 10), breaks = waiver(), n.breaks = 6) +
  theme_minimal() +
  ggtitle("Annual NHS Wales Expenditure from 2013-14 to 2022-23") +
  xlab("Financial Year Beginning") +
  ylab("Expenditure (£ billion)") +
  scale_x_discrete(limits=c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  theme(text = element_text(size = 14))


################################################################################
# Slide 7 graph
# Bar chart gif of health board expenditure over time

gif <- hbspend %>% 
  #filter(year == "2022") %>%
  mutate(X = fct_reorder(X, desc(expenditure))) %>%
  ggplot(mapping = aes(x = X, y = (expenditure/1000000))) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  theme_minimal() +
  theme(text = element_text(size = 30)) +
  coord_flip() +
  # gganimate specific bits:
  labs(title = 'Local Health Board Annual Expenditure',
       subtitle = 'in Financial Year Beginning {round(frame_time,0)}', 
       x = 'Health Board', y = 'Expenditure (£ billion)') +
  transition_time(year) +
  ease_aes('linear') 

animate(gif, height = 1000, width = 1000)
#anim_save("healthboardspend.gif")


################################################################################
# Slide 8 graph
# Bar chart gif of health board expenditure per head over time

#hbperhead$X <- factor(hbperhead$X, levels = hbperhead$X)

gifperhead <- hbperhead %>%
  #mutate(hb = fct_reorder(hb, desc(expenditure_per_head))) %>%
  ggplot(mapping = aes(x = X, y = expenditure)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  theme_minimal() +
  xlab("Health Board") +
  ylab("Expenditure Per Head (£)") +
  #theme(axis.text.x = element_text(angle=45, vjust = 0.7, hjust = 0.5)) +
  theme(text = element_text(size = 30)) +
  coord_flip() +
  # gganimate specific bits:
  labs(title = 'Local Health Board Expenditure Per Head',
       subtitle = 'in Financial Year Beginning {round(frame_time,0)}', 
       x = 'Health Board', y = 'Expenditure (£)') +
  transition_time(year) +
  ease_aes('linear') 

animate(gifperhead, height = 1000, width = 1000)
#anim_save("healthboardperhead.gif")


################################################################################
# Slide 9 graph
# Faceted area graph of percentage share of annual expenditure across categories

percentagelong %>% 
  filter(X.1 != "Total" & X.1 == X.2) %>%
  mutate(X.1 = fct_reorder(X.1, desc(expenditure))) %>%
  ggplot(mapping = aes(x = year, y = expenditure, fill = X.1)) +
  geom_area(alpha = 0.7, fill = "dodgerblue") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    text = element_text(size = 14)) +
  ggtitle("Percentage of NHS Wales Expenditure across Categories from 2013-14 to 2022-23") +
  xlab("Financial Year Beginning") +
  ylab("% of Annual Total Expenditure") +
  scale_x_discrete(limits=c(2013, 2015, 2017, 2019, 2021)) +
  facet_wrap(~X.1, ncol = 4)


################################################################################
# Slide 10 graph
# Faceted area graph of annual expenditure across categories

datalong %>% filter(X.1 != "Total" & X.1 == X.2 & year >= 2013) %>%
  mutate(X.1 = fct_reorder(X.1, desc(expenditure))) %>%
  ggplot(mapping = aes(x = year, y = expenditure)) +
  #geom_line() +
  geom_area(alpha=0.7, fill = "dodgerblue") +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    text = element_text(size = 14)
  ) +
  ggtitle("NHS Wales Expenditure across Disease Categories from 2013-14 to 2022-23") +
  xlab("Financial Year Beginning") +
  ylab("Expenditure (£million)") +
  scale_x_discrete(limits=c(2013, 2015, 2017, 2019, 2021)) +
  facet_wrap(~X.1, ncol = 4)
  

################################################################################
# Slide 11 graph
# Ten year percentage change in spending across categories 

# Filter data for totals in 2013 and 2022
tenyeardf <- datalong %>% 
  filter(X.1 == X.2, X.1 != "Total", (year == 2013 | year == 2022))

# Data frame of only 2013 data
tenyearchange <- data.frame(category = tenyeardf[1:23,1])

# Create new column of percentage change from 2013-14 to 2022-23
for (i in 1:(nrow(tenyeardf)/2)) {
  tenyearchange$expenditure[i] <- ((tenyeardf$expenditure[i+23] - tenyeardf$expenditure[i]) / 
                                     tenyeardf$expenditure[i]) * 100
}

# Plot
tenyearchange %>%
  mutate(category = fct_reorder(category, desc(expenditure))) %>%
  ggplot(mapping = aes(x = category, y = expenditure)) +
  geom_bar(stat="identity", fill = "steelblue") + 
  coord_flip() +
  ylab("Percentage Change in Expenditure") +
  xlab("Category") +
  theme_bw() +
  ggtitle("Percentage Change in Expenditure from 2013-14 to 2022-23") +
  theme(text = element_text(size = 14))


################################################################################
# Slide 11 graph
# One year percentage change in spending across categories 

# Filter data for totals in 2021 and 2022
oneyeardf <- datalong %>% 
  filter(X.1 == X.2, X.1 != "Total", (year == 2021 | year == 2022))

# Data frame of only 2021 data
oneyearchange <- data.frame(category = oneyeardf[1:23,1])

# Create new column of percentage change from 2021-22 to 2022-23
for (i in 1:(nrow(oneyeardf)/2)) {
  oneyearchange$expenditure[i] <- ((oneyeardf$expenditure[i+23] - oneyeardf$expenditure[i]) / 
                                     oneyeardf$expenditure[i]) * 100
}

# Plot
oneyearchange %>%
  mutate(category = fct_reorder(category, desc(expenditure))) %>%
  ggplot(mapping = aes(x = category, y = expenditure)) +
  geom_bar(stat="identity", fill = "steelblue") + 
  coord_flip() +
  ylab("Percentage Change in Expenditure") +
  xlab("Category") +
  ylim(-35, 35) +
  theme_bw() +
  ggtitle("Percentage Change in Expenditure from 2021-22 to 2022-23") +
  theme(text = element_text(size = 14))


################################################################################
# Unused graph
# Area graph of percentage share of annual expenditure across health boards

hbpercent <- hbspend %>%
  group_by(year, X) %>%
  summarise(n = sum(expenditure)) %>%
  mutate(percentage = n / sum(n) * 100)

hbpercent %>%
  ggplot(mapping = aes(x = year, y = percentage, fill = X)) +
  geom_area() +
  scale_x_discrete(limits = c(2013, 2015, 2017, 2019, 2021)) +
  ggtitle("Percentage of Annual Budget Spent by Each Health Board") +
  xlab("Financial Year Beginning") +
  ylab("Percentage of Total Expenditure") +
  labs(fill = "Health Board")

