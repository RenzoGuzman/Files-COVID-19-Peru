library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(magrittr)
library(gridExtra)
options(scipen = 999)
api_result <- GET('https://covidtracking.com/api/states/daily')
text_result <- content(api_result, 'text')
result <- as_tibble(fromJSON(text_result, flatten = TRUE))

start <- as.Date('2020-03-09')
end <- as.Date(today())

filtered <- TRUE

#filtered <- FALSE
daily <- result %>% 
  mutate(
    date = as.Date(parse_date_time(date, orders = 'ymd'))
  )

if(filtered) {
  daily %<>%
    filter(state %in% c('CA', 'WA', 'TX', 'NY', 'UT', 'CO', 'AZ'))
}

result <- read_csv('/Users/renzomauricioguzmananaya/Desktop/COVID-19 R/population.csv')
colnames(result) <- c(
  'state_name', 'state', 'pop2019', 'pop2010', 'pct_change', 'abs_change',
  'house_reps', 'pop_per_electoral_vote', 'pop2019_per_house_rep',
  'pop2010_per_house_rep', 'pct_of_total'
)

population <- result %>% 
  select(state, pop2019)

testing <- daily %>% 
  left_join(population, by = 'state') %>% 
  mutate(
    per_capita = total / pop2019,
    pct_positive = positive / total
  )

total <- testing %>% 
  filter(date == as.Date(today() - days(1))) %>% 
  arrange(desc(per_capita))

plot_tests_pc <- ggplot(testing, aes(x = date, y = per_capita)) + 
  geom_line(aes(color = factor(state)), size = 1) +
  xlab('') +
  ylab('') +
  ggtitle('Covid-19 Tests Per Capita') +
  labs(color = 'State') +
  scale_x_date(
    date_labels = '%m-%d', date_breaks = '3 days',
    limit = c(as.Date(start), as.Date(end))
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot_tests <- ggplot(testing, aes(x = date, y = total)) + 
  geom_line(aes(color = factor(state)), size = 1) +
  xlab('') +
  ylab('') +
  ggtitle('Total Covid-19 Tests') +
  labs(color = 'State') +
  scale_x_date(
    date_labels = '%m-%d', date_breaks = '3 days',
    limit = c(as.Date(start), as.Date(end))
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot_pct_pos <- ggplot(testing, aes(x = date, y = pct_positive)) + 
  geom_line(aes(color = factor(state)), size = 1) +
  xlab('') +
  ylab('') +
  ggtitle('% Positive Covid-19 Tests') +
  labs(color = 'State') +
  scale_x_date(
    date_labels = '%m-%d', date_breaks = '3 days',
    limit = c(as.Date(start), as.Date(end))
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot_deaths <- ggplot(testing, aes(x = date, y = death)) + 
  geom_line(aes(color = factor(state)), size = 1) +
  xlab('') +
  ylab('') +
  ggtitle('Covid-19 Deaths') +
  labs(color = 'State') +
  scale_x_date(
    date_labels = '%m-%d', date_breaks = '3 days',
    limit = c(as.Date(start), as.Date(end))
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


grid.arrange(plot_tests, plot_tests_pc, plot_pct_pos, plot_deaths) 