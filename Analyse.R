# import des bibliothèques
library(tidyverse)
library(lubridate)
library(questionr)

# import de notre dataset
debt <- read_csv("debt.csv")

# recodage, creation ou suppression de certain  variable
debt$Country <- as.factor(debt$Country)

debt <- debt %>% 
  select(-...1)

debt <- debt %>% 
  mutate(
    Year_input = make_date(year = Year)
  )

# récrire le dataset sur l'espace de travail
write_csv2(debt, "data_debt.csv")


