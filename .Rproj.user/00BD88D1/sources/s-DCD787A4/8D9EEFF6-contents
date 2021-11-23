
#update gmdacr
#devtools::install_github("araupontones/gmdacr")
library(dplyr)
library(gmdacr)
library(rio)
library(tidyr)
library(janitor)


infile <- file.path("data/census.xlsx")
exfile <- file.path("data/quotas_5perc_error.xlsx")


census <- import(infile)


# 1 transform data and count population by strata

names(census)

censusLong <- census |>
  select(-c(Group, Total)) |>
  pivot_longer(-Group_10,
               names_to = "sex",
               values_to = "population") |>
  #Using age groups of 10 years and excluding younger than 15
  group_by(Group_10, sex) |>
  summarise(population = sum(population), .groups = 'drop')

View(quotas)

#2 calculate quotas Assuming a margin of error of 5% (0.05) and a confidence interval of 95%
quotas <- censusLong |>
  mutate(quota = gmdacr::sample_size(N = population,
                                      margin_error = .05,
                                      confidence_interval = 1.9599)
  ) |>
  #add a row with totals
  janitor::adorn_totals("row")


#check total
quotas$quota[quotas$Group_10 == "Total"]

#based on the parameters, the total sample would be of 5,347. If a lower sample is to be defined,
# allow a larger error or reduce the confidence interval

#export ================================================================
export(quotas, exfile, overwrite = T)

