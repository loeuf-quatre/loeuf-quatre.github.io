setwd('/Users/edwardgivens/Git/loeuf-quatre.github.io/projects/age-adjusted-covid-by-state')

covid <- read.csv('./data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv')

cohorts <- c(
  '0-17 years', '18-29 years', '30-39 years', '40-49 years', 
  '50-64 years', '65-74 years', '75-84 years', '85 years and over'
)

covid <- covid %>%
  filter(
    Group == 'By Total' & # Total counts
    State != 'United States' & # States only
    Sex == 'All Sexes' & # Total population
    Age.Group %in% cohorts
  )

names(covid) <- janitor::make_clean_names(names(covid))

covid[is.na(covid$covid_19_deaths), 'covid_19_deaths'] <- 9

# Census data. 85+ classified as one group
pop <- read.csv('./data/sc-est2019-agesex-civ.csv')

pop <- pop %>%
  filter(
    SEX == 0 & # Total population
    SUMLEV == 40 & # State granules
    AGE != 999 # Strip out total population count
  )

names(pop) <- janitor::make_clean_names(names(pop))

# Map ages into bins
pop$age_group <- ifelse(
  between(pop$age, 0, 17),
  '0-17 years',
  ifelse(
    between(pop$age, 18, 29),
    '18-29 years',
    ifelse(
      between(pop$age, 30, 39),
      '30-39 years',
      ifelse(
        between(pop$age, 40, 49),
        '40-49 years',
        ifelse(
          between(pop$age, 50, 64),
          '50-64 years',
          ifelse(
            between(pop$age, 65, 74),
            '65-74 years',
            ifelse(
              between(pop$age, 75, 84),
              '75-84 years',
              '85 years and over'
            )
          )
        )
      )
    )
  )
)

# Merge dataframes
covid <- covid %>%
  select(
    state,
    age_group,
    covid_19_deaths
  ) %>%
  mutate(
    state = plyr::mapvalues(state, from = 'New York City', to = 'New York')
  ) %>%
  group_by(
    state,
    age_group
  ) %>%
  summarize(
    covid_19_deaths = sum(covid_19_deaths)
  )

pop <- pop %>%
  group_by(
    name,
    age_group
  ) %>%
  summarize(
    population = sum(popest2019_civ),
    .groups = 'drop'
  ) %>%
  rename(
    state = name
  )

df <- inner_join(covid, pop, by = c('state', 'age_group'))


# Visualization ---------------------------------------------------------------

ggplot() +
  geom_bar(
    data = pop[pop$NAME == 'United States' & pop$AGE < 100, ],
    aes(
      x = AGE,
      y = ifelse(SEX == 0, POPEST2019_CIV * -1, POPEST2019_CIV)
    ),
    stat = 'identity'
  )