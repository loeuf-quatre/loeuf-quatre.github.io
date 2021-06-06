###############################################################################
# Purpose: Interrogate covariate-adjusted COVID fatalities by state
# Date: 06 June 2021
###############################################################################

# Housekeeping ----------------------------------------------------------------

pacman::p_load(
  dplyr,
  ggplot2,
  janitor
)

setwd('./Git/loeuf-quatre.github.io/projects/age-adjusted-covid-by-state')

# Import and tidy data --------------------------------------------------------

# State COVID fatalities ----------------------------------

covid <- read_excel('./data/datasets.xlsx', sheet = 'COVID Fatalities')

covid <- covid %>%
  clean_names()

age_cohorts <- c(
  '0-17 years', 
  '18-29 years', 
  '30-39 years', 
  '40-49 years',
  '50-64 years', 
  '65-74 years', 
  '75-84 years', 
  '85 years and over'
)

covid <- covid %>%
  filter(
    group == 'By Total' & # Total counts
    state != 'United States' & # States only
    sex == 'All Sexes' & # Both sexes
    age_group %in% age_cohorts # By age
  )

# Values < 10 are coded as nulls in the data for privacy reasons
covid[is.na(covid$covid_19_deaths), 'covid_19_deaths'] <- 1

# Recode NYC as part of Greater NY
covid <- covid %>%
  mutate(
    state = plyr::mapvalues(state, from = 'New York City', to = 'New York')
  ) %>%
  group_by(
    state,
    age_group
  ) %>%
  summarize(
    covid_19_deaths = sum(covid_19_deaths),
    .groups = 'drop'
  )

# Whittle data
covid <- covid %>%
  select(
    state,
    age_group,
    covid_19_deaths
  )

# State populations ---------------------------------------

pop <- read_excel('./data/datasets.xlsx', sheet = 'Population')

pop <- pop %>%
  clean_names()

pop <- pop %>%
  filter(
    sex == 0 & # Both sexes
    sumlev == 40 & # State granularity
    age != 999 # Strip out total state population count
  )

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

# State temperatures --------------------------------------

# State lockdowns -----------------------------------------

lockdowns <- read_excel('./data/datasets.xlsx', sheet = 'Lockdowns')

lockdowns$average_severity <- apply(lockdowns[, -1], 1, mean)



# State 2020 election -------------------------------------

# State population density --------------------------------

# State hospital beds -------------------------------------

# State GDP -----------------------------------------------

# State religiosity ---------------------------------------

# State diversity -----------------------------------------

# Analyze data ----------------------------------------------------------------

# Aggregated logistic regression --------------------------

# Join state COVID fatalities and state populations
cfp <- left_join(covid, pop, by = c('state', 'age_group'))

cfp$state <- factor(cfp$state)

# Surviving population
cfp$covid_19_alive <- cfp$population - cfp$covid_19_deaths

# Unadjusted OR by state --------------

ucfp <- cfp %>%
  group_by(
    state
  ) %>%
  summarize(
    covid_19_deaths = sum(covid_19_deaths),
    covid_19_alive = sum(covid_19_alive),
    .groups = 'drop'
  )

ufit <- glm(
  cbind(covid_19_deaths, covid_19_alive) ~ relevel(state, ref = 'Florida'), 
  data = ucfp, 
  family = 'binomial'
)

# Adjusted OR by state ----------------

afit <- glm(
  cbind(covid_19_deaths, covid_19_alive) ~ relevel(state, ref = 'Florida') + age_group, 
  data = cfp, 
  family = 'binomial'
)

# Comparison
ufit_coef <- broom::tidy(ufit)
ufit_coef$model <- 'Unadjusted'

afit_coef <- broom::tidy(afit)
afit_coef$model <- 'Adjusted'

comp <- rbind(ufit_coef, afit_coef)
comp$state <- gsub('.*)', '', comp$term)
comp <- comp[comp$state %in% cfp$state, ]

comp <- comp %>%
  group_by(
    model
  ) %>%
  mutate(
    ranking = dense_rank(estimate),
    better_than_fla = sum(exp(estimate) < 1)
  ) %>%
  group_by(
    state
  ) %>%
  mutate(
    d = diff(estimate),
    change = diff(ranking)
  ) %>%
  arrange(
    change
  )

comp$state <- factor(comp$state, unique(comp$state))

# State-level meta-regression -----------------------------


# Visualize data --------------------------------------------------------------

ggplot() +
  geom_bar(
    data = covid,
    aes(
      x = state,
      y = covid_19_deaths,
      fill = age_group
    ),
    stat = 'identity',
    position = 'fill'
  ) +
  theme_minimal(
    base_size = 24
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

















covid <- read.csv('./data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv')

# cohorts <- c(
#   '0-17 years', '18-29 years', '30-39 years', '40-49 years', 
#   '50-64 years', '65-74 years', '75-84 years', '85 years and over'
# )

covid <- covid %>%
  filter(
    Group == 'By Total' & # Total counts
    State != 'United States' & # States only
    Sex == 'All Sexes' & # Total population
    Age.Group %in% 'All Ages' # Total deaths
  )

names(covid) <- janitor::make_clean_names(names(covid))

# covid[is.na(covid$covid_19_deaths), 'covid_19_deaths'] <- 9

# Census data. 85+ classified as one group
pop <- read.csv('./data/sc-est2019-agesex-civ.csv')

pop <- pop %>%
  filter(
    SEX != 0 & # Total population
    SUMLEV == 40 & # State granules
    AGE != 999 # Strip out total population count
  )

names(pop) <- janitor::make_clean_names(names(pop))

# Covariates ----------------------------------------------

# 2020 election
election <- read.csv('./data/2020election.csv')

# Average temperature
temp <- read.csv('./data/avg-temp.csv')
names(temp) <- janitor::make_clean_names(names(temp))
temp$state <- stringr::str_trim(gsub('/.*', '', temp$state_population))
temp$average_temperature <- as.numeric(stringr::str_extract(temp$average_temperature, '\\d+\\.*\\d*'))

# Per capita GDP
gdp <- read.csv('./data/gdp-per-capita.csv')
names(gdp) <- janitor::make_clean_names(names(gdp))
gdp <- gdp[, c('state', 'x2018')]
gdp <- gdp %>% rename(gdppc = x2018)
gdp$state <- stringr::str_trim(gdp$state)

# Hospital beds
beds <- read.csv('./data/hospital-beds-per-capita.csv')
names(beds) <- janitor::make_clean_names(names(beds))
beds <- beds[, c('state', 'state_local_government', 'total')]
beds$state_local_government <- as.numeric(beds$state_local_government)
beds[is.na(beds)] <- 0

# Religiosity
religion <- read.csv('./data/religiosity.csv')

# Lockdowns
lockdowns <- read.csv('./data/lockdowns.csv')
names(lockdowns) <- janitor::make_clean_names(names(lockdowns))
lockdowns$avg <- apply(lockdowns[, -1], 1, mean)

# # Map ages into bins
# pop$age_group <- ifelse(
#   between(pop$age, 0, 17),
#   '0-17 years',
#   ifelse(
#     between(pop$age, 18, 29),
#     '18-29 years',
#     ifelse(
#       between(pop$age, 30, 39),
#       '30-39 years',
#       ifelse(
#         between(pop$age, 40, 49),
#         '40-49 years',
#         ifelse(
#           between(pop$age, 50, 64),
#           '50-64 years',
#           ifelse(
#             between(pop$age, 65, 74),
#             '65-74 years',
#             ifelse(
#               between(pop$age, 75, 84),
#               '75-84 years',
#               '85 years and over'
#             )
#           )
#         )
#       )
#     )
#   )
# )

# Map ages into bins
pop$age_group <- ifelse(
  between(pop$age, 0, 17),
  '0-17 years',
  ifelse(
    between(pop$age, 18, 34),
    '18-29 years',
    ifelse(
      between(pop$age, 35, 54),
      '30-39 years',
      ifelse(
        between(pop$age, 55, 74),
        '40-49 years',
        '75+'
      )
    )
  )
)

# Merge dataframes
covid <- covid %>%
  mutate(
    state = plyr::mapvalues(state, from = 'New York City', to = 'New York')
  ) %>%
  group_by(
    state
  ) %>%
  summarize(
    covid_19_deaths = sum(covid_19_deaths),
    .groups = 'drop'
  )

pop <- pop %>%
  group_by(
    state
  ) %>%
  mutate(
    state_male_population = sum(popest2019_civ[sex == 1]),
    state_population = sum(popest2019_civ),
    state_male_percentage = state_male_population / state_population
  ) %>%
  group_by(
    state,
    age_group
  ) %>%
  mutate(
    state_age_population = sum(popest2019_civ),
    state_age_percentage = state_age_population / state_population
  ) %>%
  ungroup() %>%
  select(
    state = name,
    age_group,
    state_age_percentage,
    state_male_percentage,
    state_population
  ) %>%
  distinct() %>%
  pivot_wider(
    names_from = age_group,
    values_from = state_age_percentage
  ) %>%
  janitor::clean_names()
  
df <- inner_join(covid, pop, by = 'state')
df$no_deaths <- df$state_population - df$covid_19_deaths

df$state <- factor(df$state, unique(df$state))

fit <- glm(
  cbind(covid_19_deaths, no_deaths) ~ relevel(state, ref = 'Florida'), 
  data = df, 
  family = 'binomial'
)

fit <- glm(
  cbind(covid_19_deaths, no_deaths) ~ state + x75, 
  data = df, 
  family = 'binomial'
)




fit <- glm(
  cbind(covid_19_deaths, no_deaths) ~ state/dem_percent + age_group, 
  data = df[1:20, ], 
  family = 'binomial'
)

coeff <- broom::tidy(fit) %>% 
  select(
    term, 
    estimate
  ) %>% 
  mutate(
    estimate = round(exp(estimate), 2), # Exponentiate for OR
    state = gsub('.*)', '', term) # Extract state
  ) %>%
  filter(
    state %in% df$state
  )

coeff <- coeff[order(coeff$estimate), ]

coeff$state <- factor(coeff$state, coeff$state)

# Visualization ---------------------------------------------------------------

ggplot() +
  geom_point(
    data = coeff,
    aes(
      x = state,
      y = estimate
    )
  ) +
  geom_hline(
    yintercept = 1,
    linetype = 2
  ) +
  labs(
    title = '',
    subtitle = '',
    x = 'State',
    y = 'Age-Adjusted Odds Ratio'
  ) +
  coord_flip() +
  theme_minimal(
    base_size = 36
  )

ggsave(
  filename = './output/p1.png', 
  plot = p1, 
  height = 10, 
  width = 26,
  type = 'cairo-png'
)

diversity <- read.csv('./data/wallethub-diversity-rankings.csv')

names(diversity) <- janitor::make_clean_names(names(diversity))