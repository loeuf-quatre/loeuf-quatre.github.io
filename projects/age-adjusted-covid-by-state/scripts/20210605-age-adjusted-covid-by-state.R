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
    SEX != 0 & # Total population
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
    covid_19_deaths = sum(covid_19_deaths),
    .groups = 'drop'
  )

pop <- pop %>%
  group_by(
    name,
    sex,
    age_group
  ) %>%
  summarize(
    population = sum(popest2019_civ),
    .groups = 'drop'
  ) %>%
  rename(
    state = name
  ) %>%
  group_by(
    state,
    age_group
  ) %>%
  summarize(
    male_population = sum(population[sex == 1]),
    population = sum(population),
    male_percentage = male_population / population
  ) %>%
  select(
    - male_population
  )

df <- inner_join(covid, pop, by = c('state', 'age_group'))
df$no_deaths <- df$population - df$covid_19_deaths

fit <- glm(
  cbind(covid_19_deaths, no_deaths) ~ age_group + relevel(state, ref = 'Florida') + male_percentage, 
  data = df, 
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