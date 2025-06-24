# ------------------------------------------------------------------------------
# Exploratory Analysis
# ------------------------------------------------------------------------------

# read municipal data
df = read_rds("./data/clean data/muni.rds")

# check total number of municipalities
dim(
  df %>%
    count(cod_dane)
  )
# dim output is 1118, 2 indicating that there are 1118 municipalities recorded.

# check if any municipality-year data is duplicated/ not unique
df %>%
  count(cod_dane, year) %>%
  filter(n > 1)
# output is a 0x3 tibble indicating that all municipality-years are unique.


# check if municipalities have consistent time series data per municipality
df %>%
  count(cod_dane) %>%
  filter(n != 13)

dim(
  df %>%
    count(cod_dane) %>%
    filter(n != 13)
)
# dim output is 61, 2 indicating that 61 municipalities do not have consistent
# time series data.

# check year range of data
table(df$year)
# data is recorded between 1993 and 2005

# sanity check presence of palm oil in municipality variable 
table(df$palm_presence) 
# 761 municipal-year level observations include at least one active palm oil 
# plantation. 

# Tests on logged amount of palm planted each year (lnatprod)
summary(df$lnatprod)

df %>% 
  group_by(cod_dane) %>% 
  summarize(v = var(lnatprod, na.rm = TRUE)) %>% 
  filter(v == 0)

df %>% 
  group_by(year) %>% 
  summarize(v = var(lnatprod, na.rm = TRUE)) %>% 
  filter(v == 0)

# Tests on if a municipality has historically had paramilitary presence 
# (auc_dummy)
table(df$auc_dummy)

df %>% 
  group_by(cod_dane) %>% 
  summarize(v = var(auc_dummy, na.rm = TRUE)) %>% 
  filter(v == 0)

df %>% 
  group_by(year) %>% 
  summarize(v = var(auc_dummy, na.rm = TRUE)) %>% 
  filter(v == 0)

# Tests on historical levels of left-wingn guerrilla presence (farc_dummy)
table(df$farc_dummy)

df %>% 
  group_by(cod_dane) %>% 
  summarize(v = var(farc_dummy, na.rm = TRUE)) %>% 
  filter(v == 0)

df %>% 
  group_by(year) %>% 
  summarize(v = var(farc_dummy, na.rm = TRUE)) %>% 
  filter(v == 0)

# Test for treatment change
df %>%
  arrange(cod_dane, year) %>%
  group_by(cod_dane) %>%
  mutate(next_pp = lead(palm_presence)) %>%
  summarize(drops = any(palm_presence == 1 & next_pp == 0, 
                        na.rm = TRUE)) %>%
  ungroup() %>% 
  summarize(n_municipalities = sum(drops))
# No municipality stops treatment after treatment has been administered

# Test for distribution of treatment start
df %>%
  filter(palm_presence == 1) %>%
  group_by(cod_dane) %>%
  summarize(first_1 = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  count(first_1) %>%
  arrange(first_1)
