# read data
df = read_rds("../data/raw data/muni.rds")

# reformat columns
df <- df %>%
      zap_labels() %>% 
      mutate(year = as.numeric(year))

write_rds(df, "../data/clean data/muni.rds")
