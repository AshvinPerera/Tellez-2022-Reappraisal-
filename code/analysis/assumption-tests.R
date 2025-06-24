set.seed(12345)

# ------------------------------------------------------------------------------
# bacon decomposition
# ------------------------------------------------------------------------------

# fill missing data with the previous years data for the coca, farc_attack,
# and palm_presenSe data as the presence of palm oil production is assumed to be
# constant after treatment.

df_balanced <- df %>%
  complete(cod_dane, year) %>%
  arrange(cod_dane, year) %>%
  group_by(cod_dane) %>%
  fill(palm_presence, coca, farc_attack, .direction = "up") %>%
  ungroup()

# remove municipalities with incomplete displacement rate data

complete_munis <- df_balanced %>%
  group_by(cod_dane) %>%
  summarise(all_years_present = all(!is.na(sdisp_rate))) %>%
  filter(all_years_present) %>%
  pull(cod_dane)

df_bacon <- df_balanced %>%
  filter(cod_dane %in% complete_munis,
         !is.na(palm_presence))

# compute how many observations were dropped

n_before <- n_distinct(df_balanced$cod_dane)
n_after <- n_distinct(df_bacon$cod_dane)
cat("Dropped", n_before - n_after, 
    "municipalities with incomplete sdisp_rate data.\n")

# run the Goodman-Bacon analysis

bacon_out <- bacon(sdisp_rate ~ palm_presence + farc_attack + coca, 
                    id_var = "cod_dane", time_var = "year", data = df_bacon)

# ------------------------------------------------------------------------------
# parallel trends test
# ------------------------------------------------------------------------------

# setup dataset variables to record first treated etc.

onset <- df %>%
  filter(palm_presence == 1) %>%
  group_by(cod_dane) %>%
  summarise(first_treat = min(year), .groups="drop")

pta <- df %>%
  left_join(onset, by="cod_dane") %>%
  mutate(
    event_time = year - first_treat,
    event_time = if_else(is.na(event_time), -999L, as.integer(event_time))
  )

twfe_es <- feols(
  sdisp_rate ~ i(event_time, ref = -1) + farc_attack + coca,
  data    = pta,
  cluster = ~cod_dane
)

suns <- df %>% 
  dplyr::select(cod_dane, year, sdisp_rate, palm_presence, 
                farc_attack, coca) %>% 
  left_join(onset) %>% 
  mutate(first_treat = replace_na(first_treat, 10000)) %>% 
  mutate(year_rel = year - 1992, 
         first_treat_rel = first_treat - 1992)

# run a dynamic TWFE model 

sa20_es = fixest::feols(
  sdisp_rate ~ farc_attack + coca + sunab(first_treat_rel, year_rel) | 
    cod_dane + year, 
  suns
)

pDat_twfe <- fixest::iplot(twfe_es, plot = FALSE)$prms %>% as_tibble() %>% 
  mutate(method = "TWFE")
pDat_sa20 <- fixest::iplot(sa20_es, plot = FALSE)$prms %>% as_tibble() %>% 
  mutate(method = "Sun&Abraham")

pDat_both <- bind_rows(pDat_twfe, pDat_sa20)

# plot model results

my_cols <- c(
  "TWFE" = "#d95f02",
  "Sun&Abraham" = "#1b9e77"
)

ptrend_plot <- ggplot(pDat_both, aes(x = x, y = estimate, 
                                     ymin = ci_low, ymax = ci_high)) +
  geom_errorbar(aes(color = method),
                width = 0.2, size = 0.8,
                position = position_dodge(width = 0.5)) +
  geom_point(aes(fill = method),
             shape = 21, size = 3,
             color = "white",
             position = position_dodge(width = 0.5)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  
  facet_wrap(~method, ncol = 1, scales = "free_x") +
  
  scale_color_manual(values = my_cols) +
  scale_fill_manual(values = my_cols) +
  scale_x_continuous(breaks = seq(-10, 10, by = 2)) +
  theme_ipsum_rc(base_family = "sans", base_size = 13, grid = FALSE) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    legend.position = "none",
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    x = "Years Since Treatment",
    y = "Estimated ATT on Inverse-Sine Transformed Displacement Rate"
  ) +
  coord_cartesian(xlim = c(-10, 10)) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  
    axis.title.y = element_text(hjust = 0.5)  
  )

ptrend_plot

ggsave('../exhibits/fig-4.pdf', 
       device = cairo_pdf)

# apply a Wald test to test if the pre-treatment coefficients are significantly 
# different from 0

linearHypothesis(
  twfe_es,
  paste0("event_time::", -5:-2, " = 0"),
  vcov = function(model) vcov(model, cluster = "cod_dane")
)

# ------------------------------------------------------------------------------
# no interference test
# ------------------------------------------------------------------------------

# import shapefile data and identify municipality neighbours

muni_pts <- st_read("../data/geospatial/ADMINISTRATIVO/MGN_ADM_MPIO_GRAFICO.shp",
                    stringsAsFactors = FALSE) %>%
  mutate(cod_dane = as.character(as.numeric(MPIO_CCNCT))) %>%
  dplyr::select(cod_dane, geometry) %>%
  st_transform(3116) %>%
  st_make_valid() %>%
  mutate(geometry = st_centroid(geometry)) %>%
  filter(cod_dane %in% unique(as.character(df$cod_dane))) %>%
  arrange(cod_dane)

coords <- st_coordinates(muni_pts)
nb50k <- dnearneigh(coords, 0, 50000)
lw50k <- nb2listw(nb50k, style = "B", zero.policy = TRUE)
muni_pts <- muni_pts %>%
  mutate(nn = lengths(nb50k)) %>%
  arrange(cod_dane)

df_tagged <- df %>%
  mutate(
    cod_dane = as.character(cod_dane),
    existed = TRUE
  )

years <- sort(unique(df_tagged$year))
all_codes <- muni_pts$cod_dane

df_calc <- expand_grid(cod_dane = all_codes, year = years) %>%
  left_join(df_tagged, by = c("cod_dane","year")) %>%
  mutate(palm_presence = replace_na(palm_presence, 0)) %>%
  left_join(dplyr::select(muni_pts, cod_dane, geometry, nn), 
            by = "cod_dane") %>%
  st_as_sf() %>%
  arrange(cod_dane, year)

df_calc <- df_calc %>%
  group_by(year) %>%
  mutate(neigh_sum = spdep::lag.listw(lw50k, palm_presence, 
                                      zero.policy = TRUE)) %>%
  ungroup() %>%
  group_by(cod_dane) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(prop_spill = lag(neigh_sum, default = 0) / nn) %>%
  ungroup()

onset <- df %>%
  mutate(cod_dane = as.character(cod_dane)) %>%
  filter(palm_presence == 1) %>%
  group_by(cod_dane) %>%
  summarise(first_treat = min(year), .groups = "drop")

pta <- df_calc %>%
  filter(existed) %>%
  left_join(onset, by = "cod_dane") %>%
  mutate(
    event_time = year - first_treat,
    event_time = if_else(is.na(event_time), -999L, as.integer(event_time)),
    spill_q = prop_spill
  )

# run a TWFE model with neighbour controls

spill_es <- feols(
  sdisp_rate ~
    i(event_time, ref = -1)
  + i(event_time, spill_q, ref = -1)
  + farc_attack + coca
  | cod_dane + year,
  data = pta,
  cluster = ~cod_dane
)

spill_terms <- grep("spill_q", names(coef(spill_es)), value = TRUE)
et_index <- str_extract(spill_terms, "(?<=^event_time::)-?\\d+(?=:spill_q$)")
et_index <- as.integer(et_index)
pre_lead_terms <- spill_terms[et_index < -1 & et_index > -11]
print(pre_lead_terms)

# run a Wald test on the spillover coefficients

wald_res <- wald(spill_es, pre_lead_terms)

# ------------------------------------------------------------------------------
# no anticipation test
# ------------------------------------------------------------------------------

k = 2

# create dataset with treatment start dates in the past

df_csa_pb_1 <- df_csa %>% 
  mutate(
    G_lead = case_when(
      G == 0L ~ 0L,
      (G - k) < min(year) ~ NA,
      TRUE ~ G - k
    )
  ) %>% 
  filter(!is.na(G_lead))

# run the Callaway and Sant'Anna model with these treatment dates

csa_pb_1 <- att_gt(
  yname = "sdisp_rate",
  tname = "year",             
  idname = "cod_dane",
  gname = "G_lead",
  xformla = ~ farc_attack + coca,
  data = df_csa_pb_1,
  control_group = "nevertreated", 
  bstrap = TRUE,                
  est_method = "dr"
)

es_pb_1 <- aggte(csa_pb_1, type = "dynamic", na.rm = TRUE)

pDat_pb_1 <- tibble(
  x = es_pb_1$egt,        
  estimate = es_pb_1$att.egt, 
  se = es_pb_1$se.egt
) %>%
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    method = "CSA Placebo"
  )


placebo_plot_1 <- ggplot(pDat_pb_1, aes(x = x, y = estimate, 
                                    ymin = ci_low, ymax = ci_high)) +
  geom_errorbar(
    width = 0.2,
    size = 0.8,
    color = "#1b9e77"
  ) +
  geom_point(
    shape = 21,
    size = 3,
    fill = "#1b9e77",
    color = "white"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  scale_x_continuous(breaks = seq(min(pDat_pb_1$x), max(pDat_pb_1$x), by = 1)) +
  theme_ipsum_rc(base_family = "sans", base_size = 13, grid = FALSE) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Years Since (Placebo) Treatment",
    y = "Placebo ATT on Inverse-Sine Transformed Displacement Rate"
  ) +
  coord_cartesian(
    xlim = c(0, max(pDat_pb_1$x)),
    ylim = c(min(pDat_pb_1$ci_low), max(pDat_pb_1$ci_high))
  )

k = 3

df_csa_pb_2 <- df_csa %>% 
  mutate(
    G_lead = case_when(
      G == 0L ~ 0L,
      (G - k) < min(year) ~ NA,
      TRUE ~ G - k
    )
  ) %>% 
  filter(!is.na(G_lead))

csa_pb_2 <- att_gt(
  yname = "sdisp_rate",
  tname = "year",             
  idname = "cod_dane",
  gname = "G_lead",
  xformla = ~ farc_attack + coca,
  data = df_csa_pb_2,
  control_group = "nevertreated", 
  bstrap = TRUE,                
  est_method = "dr"
)

es_pb_2 <- aggte(csa_pb_2, type = "dynamic", na.rm = TRUE)

pDat_pb_2 <- tibble(
  x = es_pb_2$egt,        
  estimate = es_pb_2$att.egt, 
  se = es_pb_2$se.egt
) %>%
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    method = "CSA Placebo"
  )

placebo_plot_2 <- ggplot(pDat_pb_2, aes(x = x, y = estimate, 
                                    ymin = ci_low, ymax = ci_high)) +
  geom_errorbar(
    width = 0.2,
    size = 0.8,
    color = "#1b9e77"
  ) +
  geom_point(
    shape = 21,
    size = 3,
    fill = "#1b9e77",
    color = "white"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  scale_x_continuous(breaks = seq(min(pDat_pb_2$x), max(pDat_pb_2$x), by = 1)) +
  theme_ipsum_rc(base_family = "sans", base_size = 13, grid = FALSE) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Years Since (Placebo) Treatment",
    y = "Placebo ATT on Inverse-Sine Transformed Displacement Rate"
  ) +
  coord_cartesian(
    xlim = c(0, max(pDat_pb_2$x)),
    ylim = c(min(pDat_pb_2$ci_low), max(pDat_pb_2$ci_high))
  )

placebo_plot_2

# plot both results

placebo_plot_1 + placebo_plot_2

ggsave('../exhibits/fig-5.pdf', device = cairo_pdf)
