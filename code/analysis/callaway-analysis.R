# ------------------------------------------------------------------------------
# Setup Data
# ------------------------------------------------------------------------------

onset <- df %>%
  filter(palm_presence == 1) %>%
  group_by(cod_dane) %>%
  summarise(first_treat = min(year), .groups = "drop")

df_csa <- df %>%
  left_join(onset, by = "cod_dane") %>%
  mutate(
    G = ifelse(is.na(first_treat), 
               0L, 
               as.integer(first_treat))
  )

df_csa <- df_csa %>%
  mutate(cod_dane = as.integer(cod_dane))

# ------------------------------------------------------------------------------
# Callaway & Sant’Anna (2021)
# ------------------------------------------------------------------------------

csa_res <- att_gt(
  yname = "sdisp_rate",
  tname = "year",
  idname = "cod_dane",
  gname = "G",
  xformla = ~ farc_attack + coca,
  data = df_csa,
  control_group = "nevertreated",
  bstrap = TRUE,
  est_method = "dr"
)

es_csa <- aggte(
  csa_res,
  type = "dynamic",
  na.rm = TRUE
)

es_df <- tibble(
  k = es_csa$egt,     
  estimate = es_csa$att.egt, 
  se = es_csa$se.egt
) %>%
  mutate(
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    period = if_else(k < 0, "Pre", "Post")
  )

my_cols <- c("Pre" = "#d95f02",   
             "Post" = "#1b9e77")

csa_plot <- ggplot(es_df,
       aes(x = k, y = estimate,
           ymin = ci_low, ymax = ci_high,
           colour = period, fill = period)) +
  geom_errorbar(width = 0.25, size = 0.8) +
  geom_point(size = 3, shape = 21, colour = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = my_cols, guide = "none") +
  scale_fill_manual(values  = my_cols, guide = "none") +
  scale_x_continuous(breaks = seq(min(es_df$k), max(es_df$k), by = 1)) +
  theme_ipsum_rc(base_family = "sans", base_size = 13, grid = FALSE) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_line(colour = "grey90"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Years since palm-oil adoption",
    y = "ATT on Inverse-Sine Transformed  displacement rate"
  ) +
  coord_cartesian(xlim = c(-5, 5)) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  
    axis.title.y = element_text(hjust = 0.5)
  )

csa_plot

ggsave('../exhibits/fig-3.pdf', 
       device = cairo_pdf)

agg_eq <- aggte(csa_res, type = "group", na.rm = TRUE)

simple_avg <- mean(agg_eq$att.egt)      
simple_se <- sqrt( mean(agg_eq$se.egt^2) )

tibble(
  measure = "Simple avg (equal‐wt across g,t)",
  ATT = simple_avg,
  SE = simple_se,
  CI_low = simple_avg + qnorm(0.025) * simple_se,
  CI_high = simple_avg + qnorm(0.975) * simple_se
)

agg_wt <- aggte(csa_res, type = "simple", na.rm = TRUE)

tibble(
  measure = "Weighted avg (wt by N_g)",
  ATT = agg_wt$overall.att,
  SE = agg_wt$overall.se,
  CI_low = agg_wt$overall.att + qnorm(0.025) * agg_wt$overall.se,
  CI_high = agg_wt$overall.att + qnorm(0.975) * agg_wt$overall.se
)
