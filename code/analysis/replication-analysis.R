# ------------------------------------------------------------------------------
# Replication of Téllez (2022)
# ------------------------------------------------------------------------------

# read data
df = read_rds("../data/clean data/muni.rds")

# ------------------------------------------------------------------------------
# Models 
# ------------------------------------------------------------------------------

# models
m1 = felm(sdisp_rate ~ palm_presence + farc_attack + coca
          | cod_dane + year | 0 | cod_dane, exactDOF = 'rM',
          data = df)
summary(m1, robust = T)

m1a = felm(ldisp_rate ~ palm_presence + farc_attack + coca
           | cod_dane + year | 0 | cod_dane, exactDOF = 'rM',
           data = df)
summary(m1a, robust = T)


m2 = felm(sdisp_rate ~  palm_presence + palm_presence:lnatprod + farc_attack 
          + coca | cod_dane + year | 0 | cod_dane, exactDOF = 'rM',
          data = df)
summary(m2, robust = T)


m3 = felm(sdisp_rate ~ palm_presence+ palm_presence:auc_dummy + farc_attack 
          + coca | cod_dane + year | 0 | cod_dane, exactDOF = 'rM',
          data = df)
summary(m3, robust = T)


m4 = felm(sdisp_rate ~ palm_presence + palm_presence:farc_dummy + coca
          | cod_dane + year | 0 | cod_dane, exactDOF = 'rM',
          data = df)
summary(m4, robust = T)




# table output
stargazer(m1, m2, m3, m4,
          type = 'latex', 
          keep = c('palm_presence', 
                   ':lnatprod', 
                   ':auc_dummy', 
                   ':farc_dummy'), 
          covariate.labels = c('Palm-oil plantation', 
                               'Plantation X Natl Prod (log)', 
                               'Plantation X AUC presence (dummy)', 
                               'Plantation X FARC presence (dummy)'), 
          keep.stat = 'n', 
          title = 'Effect of palm-oil growth on displacement. Models include municipal and year fixed effects and controls for time-varying presence of coca and FARC attacks.', 
          label = 'palm-pres-mods', 
          out = '../exhibits/table-1.tex',
          dep.var.labels = c('Displacement rate (inverse-sine transformation)'))

# ------------------------------------------------------------------------------
# Main Figures 
# ------------------------------------------------------------------------------

# Authors figure 1:

# broken down by palm presence
pDat = df %>%
  dplyr::select(year, palm_presence, disp_rate, sdisp_rate) %>%
  group_by(year, palm_presence) %>%
  summarise_all(~mean(.,na.rm = T)) %>%
  drop_na() %>% 
  mutate(palm_presence = ifelse(palm_presence == 1, 
                                "Locations with palm-oil", 
                                "Locations without palm-oil"))
# 61 municipalities do not have records for some of the years, which may result 
# in an incorrect calculation.

# for adding labels
labels = 
  pDat %>% 
  filter(year == 2003)

f1 <- ggplot(filter(pDat, year < 2006),  # filter doesn't do anything
       aes(x = year, y = disp_rate, color = palm_presence)) +
  geom_line(size = 1.5) +
  theme_ipsum_rc(axis_title_size = 10, base_family = "sans") +
  labs(x = '',
       y = 'Average rate of displacement \n(per 1,000 inhabitants)',
       color = 'Palm oil cultivations:') +
  theme(
    axis.title.y = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
  ) +
  theme(legend.position = 'none', 
        panel.grid.major = element_blank()) + 
  geom_label_repel(data = labels, 
                   aes(x = year, y = disp_rate, 
                       label = palm_presence, 
                       fill = palm_presence), 
                   fontface = "bold", 
                   color = "white",
                   nudge_y = 2,
                   size = 3,
                   min.segment.length = Inf,
                   nudge_x = 1) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_continuous(
    breaks = seq(1993, 2005, 2),
    limits = c(1993, 2005),
    expand = c(0, 0)
  )

f1

ggsave('../exhibits/fig1.pdf', 
       device = cairo_pdf)

f2 <- ggplot(filter(pDat, year < 2006),  # filter doesn't do anything
             aes(x = year, y = sdisp_rate, color = palm_presence)) +
  geom_line(size = 1.5) +
  theme_ipsum_rc(axis_title_size = 10, base_family = "sans") +
  labs(x = '',
       y = 'Average rate of inverse sine displacement \n(per 1,000 inhabitants)',
       color = 'Palm oil cultivations:') +
  theme(
    axis.title.y = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
  ) +
  theme(legend.position = 'none', 
        panel.grid.major = element_blank()) + 
  geom_label_repel(data = labels, 
                   aes(x = year, y = sdisp_rate, 
                       label = palm_presence, 
                       fill = palm_presence), 
                   fontface = "bold", 
                   color = "white",
                   nudge_y = 2,
                   size = 3,
                   min.segment.length = Inf,
                   nudge_x = 1) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_continuous(
    breaks = seq(1993, 2005, 2),
    limits = c(1993, 2005),
    expand = c(0, 0)
  )

f2

# Figure 1 after accounting for :
ggplot(
  (
    df %>%
      group_by(cod_dane) %>%
      filter(n() == 13) %>% 
      ungroup() %>%
      dplyr::select(year, palm_presence, disp_rate) %>%
      group_by(year, palm_presence) %>%
      summarise_all(~mean(.,na.rm = T)) %>%
      drop_na() %>% 
      mutate(palm_presence = ifelse(palm_presence == 1, 
                                    "Locations with palm-oil", 
                                    "Locations without palm-oil"))
    ) ,
       aes(x = year, y = disp_rate, color = palm_presence)) +
  geom_line(size = 1.5) +
  theme_ipsum_rc(axis_title_size = 10, base_family = "sans") +
  labs(x = '',
       y = 'Average rate of displacement per 1,000 inhabitants',
       color = 'Palm oil cultivations:') +
  theme(legend.position = 'none', 
        panel.grid.major = element_blank()) + 
  geom_label_repel(data = labels, 
                   aes(x = year, y = disp_rate, 
                       label = palm_presence, 
                       fill = palm_presence), 
                   fontface = "bold", 
                   color = "white",
                   nudge_y = 2,
                   size = 5,
                   min.segment.length = Inf,
                   nudge_x = 5) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_continuous(breaks = seq(1992, 2004, 2))

ggsave('../exhibits/fig1_drop.pdf', 
       device = cairo_pdf)


# code from Patrick Baylis to deal with felm: https://www.patrickbaylis.com/blog/2021-01-22-predict-partial/
predict_partial <- function(object, newdata, se.fit = FALSE,
                            interval = "none",
                            level = 0.95){
  if(missing(newdata)) {
    stop("predict_partial requires newdata and predicts for all group effects = 0.")
  }
  
  # Extract terms object, removing response variable 
  tt <- delete.response(terms(object))
  X  <- model.matrix(tt, newdata)
  
  if (class(object) %in% c("lm", "felm")) { 
    B <- as.numeric(coef(object))
    df <- object$df.residual
  } else {
    stop("class(object) should be lm or felm.")
  }
  
  if(! "(Intercept)" %in% names(B)) {
    B <- c("(Intercept)" = 0, B)
  }
  
  
  fit <- data.frame(fit = as.vector(X %*% B))
  
  if(se.fit | interval != "none") {
    sig <- vcov(object)
    se <- apply(X, MARGIN = 1, FUN = get_se, sig = sig)
  }
  
  if(interval == "confidence"){
    t_val <- qt((1 - level) / 2 + level, df = df)
    fit$lwr <- fit$fit - t_val * se
    fit$upr <- fit$fit + t_val * se
  } else if (interval == "prediction"){
    stop("interval = \"prediction\" not yet implemented")
  }
  if(se.fit){
    return(list(fit=fit, se.fit = se))
  } else {
    return(fit)
  }
}

get_se <- function(r, sig) {
  # Compute linear combination, helper function for predict_partial
  # Given numeric vector r (the constants) and vcov sig (the ), compute SE 
  r_sub <- r[colnames(sig)]               # drop the extra "(Intercept)" entry
  sqrt( as.numeric(r_sub %*% sig %*% r_sub) )
}


# new data for lnatprod
newdata = tibble(palm_presence = 1, 
                 farc_attack = 0, 
                 coca = 0, 
                 lnatprod = seq(min(df$lnatprod, na.rm = TRUE), 
                                max(df$lnatprod, na.rm = TRUE), 
                                by = .05))


# get marginal effects
preds = predict_partial(m2, newdata = newdata,
                        se.fit = TRUE)

preds = bind_cols(newdata, preds) %>% 
  # exponentiate to approximate change
  mutate(effect = (exp(fit) - 1), 
         low = (exp(fit - 1.96*se.fit) - 1), 
         hi = (exp(fit + 1.96*se.fit) - 1)) %>% 
  mutate(group = "National palm-oil production\n(logged hectares)")


p1 = ggplot(preds, aes(x = lnatprod, y = effect, ymin = low, 
                       ymax = hi)) + 
  geom_ribbon(alpha = .3, color = "#1b9e77", fill = "#1b9e77") + 
  geom_line(size = 1.5, color = "#1b9e77") + 
  labs(x = NULL, 
       y = "Marginal effect of palm-oil adoption\n(percent change in displacement rate)") + 
  theme_ipsum_rc(base_family = "sans") + 
  scale_y_percent() + 
  facet_wrap(vars(group)) + 
  theme(panel.grid.minor = element_blank()) + 
  geom_hline(yintercept = 0, lty = 2) +
  theme(    
    axis.title.y = element_text(hjust = 0.5)
    )

# new data for auc
newdata = tibble(palm_presence = 1, 
                 farc_attack = 0, 
                 coca = 0, 
                 auc_dummy = c(0, 1))


# get marginal effects
preds = predict_partial(m3, newdata = newdata,
                        se.fit = TRUE)

preds = bind_cols(newdata, preds) %>% 
  # exponentiate to approximate change
  mutate(effect = (exp(fit) - 1), 
         low = (exp(fit - 1.96*se.fit) - 1), 
         hi = (exp(fit + 1.96*se.fit) - 1)) %>% 
  mutate(group = "Paramilitary presence")


p2 = ggplot(preds, aes(x = factor(auc_dummy), y = effect, 
                       ymin = low, 
                       ymax = hi)) + 
  geom_pointrange(fatten = 1, size = 3, shape = 21, fill = "white", 
                  position = position_dodge(width = .2), 
                  color = "#d95f02") +  
  labs(x = NULL, 
       y = NULL) +
  theme_ipsum_rc(base_family = "sans") + 
  scale_y_percent(limits = c(-1, 1)) + 
  scale_x_discrete(breaks = c(0, 1), 
                   labels = c("No", "Yes")) + 
  facet_wrap(vars(group)) + 
  geom_hline(yintercept = 0, lty = 2)

p1 + p2

ggsave('../exhibits/fig-2.pdf', 
       device = cairo_pdf)

