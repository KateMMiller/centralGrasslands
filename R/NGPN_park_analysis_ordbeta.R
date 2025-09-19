#----------------------------
# Setting up hurdle beta model to analyze trends in plant life form cover
#----------------------------
library(tidyverse)       # ggplot, dplyr, and friends
library(brms)            # Bayesian modeling through Stan
library(emmeans)         # Calculate marginal effects in fancy ways
library(tidybayes)       # Manipulate Stan objects in a tidy way
library(broom)           # Convert model objects to data frames
library(broom.mixed)     # Convert brms model objects to data frames
library(scales)          # For formatting numbers with commas, percents, and dollars
library(gghalves)        # for special half geoms
library(ggdist)          # for posterior distributions
library(ordbetareg)      # for ordered beta regression: https://www.robertkubinec.com/post/limited_dvs/
library(DHARMa)          # for model checking
library(DHARMa.helpers)  # for dispersion test; remotes::install_github("Pakillo/DHARMa.helpers")
# library(patchwork)       # For combining plots
# library(ggh4x)           # For nested facets in ggplot
# library(ggtext)          # Use markdown and HTML in ggplot text
# library(MetBrewer)       # Use pretty artistic colors
# library(gapminder)       # Country-year panel data from the Gapminder project
# library(palmerpenguins)  # Penguin data!

covpts <- read.csv("./data/Point_Intercept_LifeForm_2011-2024.csv")
head(covpts)
# Code below doesn't work because of where RTools44 is installed. Booo!
# install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
# cmdstanr::install_cmdstan()

options(mc.cores = 16)#, brms.backend = "cmdstanr")

CHAINS = 4
ITER = 20000
WARMUP = 1000
BAYES_SEED = 4242

# pct_cov has some values > 1, but
covpts$equal1 <- ifelse(covpts$pct_cov == 1, 1, 0)
covpts$pct_cov2 <- ifelse(covpts$pct_cov == 1, 0.999, 0)
table(covpts$equal1)
(45/12219)*100 # only 3 percent of pct_cov = 1. Going to just make them be 0.999

invg <- covpts |> filter(LifeForm == "Graminoid - Inv.")
table(covpts$LifeForm)

lfs <- sort(unique(covpts$LifeForm))

ggplot(covpts, # |> filter(LifeForm == "Bare"),
       aes(x = pct_cov, fill = Unit_Name, group = Unit_Name)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  facet_wrap(~LifeForm, scales = 'free_y') +
  theme_bw()

# Ordered beta reg: https://www.robertkubinec.com/post/limited_dvs/
badinvg <- covpts |> filter(LifeForm == "Graminoid - Inv.") |> filter(Unit_Name == "BADL")

badinvg_mod <- ordbetareg(pct_cov ~ year_std + (1 + year_std|MacroPlot_Name) + (1|year_fac),
                          data = badinvg, save_pars = save_pars(all = TRUE),
                          chains = 4, iter = 5000,  cores = 12) #, refresh = 0)

# Diagnostic code largely stolen from the brilliant Ellen Cheng.
# Model Diagnostics
summary(badinvg_mod)
rstan::check_hmc_diagnostics(badinvg_mod$fit) # no convergence issues
bayes_R2(badinvg_mod) # 0.75

# LOO
badinvg_mod <- add_criterion(badinvg_mod, criterion = "loo", moment_match = T)
(badinvg_loo <- loo(badinvg_mod)) # 98.8% good; 0.8% bad, 0.4% (1) very bad
pp_check(badinvg_mod, type = "loo_pit_qq", ndraws = 2000)

# Coefficient plot
mcmc_plot(badinvg_mod, variable="^b_", regex=TRUE) +
  geom_vline(xintercept = 0, color = "dimgrey", linewidth = 1.2) + theme_bw()

# Model convergence check
plot(badinvg_mod) # plots showing posterior distribution of parameters, and trace plots

# pp-checks
pp_check(badinvg_mod, type = "ecdf_overlay", ndraws = 2000)
pp_check(badinvg_mod, type = "stat_2d", ndraws = 2000)

# check by location
pp_check(badinvg_mod, type = "ecdf_overlay_grouped", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "violin_grouped", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "mean", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "max", group = "MacroPlot_Name", ndraws = 200)

# check by year
pp_check(badinvg_mod, type = "dens_overlay_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "ecdf_overlay_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "violin_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "mean", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "max", group = "year_std", ndraws = 200)

# DHARMa
simres <- DHARMa.helpers::dh_check_brms(badinvg_mod)
DHARMa::testZeroInflation(simres) # p = 1
DHARMa::testDispersion(simres) # p = 0.5
DHARMa::testOutliers(simres, type = "bootstrap") # p = 1

# Plot residuals vs each independent factor
DHARMa::testCategorical(simres, catPred = badinvg_mod$data$MacroPlot_Name)
DHARMa::plotResiduals(simres, badinvg_mod$data$year_std)

# There's indication of some overdispersion- like only some plots are zero inflated. Would need to
# find covariates to resolve this. But given that I'm planning an analysis of up to 60 parks
# and multiple metrics, this approach is not scaleable. Going back to my trusty case bootstrap.


# zero inflated beta
priort <- c(set_prior("student_t(3,0,2.5)", class = "b", dpar = "zi"))

badinvg_zib <- brm(
  bf(pct_cov2 ~ year_std + (1 + year_std|MacroPlot_Name) + (1|year_fac),
     phi ~ MacroPlot_Name,
     zi ~   (1 + year_std|MacroPlot_Name) ,
  data = badinvg,
  family = zero_inflated_beta(),
  chains = 4, iter = 8000, warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  prior = priort,
  thin = 2,
  file = "badinvg_zib"))

# Model Diagnostics
summary(badinvg_zib)
rstan::check_hmc_diagnostics(badinvg_mod$fit) # no convergence issues
bayes_R2(badinvg_mod) # 0.75

# LOO
badinvg_mod <- add_criterion(badinvg_mod, criterion = "loo", moment_match = T)
(badinvg_loo <- loo(badinvg_mod)) # 98.8% good; 0.8% bad, 0.4% (1) very bad
pp_check(badinvg_mod, type = "loo_pit_qq", ndraws = 2000)

# Coefficient plot
mcmc_plot(badinvg_mod, variable="^b_", regex=TRUE) +
  geom_vline(xintercept = 0, color = "dimgrey", linewidth = 1.2) + theme_bw()

# Model convergence check
plot(badinvg_mod) # plots showing posterior distribution of parameters, and trace plots

# pp-checks
pp_check(badinvg_mod, type = "ecdf_overlay", ndraws = 2000)
pp_check(badinvg_mod, type = "stat_2d", ndraws = 2000)

# check by location
pp_check(badinvg_mod, type = "ecdf_overlay_grouped", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "violin_grouped", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "mean", group = "MacroPlot_Name", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "max", group = "MacroPlot_Name", ndraws = 200)

# check by year
pp_check(badinvg_mod, type = "dens_overlay_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "ecdf_overlay_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "violin_grouped", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "mean", group = "year_std", ndraws = 200)
pp_check(badinvg_mod, type = "stat_grouped", stat = "max", group = "year_std", ndraws = 200)

# DHARMa
simres <- DHARMa.helpers::dh_check_brms(badinvg_mod)
DHARMa::testZeroInflation(simres) # p = 1
DHARMa::testDispersion(simres) # p = 0.5
DHARMa::testOutliers(simres, type = "bootstrap") # p = 1

# Plot residuals vs each independent factor
DHARMa::testCategorical(simres, catPred = badinvg_mod$data$MacroPlot_Name)
DHARMa::plotResiduals(simres, badinvg_mod$data$year_std)

### LINE-INTERCEPT BRMS TREND PLOTS ----
# These are quick plots, to make sure model results look good
# Check histograms of model predictions vs observed data
# For plots that can be used in reports and presentations (i.e., well-formatted plots), run script '4_plots_tables'
zibeta_pred <-
  badinvg_mod |>
  tidybayes::predicted_draws(
    newdata = select(badinvg_mod$data, -pct_cov)  |>  distinct())  |>
  dplyr::mutate(
    is_zero = .prediction == 0,
    type = "predicted") |>
  dplyr::ungroup() |>
  dplyr::select(year_std, MacroPlot_Name, response = .prediction, is_zero)

dat_real <- badinvg_mod$data %>%
  dplyr::mutate(
    is_zero = pct_cov == 0,
    type = "actual") %>%
  dplyr::select(year_std, MacroPlot_Name, pct_cov, is_zero)

dat_combined <- rbind.data.frame(dat_real, zibeta_pred)

# Expected vs observed zero-data, by site
ggplot(dat_combined, aes(x = type, fill = is_zero)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(vars(Site), labeller = label_both) +
  theme_bw() +
  theme(legend.position = "right")

# Expected vs observed non-zero response, by site
ggplot(subset(dat_combined, is_zero == FALSE), aes(x = response)) +
  geom_density(aes(fill = type, color = type), alpha = 0.4) +
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_x_continuous(limits = c(-0.1, 1), labels = scales::label_percent()) +
  scale_fill_manual(values = c("actual" = "#8DB600", "predicted" = "#B3446C")) +
  scale_color_manual(values = c("actual" = "#8DB600", "predicted" = "#B3446C")) +
  facet_wrap(vars(Site), labeller = label_both) +
  theme_bw() +
  theme(legend.position = "right")

# Expected vs observed zero-data, by site and year
ggplot(dat_combined, aes(x = type, fill = is_zero)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(direction = -1) +
  facet_grid(rows = vars(Site), cols = vars(YearNum), labeller = label_both) +
  theme_bw() +
  theme(legend.position = "right")

# Expected vs observed non-zero response, by year
ggplot(subset(dat_combined, is_zero == FALSE), aes(x = response)) +
  geom_density(aes(fill = type, color = type), alpha = 0.4) +
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_x_continuous(limits = c(-0.1, 1), labels = scales::label_percent()) +
  scale_fill_manual(values = c("actual" = "#8DB600", "predicted" = "#B3446C")) +
  scale_color_manual(values = c("actual" = "#8DB600", "predicted" = "#B3446C")) +
  facet_wrap(vars(YearNum), labeller = label_both) +
  theme_bw() +
  theme(legend.position = "right")



pp_check(badinvg_mod) + theme_bw()
pp_check(badinvg_mod, type= "stat_2d", ndraws = 2000) + theme_bw()
plot(brms::conditional_effects(badinvg_mod))

pp_check(badinvg_mod, type = "loo_pit_qq", ndraws = 2200, moment_match = T) + theme_bw()
mcmc_plot(badinvg_mod, variable = "^b_", regex = T) +
  geom_vline(xintercept = 0, color = 'dimgrey')
plot(badinvg_mod) + theme_bw()

# check by location
pp_check(badinvg_mod, type = "ecdf_overlay", ndraws = 1000) + theme_classic()
pp_check(badinvg_mod, type = "violin_grouped", group = )

badinvg_check <- createDHARMa(simulatedResponse = t(posterior_predict(badinvg_mod)),
                              observedResponse = badinvg$pct_cov,
                              fittedPredictedResponse = apply(t(posterior_epred(badinvg_mod)), 1, mean),
                              integerResponse = TRUE)
plot(badinvg_check)



# https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#4-zero-inflated-beta-regression-bayesian-style
# Change this to 1 park at a time, so faster run time and easier to interpret.

# phi is precision, which I would expect to change by unit
# zi is the zero inflation- modeling 0 vs non-zero

beta_invg_unit <- brm(
  bf(pct_cov2 ~ year_std * Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac),
     phi ~ year_std * Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac),
     zi ~  year_std * Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac)),
     data = invg,
     family = zero_inflated_beta(),
     chains = CHAINS, iter = ITER, warmup = WARMUP, file = "beta_invg_unit")
#pairs(beta_invg_unit)

plot(beta_invg_unit, variable = c("b_year_std"))
tidy(beta_invg_unit, effects = 'fixed')

pp_check(beta_invg_unit) + theme_bw()

pp_check(beta_invg_unit, group = "Unit_Name", type = "stat", stat = "mean", ndraws = 1000)
pp_check(beta_invg_unit, "intervals_grouped", group = "Unit_Name", ndraws = 100)
tidy(beta_invg_unit)

pred <- posterior_predict(beta_invg_unit)
bayesplot::ppc_dens_overlay(y = log1p(invg$pct_cov2),
                            yrep = log1p(pred[1:100,]))
tidy(beta_invg_unit, effects = "fixed")
# Intercept is on logit scale, so use plogis(); phi_intercept, phi_ are on log scale, so use exp

# zi estimate
zi_intercept <- tidy(beta_invg_unit, effects = "fixed") %>%
  filter(component == "zi", term == "(Intercept)") %>%
  pull(estimate)

zi_quota <- tidy(beta_invg_unit, effects = "fixed") %>%
  filter(component == "zi", term == "quotaTRUE") %>%
  pull(estimate)

plogis(zi_intercept + zi_quota) - plogis(zi_intercept)
## b_zi_Intercept
##        -0.0283
