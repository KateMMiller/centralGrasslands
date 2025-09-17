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
# library(patchwork)       # For combining plots
# library(ggh4x)           # For nested facets in ggplot
# library(ggtext)          # Use markdown and HTML in ggplot text
# library(MetBrewer)       # Use pretty artistic colors
# library(gapminder)       # Country-year panel data from the Gapminder project
# library(palmerpenguins)  # Penguin data!

covpts <- read.csv("./data/Point_Intercept_LifeForm_2011-2024.csv")

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

# https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#4-zero-inflated-beta-regression-bayesian-style
beta_invg_unit <- brm(
  bf(pct_cov2 ~ year_std + Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac),
     phi ~ year_std + Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac),
     zi ~  year_std + Unit_Name + (1 + year_std|MacroPlot_Name) + (1|year_fac)),
     data = invg,
     family = zero_inflated_beta(),
     chains = CHAINS, iter = ITER, warmup = WARMUP, file = "beta_invg_unit")
#pairs(beta_invg_unit)

plot(beta_invg_unit, variable = c("b_year_std"))
tidy(beta_invg_unit, effects = 'fixed')

pp_check(beta_invg_unit)
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
