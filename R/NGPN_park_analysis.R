#------------------------------
# Non-parametric trend analysis
#------------------------------
library(forestTrends) #remotes::install_github("katemmiller/forestTrends")
library(tidyverse)

# Within plot jaccard similarity using point intercepts
point_jac <- read.csv("./data_final/NGPN_point-level_jaccard_similarity.csv")

park_list <- sort(unique(point_jac$Unit_Name))

jac_boot_point <-
  purrr::map(park_list, function(p){
    df <- point_jac |> filter(Unit_Name %in% p)
    mod <- case_boot_lmer(df, x = 'year_std', y = 'jac_mean', group = 'Unit_Name',
                          ID = "MacroPlot_Name", num_reps = 250,
                          random_type = "custom",
                          random_formula = "(1+year_std|MacroPlot_Name)")
    mod$Unit_Name <- p
    return(mod)
  }, .progress = T) |>
  list_rbind()

head(jac_boot_point)

plot_trend_response(jac_boot, xlab = 'Year', ylab = "Within Plot Jaccard Similarity",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


# Within park similarity, using plot-level species percent cover
plot_jac <- read.csv("./data_final/NGPN_plot-level_jaccard_similarity.csv")
plot_jac$year_std <- plot_jac$year - min(plot_jac$year)
plot_jac$year_fac <- as.factor(plot_jac$year)
plot_jac$plotid <- paste0(plot_jac$Unit_Name, "_", plot_jac$id)
head(plot_jac)
park_list <- sort(unique(plot_jac$Unit_Name))

# ENDED HERE
# add year_std and make ID more like a macroplot

jac_boot_point <-
  purrr::map(park_list, function(p){
    df <- plot_jac |> filter(Unit_Name %in% p)
    mod <- case_boot_lmer(df, x = 'year_std', y = 'jaccard', group = 'Unit_Name',
                          ID = "plotid", num_reps = 250,
                          random_type = "custom",
                          random_formula = "(1|year_fac)") # the plotids are different pairs
    mod$Unit_Name <- p
    return(mod)
  }, .progress = T) |>
  list_rbind()

head(jac_boot_point)

plot_trend_response(jac_boot_point, xlab = 'Year', ylab = "Within Park Jaccard Similarity",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
