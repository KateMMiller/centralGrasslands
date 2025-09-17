library(plantcomNGPN)
library(dplyr)
library(tidyr)
library(ggplot2)

importViews(import_path = "./data_final/NGPN_FFI_views_20250825.zip")
macro <- getMacroPlot() |> select(MacroPlot_Name, vegtype = MacroPlot_UV4) # only include grassland plots based on
head(macro)
covpts <- getCoverPoints() |> select(MacroPlot_Name, Unit_Name, UTM_X, UTM_Y, UTMzone,
                                     year, month, doy, Symbol,
                                     NumTran, TranLen, NumPtsTran, Transect, Point, Tape, Order, Height) |>
  filter(!is.na(Symbol)) |>
  distinct() |>
  filter(Transect %in% 1:2) |> filter(NumTran %in% 1:2)

covpts$year <- as.numeric(covpts$year)
# the last 2 filters drop some rare events with 5 transects for unknown reasons and that I wasn't sure
# how to deal with the offset.

table(covpts$NumPtsTran, covpts$NumTran)

covpts2 <- left_join(covpts, macro, by = "MacroPlot_Name") |>
  filter(grepl("UG|BS", vegtype)) |> # keep plots with grassland or badlands sparse
  filter(!grepl("PP", vegtype))|> # drop plots with Ponderosa pine also in vegtype
  filter(!Unit_Name %in% "JECA") |> # only has 2 UG plots
  distinct()
table(covpts2$vegtype)

head(covpts)
nrow(covpts2) #262999

# Taxa by park
taxa <- VIEWS_NGPN$Taxa_Table |> select(Unit_Name, Symbol, ScientificName, Nativity, Invasive, LifeCycle,
                                        LifeForm_Name, NotBiological, Species_UV1) |>
  group_by(Symbol) |> fill(Species_UV1) |> ungroup() |>
  mutate(LifeForm_Name = ifelse(LifeForm_Name == "Grass-like", "Graminoid", LifeForm_Name)) |>
  mutate(LifeForm_Name = ifelse(LifeForm_Name == "Not Defined", NA_character_, LifeForm_Name)) |>
  group_by(Symbol) |> fill(LifeForm_Name) |>
  mutate(LifeForm_Name = ifelse(is.na(LifeForm_Name), "Not Defined", LifeForm_Name))

taxa_dup <- VIEWS_NGPN$Taxa_Table |> group_by(ScientificName) |>
  summarize(num_sym = sum(!is.na(Symbol)),
            num_tsn = sum(!is.na(ITIS_TSN)),
            tot_nums = num_sym + num_tsn) |>
  filter(tot_nums > 2)
# 683 duplicate species that have the same symbol but different ScientificName or ITIS_TSN.
# Going to ignore this for now, in case I don't need to clean this.

covpts3 <- left_join(covpts2, taxa, by = c("Symbol", "Unit_Name")) |>
  mutate(LifeForm = case_when(NotBiological == TRUE & Symbol == "BARE" ~ "Bare",
                              NotBiological == TRUE & Symbol == "LITT" ~ "Litter",
                              NotBiological == TRUE & !Symbol %in% c("BARE", "LITT") ~ "Other Non-Bio",
                              LifeForm_Name == "Graminoid" & Invasive == TRUE ~ "Graminoid - Inv.",
                              LifeForm_Name == "Graminoid" & Invasive == FALSE ~ "Graminoid",
                              TRUE ~ LifeForm_Name))

table(covpts3$LifeForm, covpts3$LifeForm_Name)

covpts_tran <- covpts3 |>
  select(MacroPlot_Name, year, month, doy, NumPtsTran, Transect) |> unique() |>
  group_by(MacroPlot_Name, year, month, doy) |>
  summarize(num_points = sum(NumPtsTran), .groups = 'drop')


# Summarize by lifeform, given that sometime multiple species of a lifeform are recorded in a point
covpts4 <- left_join(covpts3, covpts_tran, by = c("MacroPlot_Name", "year", "month", "doy")) |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy, num_points, LifeForm, Transect, Tape) |>
  summarize(num_hits = sum(!is.na(Point)),
            point_hit = ifelse(num_hits > 0, 1, 0),
            .groups = 'drop')

covpts5 <- covpts4 |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy,
           num_points, LifeForm) |>
  summarize(num_hits = sum(point_hit),
#            pt_check = ifelse(num_hits > first(num_points), 1, 0),
            .groups = 'drop')

col_pal <- c("Forb/herb" = "#05e689", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Subshrub" = "#9371B9",
             "Shrub" = "#386EC7", "Tree" = "#8A20A1", "Vine" = "#ff7f00",
             "Bare" = "#735E47", "Litter" = "#C2AF80", "Nonvascular" = '#DFACFC',
             "Other Non-Bio" = "#575757", "Undefined" = "grey")

labels <- c("Forb/herb" = "Forb/herb", "Graminoid" = "Graminoid",
            "Graminoid - Inv." = "Graminoid - Inv.", "Subshrub" = "Subshrub",
            "Shrub" = "Shrub", "Tree" = "Tree", "Vine" = "Woody Vine",
            "Bare" = "Bare Ground", "Litter" = "Dead Litter", "Nonvascular" = "Nonvascular",
            "Other Non-Bio" = "Other Non-Bio", "Undefined" = "Undefined")

shps <- c("Forb/herb" = 19, "Graminoid" = 17, "Graminoid - Inv." = 15,
          "Subshrub" = 18, "Shrub" = 15, "Tree" = 17,
          "Vine" = 15, "Bare" = 15, "Litter" = 18, "Nonvascular" = 15,
          "Other Non-Bio" = 17, "Undefined" = 19)

shpsz <- c("Forb/herb" = 2.5, "Graminoid" = 2.5, "Graminoid - Inv." = 2,
           "Subshrub" = 3, "Shrub" = 2, "Tree" = 2.5,
           "Vine" = 2, "Bare" = 2, "Litter" = 3,
           "Nonvascular" = 2, "Other Non-Bio" = 2, "Undefined" = 2.5)

covpts_exp <- covpts5 |> pivot_wider(names_from = LifeForm, values_from = num_hits, values_fill = 0) |>
  pivot_longer(cols = Bare:Undefined, names_to = "LifeForm", values_to = "num_hits") |>
  mutate(pct_cov = num_hits/num_points)

covpts_exp$year_fac <- as.factor(covpts_exp$year)
covpts_exp$year_std <- covpts_exp$year - min(covpts_exp$year)

write.csv(covpts_exp, "./data/Point_Intercept_LifeForm_2011-2024.csv", row.names = F)

covpts_sum <- covpts_exp |>
  group_by(Unit_Name, year, LifeForm) |>
  summarize(mean_cov = mean(pct_cov),
            .groups = "drop")

covpts_sum$LifeForm_fac <-
  factor(covpts_sum$LifeForm,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Subshrub", "Shrub", "Tree", "Vine",
                    "Bare", "Litter", "Nonvascular", "Other Non-Bio", "Undefined"))

#plot_point_lf <-
ggplot(covpts_sum,
       aes(x = year, y = mean_cov,
           group = LifeForm_fac, color = LifeForm_fac,
           shape = LifeForm_fac, size = LifeForm_fac)) +
  geom_point() + facet_wrap(~Unit_Name) +
  scale_shape_manual(values = shps, name = "Life Form", labels = labels) +
  scale_size_manual(values = shpsz,
                    name = "Life Form", labels = labels) +
  scale_color_manual(values = col_pal, name = "Life Form", labels = labels) +
  geom_line(linewidth = 0.5) +
  theme_bw() + labs(x = NULL, y = "Mean % Cover") +
  theme(legend.position = "bottom")

#
