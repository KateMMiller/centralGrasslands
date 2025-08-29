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
  distinct()

table(macro$vegtype)

covpts2 <- left_join(covpts, macro, by = "MacroPlot_Name") |>
  filter(grepl("UG|BS", vegtype)) |> # keep plots with grassland or badlands sparse
  filter(!grepl("PP", vegtype))|> # drop plots with Ponderosa pine also in vegtype
  filter(!Unit_Name %in% "JECA") |> # only has 2 UG plots
  distinct()
table(covpts2$vegtype)

head(covpts) #410698 v 384313
nrow(covpts2) #263255

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

covpts3 <- left_join(covpts2, taxa, by = c("Symbol", "Unit_Name"))

# diversity of heights:
# range of heights
ht_div <- covpts3 |> select(MacroPlot_Name, Unit_Name, year, month, doy,
                            NumTran, TranLen, Transect, Height) |>
  filter(Height <= 2) |> # Drop a couple of error heights in dataset
  group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  summarize(ht_max = max(Height, na.rm = T),
            ht_med = median(Height, na.rm = T),
            ht_min = min(Height, na.rm = T),
            ht_range = ht_max - ht_med,
            ht_sd = sd(Height, na.rm = T),
            .groups = 'drop')

length(unique(ht_div$MacroPlot_Name))

ggplot(ht_div, aes(x = year, y = ht_range, group = MacroPlot_Name,
                   color = MacroPlot_Name)) +
  geom_point() + geom_line() + facet_wrap(~Unit_Name) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(ht_div, aes(x = year, y = ht_med, group = MacroPlot_Name,
                   color = MacroPlot_Name)) +
  geom_point() + geom_line() + facet_wrap(~Unit_Name) +
  theme_bw() +
  theme(legend.position = "none")

# number of hits per point
num_hits <- covpts3 |> group_by(MacroPlot_Name, Unit_Name, year, month, doy, Transect, Point) |>
  summarize(num_hits = sum(!is.na(Order)), .groups = "drop") |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  summarize(hits_med = median(num_hits, na.rm = T),
            hits_max = max(num_hits, na.rm = T),
            hits_min = min(num_hits, na.rm = T),
            hits_sd = sd(num_hits, na.rm = T),
            .groups = 'drop')

ggplot(num_hits, aes(x = year, y = hits_med, group = MacroPlot_Name,
                   color = MacroPlot_Name)) +
  geom_point() + geom_line() + facet_wrap(~Unit_Name) +
  theme_bw() +
  theme(legend.position = "none")

# Total spp richness (even though I don't like this measure)
table(covpts3$NumPtsTran) # quite a few plots with 75 or 100 points
table(covpts$NumTran) # qute a few with 1 or 5 transects
covpts4 <- covpts3 |>
  mutate(LifeForm = case_when(NotBiological == TRUE & Symbol == "BARE" ~ "Bare",
                              NotBiological == TRUE & Symbol == "LITT" ~ "Litter",
                              NotBiological == TRUE & !Symbol %in% c("BARE", "LITT") ~ "Other Non-Bio",
                              LifeForm_Name == "Graminoid" & Invasive == TRUE ~ "Graminoid - Inv.",
                              LifeForm_Name == "Graminoid" & Invasive == FALSE ~ "Graminoid",
                              TRUE ~ LifeForm_Name
  ))
covpts4$LifeForm[covpts4$Symbol %in% c("TRADE", "CUSCU")] <- "Forb/herb"
table(covpts4$LifeForm, covpts4$LifeForm_Name)
table(covpts4$LifeForm, useNA = 'always')

spprich <- covpts4 |>
  filter(NumPtsTran == 50) |>
  filter(NumTran == 2) |>
  filter(NotBiological == FALSE) |> # plants only
  select(MacroPlot_Name, Unit_Name, year, month, doy, Transect, Symbol, LifeForm) |>
  distinct() |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy, LifeForm) |>
  summarize(numspp = sum(!is.na(Symbol)), .groups = 'drop') |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  mutate(total_spp = sum(numspp))

# plot level
ggplot(spprich, aes(x = year, y = numspp, group = MacroPlot_Name,
                    color = MacroPlot_Name)) +
  geom_point() + geom_line() + facet_wrap(~Unit_Name) +
  theme_bw() +
  theme(legend.position = "none")

# park level (eventually model this with proper random effects)
spprich_park <- spprich |> group_by(Unit_Name, year, LifeForm) |>
  summarize(mean_rich = mean(numspp),
            mean_rich_tot = mean(first(total_spp)),
            .groups = 'drop')

col_pal <- c("Forb/herb" = "#05e689", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Subshrub" = "#9371B9", "Shrub" = "#386EC7", "Tree" = "#8A20A1", "Vine" = "#ff7f00",
            "Nonvascular" = '#DFACFC')

labels <- c("Forb/herb" = "Forb/herb", "Graminoid" = "Graminoid",
            "Graminoid - Inv." = "Graminoid - Inv.", "Subshrub" = "Subshrub",
            "Shrub" = "Shrub", "Tree" = "Tree", "Vine" = "Woody Vine",
            "Nonvascular" = "Nonvascular")

shps <- c("Forb/herb" = 19, "Graminoid" = 17, "Graminoid - Inv." = 15,
          "Subshrub" = 18, "Shrub" = 15, "Tree" = 17,
          "Vine" = 15, "Nonvascular" = 15)

shpsz <- c("Forb/herb" = 2.5, "Graminoid" = 2.5, "Graminoid - Inv." = 2,
           "Subshrub" = 3, "Shrub" = 2, "Tree" = 2.5,
           "Vine" = 2, "Nonvascular" = 2)

spprich_park$LifeForm_fac <-
  factor(spprich_park$LifeForm,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Subshrub", "Shrub", "Tree", "Vine",
                    "Nonvascular"))
head(spprich_park)

table(spprich_park$LifeForm, spprich_park$LifeForm_fac)

# Spp rich by life form
plot_r_lf <-
ggplot(spprich_park, aes(x = year, y = mean_rich,
                         group = LifeForm_fac, color = LifeForm_fac,
                         shape = LifeForm_fac, size = LifeForm_fac)) +
  geom_point() + facet_wrap(~Unit_Name) +
  scale_shape_manual(values = shps, name = "Life Form", labels = labels) +
  scale_size_manual(values = shpsz,
                    name = "Life Form", labels = labels) +
  scale_color_manual(values = col_pal, name = "Life Form", labels = labels) +
  geom_line(linewidth = 0.5) +
  theme_bw() + labs(x = NULL, y = "Mean Species Richness") +
  theme(legend.position = "bottom")

plot_r <-
ggplot(spprich_park |> select(Unit_Name, year, mean_rich_tot) |> distinct(),
       aes(x = year, y = mean_rich_tot)) +
  geom_point(color = "dimgrey") + #geom_line() +
  facet_wrap(~Unit_Name) +
  geom_smooth(color = "#2058A1") + theme_bw() +
  labs(x = NULL, y = "Mean total richness")

names(spprich_park)
#---- Species % cover ----
pctcov <- covpts3 |> select(MacroPlot_Name, Unit_Name, year, month, doy,
                            NumTran, NumPtsTran, Transect, Symbol, ScientificName, Point) |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy,
           NumTran, NumPtsTran, Symbol, ScientificName) |>
  summarize(num_hits = sum(!is.na(Point)),
            pct_cov = (num_hits/(first(NumPtsTran) * first(NumTran)))*100,
            # note that b/c most sites have 100 points num_hits is already percent cover per spp.
            .groups = 'drop') #|>
  #filter(Symbol != "XXXX") |> data.frame()
pctcov <- pctcov |> group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  mutate(total_cov = sum(pct_cov),
         rel_cov = (pct_cov/total_cov)*100) |> data.frame()
         #cov_check = sum(rel_cov)) |> data.frame()

#--- Looking at various groups - have to relativize cover for each grouping.
pctcov2 <- left_join(pctcov, taxa, by = c("Unit_Name", "Symbol", "ScientificName")) |>
  mutate(LifeForm = case_when(NotBiological == TRUE & Symbol == "BARE" ~ "Bare",
                              NotBiological == TRUE & Symbol == "LITT" ~ "Litter",
                              NotBiological == TRUE & !Symbol %in% c("BARE", "LITT") ~ "Other Non-Bio",
                              LifeForm_Name == "Graminoid" & Invasive == TRUE ~ "Graminoid - Inv.",
                              LifeForm_Name == "Graminoid" & Invasive == FALSE ~ "Graminoid",
                              TRUE ~ LifeForm_Name
                              ))
pctcov2$LifeForm[pctcov2$Symbol %in% c("TRADE", "CUSCU")] <- "Forb/herb"
table(pctcov2$LifeForm, pctcov2$LifeForm_Name)
table(pctcov2$LifeForm, useNA = 'always')

pctcov_lf <- pctcov2 |> group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  mutate(total_cov = sum(pct_cov)) |> ungroup() |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy, LifeForm) |>
  summarize(tot_cov = sum(pct_cov),
            rel_cov = ((sum(pct_cov))/first(total_cov))*100,
            rel_cov_check = sum(rel_cov),
            .groups = 'drop') |> data.frame()

# Percent Lifeform
pct_life_park <- pctcov_lf |> group_by(Unit_Name, year, LifeForm) |>
  summarize(avg_rel_cov = mean(rel_cov),
            avg_pct_cov = mean(tot_cov),
            .groups = "drop")

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

pct_life_park$LifeForm_fac <-
  factor(pct_life_park$LifeForm,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Subshrub", "Shrub", "Tree", "Vine",
                    "Bare", "Litter", "Nonvascular", "Other Non-Bio", "Undefined"))

table(pct_life_park$LifeForm, pct_life_park$LifeForm_fac)

plot_rc_lf <-
ggplot(pct_life_park, aes(x = year, y = avg_rel_cov,
                          group = LifeForm_fac, color = LifeForm_fac,
                          shape = LifeForm_fac, size = LifeForm_fac)) +
  geom_point() + facet_wrap(~Unit_Name) +
  scale_shape_manual(values = shps, name = "Life Form", labels = labels) +
  scale_size_manual(values = shpsz,
                    name = "Life Form", labels = labels) +
  scale_color_manual(values = col_pal, name = "Life Form", labels = labels) +
  geom_line(linewidth = 0.5) +
  theme_bw() + labs(x = NULL, y = "Avg. Relative % Cover")

plot_rc_lf # relative cover by lifeform
plot_r_lf # species richness by lifeform
plot_r # species richness
