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
  select(MacroPlot_Name, year, month, doy, NumPtsTran, Transect, vegtype) |> unique() |>
  group_by(MacroPlot_Name, year, month, doy, vegtype) |>
  summarize(num_points = sum(NumPtsTran), .groups = 'drop')

# Summarize by lifeform, given that sometime multiple species of a lifeform are recorded in a point
covpts4 <- left_join(covpts3, covpts_tran, by = c("MacroPlot_Name", "year", "month", "doy", "vegtype")) |>
  group_by(MacroPlot_Name, vegtype, Unit_Name, year, month, doy, num_points, LifeForm, Transect, Tape) |>
  summarize(num_hits = sum(!is.na(Point)),
            point_hit = ifelse(num_hits > 0, 1, 0),
            .groups = 'drop')

covpts5 <- covpts4 |>
  group_by(MacroPlot_Name, Unit_Name, vegtype, year, month, doy,
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

# Species diversity metrics
spprich1 <- covpts3 |>
  mutate(LifeForm = case_when(NotBiological == TRUE & Symbol == "BARE" ~ "Bare",
                              NotBiological == TRUE & Symbol == "LITT" ~ "Litter",
                              NotBiological == TRUE & !Symbol %in% c("BARE", "LITT") ~ "Other Non-Bio",
                              LifeForm_Name == "Graminoid" & Invasive == TRUE ~ "Graminoid - Inv.",
                              LifeForm_Name == "Graminoid" & Invasive == FALSE ~ "Graminoid",
                              TRUE ~ LifeForm_Name
  ))

# Point-level species richness vs. Plot-level species richness
# plot level first
spprich1$LifeForm[spprich1$Symbol %in% c("TRADE", "CUSCU")] <- "Forb/herb"
table(spprich1$LifeForm, spprich1$LifeForm_Name)
table(spprich1$LifeForm, useNA = 'always')

spprich <- spprich1 |>
  filter(NumPtsTran == 50) |>
  filter(NumTran == 2) |>
  filter(NotBiological == FALSE) |> # plants only
  select(MacroPlot_Name, Unit_Name, year, month, doy, Transect, Symbol, LifeForm) |>
  distinct() |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy, LifeForm) |>
  summarize(numspp = sum(!is.na(Symbol)), .groups = 'drop') |>
  group_by(MacroPlot_Name, Unit_Name, year, month, doy) |>
  mutate(total_spp = sum(numspp))

write.csv(spprich, "./data_final/NGPN_plot_level_spprich_by_lifeform.csv", row.names = F)

# Within site diversity
sppdiv_df <- spprich1 |>
  mutate(present = 1,
         tran_loc = paste0("T", Transect, "_", Tape)) |> arrange(Symbol) |>
  select(MacroPlot_Name, Unit_Name, vegtype, year, month, doy, Symbol, NumTran,
         TranLen, NumPtsTran, tran_loc, present) |>  distinct() |>
  pivot_wider(names_from = Symbol, values_from = present, values_fill = 0) |>
  arrange(MacroPlot_Name, year, doy, tran_loc)

head(sppdiv_df)

write.csv(sppdiv_df, "./data_final/NGPN_plot-level_diversity_data.csv", row.names = F)
names(sppdiv_df)
non_sppnames <- c("MacroPlot_Name", "Unit_Name", "vegtype", "year", "month",
                  "doy", "NumTran", "TranLen", "NumPtsTran", "tran_loc" )
sppnames <- names(sppdiv_df)[!names(sppdiv_df) %in% non_sppnames]
sppdiv_df2 <- sppdiv_df |>
  mutate(num_spp = rowSums(across(all_of(sppnames)))) |> data.frame()
sppdiv_df2$plotid <- paste0(sppdiv_df2$MacroPlot_Name, "_", sppdiv_df2$year)
sppdiv_df3 <- sppdiv_df2[,c("plotid", "Unit_Name", "tran_loc", "num_spp", sppnames)]

head(sppdiv_df3)
plot_list <- sort(unique(sppdiv_df3$plotid))

# park level
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
# Spp rich by life form
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


# Using each point intercept pair to assess heterogeniety within a plot, and will look at trends over time
# Within plot jaccard similarity. Using jaccard b/c works with binary and treats double 0s as different
point_jac <- purrr::map(plot_list,
                       function(p){
                         sppdiv1 <- sppdiv_df3 |> filter(plotid == p)
                         nzspp <- names(sppdiv1[,sppnames])[colSums(sppdiv1[,sppnames]) != 0]
                         sppdiv2 <- sppdiv1[,nzspp]
                         jacdist <- vegdist(sppdiv2, method = "jaccard")
                         jac_df <- data.frame(plotid = p,
                                              jac_mean = mean(jacdist),
                                              jac_sd = sd(jacdist),
                                              jac_min = min(jacdist),
                                              jac_max = max(jacdist),
                                              jac_u95 = quantile(jacdist, probs = 0.975),
                                              jac_l95 = quantile(jacdist, probs = 0.025),
                                              jac_n = nrow(sppdiv2)
                                              )
                       }, .progress = TRUE) |> list_rbind()

point_jac$Unit_Name <- substr(point_jac$plotid, 1, 4)
point_jac$year <- as.numeric(substr(point_jac$plotid, (nchar(point_jac$plotid) - 3), nchar(point_jac$plotid)))
point_jac$Macropoint_Name <- substr(point_jac$plotid, 1, nchar(point_jac$plotid) - 5)
point_jac <- point_jac[,c("Macropoint_Name", "Unit_Name", "year", "jac_mean", "jac_sd",
                        "jac_min", "jac_max", "jac_u95", "jac_l95", "jac_n")]
point_jac$year_fac <- as.factor(point_jac$year)
point_jac$year_std <- point_jac$year - min(point_jac$year)

write.csv(point_jac, "./data_final/NGPN_point-level_jaccard_similarity.csv", row.names = F)

# plot-level jaccard
spprich2 <- left_join(spprich1, covpts_tran, by = c("MacroPlot_Name", "year", "month", "doy", "vegtype"))
spprich3 <- spprich2 |> group_by(MacroPlot_Name, Unit_Name, vegtype, year, month, doy, Symbol, num_points) |>
  summarize(num_hits = sum(!is.na(Order)),
            pct_cov = num_hits/first(num_points),
            .groups = 'drop')
range(spprich3$pct_cov)
head(spprich3)

plot_div <- spprich3 |> arrange(Symbol) |>
  select(MacroPlot_Name, Unit_Name, vegtype, year, month, doy, Symbol, num_points, pct_cov) |>
  distinct() |>
  pivot_wider(names_from = Symbol, values_from = pct_cov, values_fill = 0) |>
  arrange(MacroPlot_Name, year, doy)
names(plot_div)
non_spp_names <- c("MacroPlot_Name", "Unit_Name", "vegtype", "year", "month",
                  "doy", "num_points")
spp_names <- names(plot_div)[!names(plot_div) %in% non_spp_names]

head(plot_div)
park_list <- sort(unique(plot_div$Unit_Name))
year_list <- sort(unique(plot_div$year))
park_year <- expand.grid(park_list, year_list) |> arrange(Var1, Var2) |>
  select(park = Var1, year = Var2)

park_year <- data.frame(table(plot_div$Unit_Name, plot_div$year)) |>
  filter(Freq > 0) |> select(park = Var1, year = Var2) |>
  arrange(park, year)
park_year[31,]
p = "DETO"
year = 2016

name_plot1 <- function(df, num){
  rep(plot_list[num], times = (nrow(df) - num))
}

name_plot2 <- function(df, num){
  num2 = num + 1
  plot_list[num2:nrow(df)]
}
park_year[31,]
p = "DETO"; y = 2016

plot_sim <- purrr::map2(park_year$park, park_year$year,
  function(p, y){
    df1 <- plot_div |> filter(Unit_Name == p) |> filter(year %in% y)
    #print(head(df1))
    plot_list <- unique(df1$MacroPlot_Name)
    if(length(plot_list) > 1){
        nzspp <- names(df1[,spp_names])[colSums(df1[,spp_names]) != 0]
        df2 <- df1[,nzspp]
        jacdist <- vegdist(df2, method = "jaccard", diag = F)
        jacdf <- data.frame(jac = as.numeric(jacdist))

        plot_col1 <- data.frame(col1 = lapply(1:nrow(df1), function(x){
          name_plot1(df = df1, num = x)}) |> unlist())
        plot_col2 <- data.frame(col2 = lapply(1:(nrow(df1)-1), function(x){
          name_plot2(df = df1, num = x)}) |> unlist())

        bcdist <- vegdist(df2, method = "bray")
        braydf <- data.frame(bray = as.numeric(bcdist))

        mat1 <- data.frame(Unit_Name = as.character(rep(p, nrow(plot_col1))),
                           year = as.numeric(rep(y, nrow(plot_col1))),
                           plot_col1 = plot_col1$col1,
                           plot_col2 = plot_col2$col2,
                           jaccard = jacdf$jac,
                           bray = braydf$bray)
        mat1
        #print(head(mat1))
      } else {
        mat1 <- data.frame(Unit_Name = as.character(p),
                           year = as.numeric(y),
                           plot_col1 = as.character(plot_list),
                           plot_col2 = NA_character_,
                           jaccard = NA_real_,
                           bray = NA_real_)
        mat1
                       }
                            #bray = bcdist[lower.tri(bcdist, diag = T)])
                        }, .progress = TRUE) |> list_rbind()

write.csv(plot_sim, "./data_final/NGPN_plot-level_jaccard_similarity.csv", row.names = F)
