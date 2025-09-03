# Each park has it's own species' designations for Invasive or not, but that doesn't
# work for regional analyses. Any species invasive in one park is going to be treated
# as invasive for this analysis.
taxa_inv <- taxa |> select(Symbol, Invasive) |> mutate(present = 1) |>
  distinct() |>
  pivot_wider(names_from = Invasive, values_from = present, values_fill = 0) |>
  mutate(Invasive = ifelse(`TRUE` == 1, TRUE, FALSE))

# Taking the Species_UV1 designation if it exists
taxa_c3c4 <- taxa |> select(Symbol, Species_UV1) |>
  filter(Species_UV1 %in% c("warm", "cool")) |>
  filter(!is.na(Symbol)) |>
  distinct()

taxa2 <- left_join(taxa |> select(Symbol, Nativity, LifeCycle, LifeForm_Name,
                                  NotBiological) |> distinct(),
                   taxa_inv, by = "Symbol") |>
  filter(!(Symbol == "PIMI7" & is.na(LifeCycle)))

taxa3 <- left_join(taxa2, taxa_c3c4, by = "Symbol") |>
  mutate(LifeForm_Name = ifelse(LifeForm_Name == "Grass-like", "Graminoid", LifeForm_Name)) |> distinct()

head(taxa3)

taxa4 <-
  taxa3 |>
  mutate(grassc3c4 = ifelse(Species_UV1 %in% c("cool", "warm"), Species_UV1, NA_character_ ),
         LifeCycle = ifelse(is.na(LifeCycle), "Not Defined", LifeCycle)) |>
  select(Symbol, Nativity, Invasive, LifeCycle, LifeForm_Name,
         NotBiological, grassc3c4) |> distinct()

# More taxa table fixes
taxa4$LifeCycle[taxa4$Symbol %in% c("HELIA3", "SALSO", "SIAL2", "ALAL3")] <- "Annual" # all annual except 1
taxa4$LifeCycle[taxa4$Symbol %in% c("CANU4", "ARPY4", "BOST4")] <- "Biennial" # mostly biennial
taxa4$LifeCycle[taxa4$Symbol %in% c("ERIGE2", "SYMPH4", "TRADE", "OXALI")] <- "Perennial" # all perennial buy 1
taxa4$Nativity[taxa4$Symbol %in% c("JUNCU", "ERIGE2", "SYMPH4", "TRADE",
                                   "OXALI", "POLYG4", "BOST4", "AMRE")] <- TRUE # all T but 1
taxa4$LifeForm_Name[taxa4$Symbol == "SYMPH4"] <- "Forb/herb" # fixes not defined
taxa4$LifeForm_Name[taxa4$Symbol %in% c("CASE5", "COUM", "ARLU", "ERIGE2",
                                        "GAAP2", "FACO", "TRADE", "OXALI")] <- "Forb/herb" # all forb but 1 subshrub
taxa4$Nativity[taxa4$Symbol == "ELYMU"] <- FALSE # all F but 1
taxa4$Species_UV1[taxa4$Symbol %in% c("ELYMU", "JUNCU")] <- "cool" # all cool but 1 blank
taxa4$LifeForm_Name[taxa4$Symbol %in% c("HEDR", "OESE3")] <- "Subshrub"

taxa4$LifeCycle[taxa4$Symbol == "MOSS"] <- "Perennial"
taxa4$Nativity[taxa4$Symbol == "MOSS"] <- TRUE
taxa4$LifeForm_Name[taxa4$Symbol == "MOSS"] <- "Nonvascular"

taxa5 <- distinct(taxa4)

pctcov2 <- left_join(pctcov,
                     taxa5,
                     by = c("Symbol"))
pctcov[18739, "Symbol"]

head(pctcov)
