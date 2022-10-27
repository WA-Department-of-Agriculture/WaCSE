box::use(
  vroom[vroom],
  dplyr[
    select,
    across,
    starts_with,
    ends_with,
    rename,
    mutate,
    left_join,
    relocate,
    bind_cols,
    summarize
  ],
  tidyr[extract, replace_na, pivot_longer, pivot_wider],
  stringr[str_replace, str_replace_all],
  usethis[use_data]
)

# read in data ------------------------------------------------------------

# import -999 values as na which represent "not estimated"
# filter out multiple CPS implementations since COMET team confirmed they are additive.
comet_wa <- vroom(
  "data-raw/US_COMET-Planner_data.csv",
  na = "-999"
) |>
  subset(state == "WA" & cps_name != "Multiple Conservation Practices") |>
  select(
    state,
    county,
    mlra,
    class,
    cps_name,
    planner_implementation,
    starts_with(c("co2", "n2o", "ch4", "total")),
    -c(ends_with(c("min", "max")))
  ) |>
  rename(
    mlra_code = mlra,
    practice = cps_name,
    implementation = planner_implementation,
    total_ghg_co2_mean = total_ghg_co2
  ) |>
  mutate(across(ends_with(c("mean", "sterr")), as.numeric))

mlra <- vroom(
  "data-raw/MLRA.csv",
  col_select = c(MLRARSYM, MLRA_NAME)
) |>
  unique()

# join comet with MLRA to get MLRA names ----------------------------------

comet_wa <- left_join(comet_wa, mlra, by = c("mlra_code" = "MLRARSYM")) |>
  relocate(mlra = MLRA_NAME, .after = county) |>
  subset(select = -c(state, mlra_code))

# parse out CPS implementations -------------------------------------------

comet_wa <- comet_wa |>
  extract(
    "implementation",
    "nutrient_practice",
    "(Rate|Beef Feedlot Manure|Chicken|Compost|Dairy Manure|Other Manure|Sheep Manure|Swine Manure)",
    remove = FALSE
  ) |>
  extract(
    "implementation",
    "irrigation",
    "(Non-Irrigated|Irrigated)",
    remove = FALSE
  ) |>
  extract(
    "implementation",
    "current_land_use",
    "(Cropland|Crops|Rangeland|Grassland|Pasture)",
    remove = FALSE
  )

# replace NAs

comet_wa <- comet_wa |> replace_na(list(
  irrigation = "Not Specified",
  current_land_use = "Not Specified",
  nutrient_practice = "Not Applicable"
))

# replace Crops with Cropland, Rate with Reduce Application Rate, Chicken with Chicken Manure

comet_wa$current_land_use <-
  str_replace(comet_wa$current_land_use, "Crops", "Cropland")

comet_wa$nutrient_practice <-
  str_replace_all(
    comet_wa$nutrient_practice,
    c(
      "Rate" = "Reduce Application Rate",
      "Chicken" = "Chicken Manure"
    )
  )

# convert character strings to factors

comet_wa <- as.data.frame(
  unclass(comet_wa),
  stringsAsFactors = TRUE
)

# abbreviate implementation for improved readability in plot

comet_wa$abbr <- str_replace_all(
  comet_wa$implementation,
  c(
    "Irrigated" = "Irr",
    "Permanent" = "Perm",
    "Fertilizer" = "Fert",
    "Unfertilized" = "Unfert",
    "Synthetic" = "Synth",
    "Management" = "Mngmt",
    "with" = "w/"
  )
)

comet_wa <- comet_wa |> relocate(abbr, .after = implementation)

# create separate df for just the tags
comet_tags <- comet_wa |>
  select(class:nutrient_practice) |>
  unique()

# pivot to tidy data ------------------------------------------------------------

comet_wa_long <- comet_wa |>
  pivot_longer(
    cols = where(is.numeric),
    names_to = c("ghg_type", "type"),
    names_pattern = "(.*)(_mean|_sterr)$",
    values_to = "value"
  ) |>
  mutate(type = substr(type, 2, nchar(type)))

comet_wa <- comet_wa_long |>
  pivot_wider(
    names_from = type,
    values_from = value
  )

# calculate error bars ----------------------------------------------------

fct_error <- function(data) {
  errors <- data |>
    summarize(
      lower = mean - sterr,
      upper = mean + sterr
    )

  bind_cols(data, errors)
}

comet_wa <- fct_error(comet_wa)

# write to csv and load in usethis ----------------------------------------

write.csv(comet_wa, "data-raw/comet_wa.csv")
write.csv(comet_tags, "data-raw/comet_tags.csv")

use_data(comet_wa, overwrite = TRUE)
use_data(comet_tags, overwrite = TRUE)
