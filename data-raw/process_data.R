library(dplyr)
library(tidyr)

# Read in data ------------------------------------------------------------

comet_all <- vroom::vroom("data-raw/US_COMET-Planner_Download.csv",
  na = "-999.00", # import -999 values as na (means "not estimated")
  col_select = c(1:2, 4, 6, 8, 9, 12:17, 33:34)
  # select only cols of interest
  # keep only GHG variables that have data. Soil carbon equals CO2.
)

mlra <- vroom::vroom("data-raw/MLRA.csv",
                     col_select = c(MLRARSYM, MLRA_NAME)) %>%
  unique()

# rename columns ----------------------------------------------------------

comet_all <- comet_all %>%
  dplyr::rename(
    practice = cps_name,
    implementation = planner_implementation,
    total.ghg.co2_mean = total_ghg_co2,
    total.ghg.co2_sterr = total_ghg_co2_sterr
  )

# filter to only WA
# filter out multiple CPS implementations since COMET team confirmed they are additive.

comet_wa <- comet_all %>%
  dplyr::filter(state == "WA") %>%
  dplyr::filter(!(practice == "Multiple Conservation Practices"))


# join comet with MLRA to get MLRA names ----------------------------------

comet_wa <- dplyr::left_join(comet_wa, mlra, by = c("mlra" = "MLRARSYM")) %>%
  relocate(mlra = MLRA_NAME, .after = county) %>%
  subset(select = -c(1,4))

# parse out CPS implementations -------------------------------------------

comet_wa <- comet_wa %>%
  tidyr::extract(
    "implementation",
    "nutrient_practice",
    "(Rate|Beef Feedlot Manure|Chicken|Compost|Dairy Manure|Other Manure|Sheep Manure|Swine Manure)",
    remove = FALSE
  ) %>%
  tidyr::extract(
    "implementation",
    "irrigation",
    "(Non-Irrigated|Irrigated)",
    remove = FALSE
  ) %>%
  tidyr::extract(
    "implementation",
    "current_land_use",
    "(Cropland|Crops|Rangeland|Grassland|Pasture)",
    remove = FALSE
  )

# replace NAs

comet_wa <- comet_wa %>% tidyr::replace_na(list(
  irrigation = "Not Specified",
  current_land_use = "Not Specified",
  nutrient_practice = "Not Applicable"
))

# replace Crops with Cropland, Rate with Reduce Application Rate, Chicken with Chicken Manure

comet_wa$current_land_use <-
  stringr::str_replace(comet_wa$current_land_use, "Crops", "Cropland")

comet_wa$nutrient_practice <-
  stringr::str_replace_all(comet_wa$nutrient_practice, c(
    "Rate" = "Reduce Application Rate",
    "Chicken" = "Chicken Manure"
  ))

# convert character strings to factors

comet_wa <- as.data.frame(unclass(comet_wa),
  stringsAsFactors = TRUE
)

# abbreviate implementation for improved readability in plot

comet_wa$abbr <- stringr::str_replace_all(comet_wa$implementation, c(
  "Irrigated" = "Irr",
  "Permanent" = "Perm",
  "Fertilizer" = "Fert",
  "Unfertilized" = "Unfert",
  "Synthetic" = "Synth",
  "Management" = "Mngmt"
))

comet_wa <- comet_wa %>% relocate(abbr, .after = implementation)

# create separate df for just the tags
comet_tags <- unique(comet_wa[, 3:9])

# pivot to tidy data ------------------------------------------------------------

comet_wa <- comet_wa %>%
  tidyr::pivot_longer(
    cols = 10:17,
    names_to = c("ghg_type", "type"),
    names_sep = "_",
    values_to = "value"
  )

comet_wa <- comet_wa %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = value
  )

# calculate error bars ----------------------------------------------------

fct_error <- function(data) {
  errors <- data %>%
    dplyr::summarize(
      lower = mean - sterr,
      upper = mean + sterr
    )

  bind_cols(data, errors)
}

comet_wa <- fct_error(comet_wa)

# write to csv and load in usethis ----------------------------------------

write.csv(comet_wa, "data-raw/comet_wa.csv")
write.csv(comet_tags, "data-raw/comet_tags.csv")

usethis::use_data(comet_wa, overwrite = TRUE)
usethis::use_data(comet_tags, overwrite = TRUE)
