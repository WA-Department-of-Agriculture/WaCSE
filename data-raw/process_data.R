# Read in data ------------------------------------------------------------

comet_all <- vroom::vroom("data-raw/US_COMET-Planner_Download.csv",
                          na = "-999.00", # import -999 values as na (means "not estimated")
                          col_select = c(1:2, 4, 6, 8, 9, 12:15, 18:19, 33:34)
                          # select only cols of interest
                          # keep only GHG variables that have data. CH4 has no data.
)

# rename columns ----------------------------------------------------------

comet_all <- comet_all %>%
  dplyr::rename(
    practice = cps_name,
    implementation = planner_implementation,
    soil.carbon.co2_mean = soil_carbon_co2,
    soil.carbon.co2_sterr = soil_carbon_co2_sterr,
    total.ghg.co2_mean = total_ghg_co2,
    total.ghg.co2_sterr = total_ghg_co2_sterr
  )

# filter to only WA
# filter out multiple CPS implementations since Haley Nagle confirmed they are additive.

comet_wa <- comet_all %>%
  dplyr::filter(state == "WA") %>%
  dplyr::filter(!(practice == "Multiple Conservation Practices"))


# parse out CPS implementations -------------------------------------------

## tags for irrigation (options: non-irrigated, irrigated, not specified)

comet_wa$irrigation <- NA

for (row in 1:nrow(comet_wa)) {
  comet_wa$irrigation[row] <-
    ifelse(
      stringr::str_detect(comet_wa$implementation[row],
                          "Non-Irrigated") == TRUE, "Non-irrigated",
      ifelse(
        stringr::str_detect(comet_wa$implementation[row],
                            "Irrigated") == TRUE, "Irrigated",
        "Not specified"
      )
    )
}

comet_wa <- comet_wa %>%
  dplyr::relocate(irrigation, .after = implementation)

# convert character strings to factors ------------------------------------

comet_wa <- as.data.frame(unclass(comet_wa),
                          stringsAsFactors = TRUE)

# pivot to tidy data ------------------------------------------------------------

comet_wa <- comet_wa %>%
  tidyr::pivot_longer(
    cols = 8:15,
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
      upper = mean + sterr)

  bind_cols(data, errors)

}

comet_wa <- fct_error(comet_wa)

# write to csv and load in usethis ----------------------------------------

write.csv(comet_wa, "data-raw/comet_wa.csv")

usethis::use_data(comet_wa, overwrite = TRUE)
