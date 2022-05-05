# Read in data ------------------------------------------------------------

comet_all <- vroom::vroom("data-raw/US_COMET-Planner_Download.csv",
                          na = "-999.00", # import -999 values as na (means "not estimated")
                          col_select = c(1:15, 18:19, 33:34) # select only cols of interest
)

# Filter to only WA
# Filter out multiple CPS implementations since Haley Nagle confirmed they are additive.
# Keep only GHG variables that have data. CH4 has no data.

comet_wa <- comet_all %>%
  dplyr::filter(state == "WA") %>%
  dplyr::filter(!is.na(cpsnum))

# Parse out CPS implementations -------------------------------------------

## tags for irrigation (options: non-irrigated, irrigated, not specified)

comet_wa$irrigation <- NA

for (row in 1:nrow(comet_wa)) {
  comet_wa$irrigation[row] <-
    ifelse(
      stringr::str_detect(comet_wa$planner_implementation[row],
                          "Non-Irrigated") == TRUE, "Non-irrigated",
      ifelse(
        stringr::str_detect(comet_wa$planner_implementation[row],
                            "Irrigated") == TRUE, "Irrigated",
        "Not specified"
      )
    )
}

write.csv(comet_wa, "data-raw/comet_wa.csv")

usethis::use_data(comet_wa)
