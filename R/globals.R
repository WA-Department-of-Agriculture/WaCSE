# remotes::install_github("thinkr-open/checkhelper")
# checkhelper::print_globals()

utils::globalVariables(unique(c(
  # fct_plot:
  "abbr", "county", "lower", "mlra", "upper",
  # fct_tableFilter:
  "ghg_type",
  # mod_estimate_server :
  "acres", "comet_tags", "comet_wa", "county", "current_land_use",
  "implementation", "irrigation", "mlra", "practice", "totalGHG",
  # mod_estimate_ui:
  "comet_tags", "comet_wa",
  # mod_explore_server :
  "comet_tags", "comet_wa", "county", "current_land_use", "ghg_type",
  "irrigation", "nutrient_practice", "practice",
  # mod_explore_ui:
  "comet_tags", "comet_wa"
)))


# set system timezone to PST

Sys.setenv(TZ = "America/Los_Angeles")
