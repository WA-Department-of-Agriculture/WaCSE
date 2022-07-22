#' ghgEq
#'
#' @description A fct function to calculate greenhouse gas equivalencies from EPA's calculator: https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references
#'
#' @param CO2eq is the total greenhouse gas emissions in MT CO2eq/yr
#'
#' @param eq is the equivalency, options are: home, gas, smartphone, waste, trash, light, seedling, forest, conversion
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

fct_ghgEq <- function(CO2eq, eq) {
  CO2eq <- as.numeric(CO2eq)

  switch(eq,
    "home" = format(round(CO2eq / 5.139, 1),
      big.mark = ",",
      scientific = FALSE
    ),
    "gas" = format(round(CO2eq / 8.887E-3, 0),
      big.mark = ",",
      scientific = FALSE
    ),
    "smartphone" = format(round(CO2eq / 8.220801116E-6, 0),
      big.mark = ",",
      scientific = FALSE
    ),
    "waste" = format(round(CO2eq / 2.89, 1),
      big.mark = ",", scientific = FALSE
    ),
    "trash" = format(round(CO2eq / 20.23, 1),
      big.mark = ",",
      scientific = FALSE
    ),
    "light" = format(round(CO2eq / 2.63849007E-2, 0),
      big.mark = ",",
      scientific = FALSE
    ),
    "seedling" = format(round(CO2eq / 0.0604935136, 0),
      big.mark = ",",
      scientific = FALSE
    ),
    "forest" = format(round(CO2eq / 0.8433333, 0),
      big.mark = ",",
      scientific = FALSE
    ),
    "conversion" = format(round(CO2eq / 148.2433333, 1),
      big.mark = ",",
      scientific = FALSE
    )
  )
}
