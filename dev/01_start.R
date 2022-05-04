# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "WaCSEshiny", # The Name of the package containing the App
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> aa99b20938c08397c8f9a4483530660eb194a9e3
  pkg_title = "PKG_TITLE", # The Title of the package containing the App
  pkg_description = "PKG_DESC.", # The Description of the package containing the App
  author_first_name = "AUTHOR_FIRST", # Your First Name
  author_last_name = "AUTHOR_LAST", # Your Last Name
  author_email = "AUTHOR@MAIL.COM", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
<<<<<<< HEAD
=======
  pkg_title = "WaCSE", # The Title of the package containing the App
  pkg_description = "The Washington Climate Smart Estimator (WaCSE) is a shiny application to explore greenhouse gas emission reduction potentials from different conservation practices across Washington's diverse counties.", # The Description of the package containing the App
  author_first_name = "Jadey", # Your First Name
  author_last_name = "Ryan", # Your Last Name
  author_email = "jryan@agr.wa.gov", # Your Email
  repo_url = "https://github.com/WA-Department-of-Agriculture/WaCSEshiny" # The URL of the GitHub Repo (optional)
>>>>>>> 291fc0b (initial commit)
=======
>>>>>>> aa99b20938c08397c8f9a4483530660eb194a9e3
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
<<<<<<< HEAD
<<<<<<< HEAD
usethis::use_mit_license("Golem User") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
=======
usethis::use_ccby_license() # You can set another license here
usethis::use_readme_rmd(open = TRUE)
>>>>>>> 291fc0b (initial commit)
=======
usethis::use_mit_license("Golem User") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
>>>>>>> aa99b20938c08397c8f9a4483530660eb194a9e3
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
<<<<<<< HEAD
<<<<<<< HEAD
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon
=======
golem::use_favicon(path = "inst/app/www/hex-WaCSE.png") # path = "path/to/ico". Can be an online file.
#golem::remove_favicon() # Uncomment to remove the default favicon
>>>>>>> 291fc0b (initial commit)
=======
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon
>>>>>>> aa99b20938c08397c8f9a4483530660eb194a9e3

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
