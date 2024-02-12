# # Building a Prod-Ready, Robust Shiny Application.
# #
# # README: each step of the dev files is optional, and you don't have to
# # fill every dev scripts before getting started.
# # 01_start.R should be filled at start.
# # 02_dev.R should be used to keep track of your development during the project.
# # 03_deploy.R should be used once you need to deploy your app.
# #
# #
# ###################################
# #### CURRENT FILE: DEV SCRIPT #####
# ###################################
#
# # Engineering
#
# ## Dependencies ----
# ## Amend DESCRIPTION with dependencies read from package code parsing
# ## install.packages('attachment') # if needed.
# attachment::att_amend_desc()
#
#
# usethis::use_package("DT")
# usethis::use_package("config")
# usethis::use_package("sommer")
# usethis::use_package("RSpectra")
# usethis::use_package("pedigreemm")
# usethis::use_package("LMMsolver")
# usethis::use_package("utils")
# usethis::use_package("stats")
# usethis::use_package("shiny")
# usethis::use_package("shinyjs")
# usethis::use_package("shinydashboard")
# usethis::use_package("shinycssloaders")
# usethis::use_package("shinyAce")
# usethis::use_package("shinyWidgets")
# usethis::use_package("magrittr")
# usethis::use_package("plotly")
# usethis::use_package("plyr")
# usethis::use_package("tibble")
# usethis::use_package("psych")
# usethis::use_package("shinybusy")
# usethis::use_package("RColorBrewer")
# usethis::use_package("inline")
# usethis::use_package("data.table")
# usethis::use_package("shinybusy")
# usethis::use_package("markdown")
# usethis::use_package("kableExtra")
# usethis::use_package("ggplot2")
# # usethis::use_package("tidyverse")
# usethis::use_pipe()
# usethis::use_dev_package("cgiarBase", type = "Imports", remote = "Breeding-Analytics/cgiarBase")
# usethis::use_dev_package("cgiarPipeline", type = "Imports", remote = "Breeding-Analytics/cgiarPipeline")
# usethis::use_dev_package("cgiarOcs", type = "Imports", remote = "Breeding-Analytics/cgiarOcs")
# usethis::use_dev_package("QBMS", type = "Imports", remote = "icarda-git/QBMS")
# usethis::use_package( "shinymanager" )
#
# # remotes::install_github("Breeding-Analytics/cgiarBase")
# # remotes::install_github("Breeding-Analytics/cgiarPipeline")
# # remotes::install_github("Breeding-Analytics/cgiarOcs")
# # remotes::install_github("icarda-git/QBMS")
#
# ## Add modules ----
# ## Create a module infrastructure in R/
# # golem::add_module(name = "input_file_sta", with_test = TRUE) # Name of the module
# golem::add_module(name = "getData", with_test = TRUE) # extracting raw data
#
# golem::add_module(name = "qaRawApp", with_test = TRUE) # QA on raw data
# golem::add_module(name = "filterPhenoApp", with_test = TRUE ) # QA on raw data
# golem::add_module(name = "expDesignEditApp", with_test = TRUE ) # QA on raw data
# golem::add_module(name = "qaGenoApp", with_test = TRUE) # QA after sta
# golem::add_module(name = "singleCrossGenoApp", with_test = TRUE) # QA after sta
#
# golem::add_module(name = "staApp", with_test = TRUE) # single trial analysis
# golem::add_module(name = "qaStaApp", with_test = TRUE) # QA after sta
# golem::add_module(name = "mtaApp", with_test = TRUE) # multi trial analysis
# golem::add_module(name = "indexDesireApp", with_test = TRUE) # desire selection index
# golem::add_module(name = "ocsApp", with_test = TRUE) # optimal cross selection
# golem::add_module(name = "rggApp", with_test = TRUE) # realized genetic gain
# golem::add_module(name = "pggApp", with_test = TRUE) # predicted genetic gain
#
# golem::add_module(name = "homeApp", with_test = TRUE) # home
#
# golem::add_module(name = "saveData", with_test = TRUE) # extracting raw data
# golem::add_module(name = "aboutApp", with_test = TRUE) # technology
# golem::add_module(name = "meetTheTeamApp", with_test = TRUE) # home
# golem::add_module(name = "contactUsApp", with_test = TRUE) # home
#
# golem::add_module(name = "sectionInfoQAApp", with_test = TRUE) # for quality assurance
# golem::add_module(name = "sectionInfoGEApp", with_test = TRUE) # for genetic evaluation
# golem::add_module(name = "sectionInfoSHApp", with_test = TRUE) # for selection history
#
# ## Add helper functions ----
# ## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)
#
# ## External resources
# ## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")
#
# ## Add internal datasets ----
# ## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)
#
# ## Tests ----
# ## Add one line by test you want to create
# usethis::use_test("app")
#
# # Documentation
#
# ## Vignette ----
# # usethis::use_vignette("frame")
# devtools::build_vignettes()
#
# ## Code Coverage----
# ## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()
#
# # Create a summary readme for the testthat subdirectory
# covrpage::covrpage()
#
# ## CI ----
# ## Use this part of the script if you need to set up a CI
# ## service for your application
# ##
# ## (You'll need GitHub there)
# usethis::use_github()
#
# # GitHub Actions
# usethis::use_github_action()
# # Chose one of the three
# # See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# # Add action for PR
# usethis::use_github_action_pr_commands()
#
# # Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()
#
# # AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()
#
# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()
#
# # Jenkins
# usethis::use_jenkins()
#
# # GitLab CI
# usethis::use_gitlab_ci()
#
# # You're now set! ----
# # go to dev/03_deploy.R
# rstudioapi::navigateToFile("dev/03_deploy.R")
