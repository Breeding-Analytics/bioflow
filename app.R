# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# Run the application (use port 1410 if run on localhost)
if (Sys.getenv('SHINY_PORT') == "") {
  bioflow::run_app(options = list(launch.browser = TRUE, port = 1410))
} else {
  bioflow::run_app(options = list(launch.browser = TRUE))
}
