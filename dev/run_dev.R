# Sass code compilation
sass::sass(input = sass::sass_file("inst/app/www/custom.sass"), output = "inst/app/www/custom.css", cache = NULL)

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# press Ctrl + F3 to see the reactive graph and identify where things are blocked or delayed
# options(shiny.reactlog = TRUE)

# Run the application (use port 1410 if run on localhost)
run_app()

# profile your Shiny application
#profvis::profvis(print(run_app()))
