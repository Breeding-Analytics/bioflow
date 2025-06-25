image_files <- list.files(paste0(getwd(),"/inst/app/www/activities"), pattern=".jpg", full.names = FALSE)

#' meetTheTeamApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meetTheTeamApp_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$div(
      fluidRow(
        shinydashboard::box(width = 12, status = "success", collapsible = FALSE,
                            tags$body(
                              p()
                            ),
                            column(width = 12,

                                   shinydashboard::box(status="success", width = 5,
                                                       title = "Contributors of Analytical Modules", solidHeader = TRUE,
                                                       img(src = " www/centers.png", height = 400, width = 480), # add an image
                                                       p(" "),
                                                       p("All CGIAR centers with Biometrics capacity have contributed to the design of the breeding analytics platform
                                                  and currently work in developing analytical modules. Want to contribute? Contact us.")
                                   ),
                                   shinydashboard::box(status="success", width = 7,
                                                       title = "Team Activities", solidHeader = TRUE,

                                                       #######
                                                       tags$head(
                                                        tags$style(HTML("
                                                            #carousel-container {
                                                              width: 600px;
                                                              height: 400px;
                                                              position: relative;
                                                              margin: auto;
                                                              overflow: hidden;
                                                            }
                                                            .carousel-img {
                                                              display: none;
                                                              width: 100%;
                                                              height: 100%;
                                                              object-fit: contain;
                                                            }
                                                            .carousel-img.active {
                                                              display: block;
                                                            }
                                                          "))
                                                      ),

                                                      div(id = "carousel-container",
                                                          lapply(seq_along(image_files), function(i) {
                                                            tags$img(
                                                              src = file.path("www/activities", image_files[i]),
                                                              class = paste("carousel-img", if (i == 1) "active"),
                                                              id = paste0("img", i)
                                                            )
                                                          })
                                                      ),

                                                      # JavaScript to handle automatic carousel switching
                                                      tags$script(HTML(sprintf("
                                                          let current = 1;
                                                          const total = %d;

                                                          setInterval(() => {
                                                            document.getElementById('img' + current).classList.remove('active');
                                                            current = current %% total + 1;
                                                            document.getElementById('img' + current).classList.add('active');
                                                          }, 5000);
                                                        ", length(image_files)))),

                                                       #############
                                                       br()
                                   ),
                                   shinydashboard::box(status="success", width = 12,
                                                       title = "Team Members across Centers", solidHeader = TRUE,
                                                       p(strong("ABI:"), "Christian Werner (C.WERNER@cgiar.org), Dorcus Gemenet (d.gemenet@cgiar.org) "),
                                                       p(strong("AfricaRice:"), "Aubin Amagnide (A.Amagnide@cgiar.org)"),
                                                       p(strong("CIAT:"),"Sergio Cruz (S.Cruz@cgiar.org), Christian Cadena (C.C.Cadena@cgiar.org) "),
                                                       p(strong("CIMMYT: "),"Keith Gardner (K.GARDNER@cgiar.org), Angela Pacheco (r.a.pacheco@cgiar.org), Juan Burgueno (j.burgueno@cgiar.org), Abishek Rathore (ABHISHEK.RATHORE@cgiar.org), Roma Das (r.das@cgiar.org) "),
                                                       p(strong("CIP: "),"Bert de Boeck (B.DeBoeck@cgiar.org), Raul Eyzaguirre (r.eyzaguirre@cgiar.org) "),
                                                       p(strong("ICARDA: "),"Khaled Al-Shamaa (K.EL-SHAMAA@cgiar.org)"),
                                                       p(strong("ICRISAT: "),"Anitha Raman (Anitha.raman@icrisat.org) "),
                                                       p(strong("IITA: "),"Ibnou Dieng (i.dieng@cgiar.org) "),
                                                       p(strong("IRRI: "),"Alaine Guilles (a.gulles@irri.org), Justine Bonifacio (j.bonifacio@irri.org), Daniel Pisano(d.pisano@irri.org), Leilani Nora (l.nora@irri.org)"),
                                                       p(strong("Other"),"Giovanny Covarrubias-Pazaran (covaruberpaz@gmail.com)"),
                                   ),

                                   ),
                            # shinydashboard::box(status="success", width = 12,
                            #                     title = "The team behind the scenes:", solidHeader = TRUE,
                            #                     img(src = " www/team.jpeg", height = 460, width = 600), # add an image
                            #                     p("Inception meeting celebrated in Nairobi Kenya. From left to right; Star (consultant), Khaled (ICARDA), Young-Wha (BMGF),
                            #                       Bert (CIP), Johan (CIAT), Dorcus (GI-BRI), Juan (CIMMYT), Eduardo (IRRI), Keith (CIMMYT), Sergio (CIAT),
                            #                       Aubin (AfricaRice), Raul (CIP).")
                            # ),

        ),
        class = "tab-content"
      )
    )


  )
}

#' meetTheTeamApp Server Functions
#'
#' @noRd
mod_meetTheTeamApp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_meetTheTeamApp_ui("meetTheTeamApp_1")

## To be copied in the server
# mod_meetTheTeamApp_server("meetTheTeamApp_1")
