require(shiny)

# setwd("~/Documents/shinyapps.io/newsboard")

server <- function(input, output, session) {
  observe({
    url = readLines('https://raw.githubusercontent.com/geotheory/newsboard/master/webpage_url.txt')
    output$frame <- renderUI({
      tags$iframe(src = url, width = input$dimension[1], height = input$dimension[2])
    })
  })
}

ui <- fluidPage(
  # return screen dimensions (inc. responsively) to server to manage iframe dims
  tags$head(tags$script('
                        var dimension = 0;
                        $(document).on("shiny:connected", function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')
  ),
  tags$head(
    # custom css
    tags$style(HTML("
                    .container-fluid { padding: 0px 0px 0px 0px; }
                    /* rescale iframe contents 
                    iframe {
                    overflow-x:hidden;
                    zoom: .8;
                    -webkit-zoom: .8;
                    -ms-zoom: .8;
                    -moz-transform: scale(.8, .8);
                    -webkit-transform: scale(1);
                    -o-transform: scale(1, 1);
                    -ms-transform: scale(1.25, 1.25);
                    transform: scale(1.25, 1.25);
                    -moz-transform-origin: top left;
                    -webkit-transform-origin: top left;
                    -o-transform-origin: top left;
                    -ms-transform-origin: top left;
                    transform-origin: top left;
                    }
                    @media screen and (-webkit-min-device-pixel-ratio:0) { #scaled-frame { zoom: .667;} }*/
                    "))
    ),

  htmlOutput("frame")
)

shinyApp(ui, server)
