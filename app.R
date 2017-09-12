require(shiny)
require(stringr)
require(dplyr)
require(stringdist)

# setwd("~/Documents/shinyapps.io/newsboard")

fn = function(x1, x2) stringdist(x1, x2)/nchar(x1)

id_duplicates = function(x, FUN){
  v = 1
  for(i in 2:length(x)){
    j = x[i]
    sd = fn(j, x[v])
    if(all(sd > .4)) v = c(v, i)
  }
  (1:length(x))[-v]
}

floating_title = readLines('https://raw.githubusercontent.com/geotheory/newsboard/master/title.html')

update_iframe = function(url, verbose = F){
  d = readLines(url)
  i1 = which(str_detect(d, 'img.thumbimg:hover')) + 2
  i2 = which(str_detect(d, '<script type="text/javascript">')) - 1
  newcss = '\timg.thumbimg { filter: brightness(90%); }\n\tdiv { text-shadow: 0 0 0.4em #000;}'
  d2 = c(d[1:i1], newcss, d[(i1+1):i2], floating_title, '</body></html>')
  
  # remove duplicates
  i3 = which(str_detect(d2, '<div class="pane">'))
  d3_1 = d2[ 1:(i3-1) ]
  d3_2 = d2[ i3 ]
  d3_3 = d2[ (i3+1):length(d2) ]
  i4 = str_locate(d3_2, '<script')[1]
  d3_2_2 = substr(d3_2, i4, nchar(d3_2))
  d3_2_1 = substr(d3_2, 1, i4-1)
  divs = str_replace_all(d3_2_1, '</div><div class="pane"', '</div>~~<div class="pane"') %>% str_split('~~') %>% .[[1]]
  x = lapply(divs, function(v) str_extract(v, '<br>.*') %>% substr(5, nchar(.)-16)) %>% as.character()
  dups = id_duplicates(x, fn)
  if(verbose){ print(x); print(dups) }
  divs2 = divs[-dups] %>% paste(collapse = '\n')
  c(d3_1, divs2, d3_2_2, d3_3) %>% paste(collapse='\n') %>% 
    str_replace_all('="/assets/', '="./')
}


server <- function(input, output, session){
  observe({
    url = readLines('https://raw.githubusercontent.com/geotheory/newsboard/master/webpage_url.txt', warn = F)
    new_iframe = update_iframe(url)
    sink('www/new_iframe.html'); cat(new_iframe); sink()
    
    output$frame = renderUI({
      tags$iframe(src = 'new_iframe.html', width = input$dimension[1], height = input$dimension[2]) # 'new_iframe.html'
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
                    /* rescale iframe contents */
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
                    @media screen and (-webkit-min-device-pixel-ratio:0) { #scaled-frame { zoom: .8;} } 
                    "))
    ),
  
  htmlOutput("frame")
    )

shinyApp(ui, server)