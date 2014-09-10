library(shiny)
library(leaflet)
library(rCharts)


googleAnalytics <- function(account="UA-34839111-4"){
  HTML(paste("<script type=\"text/javascript\">

    var _gaq = _gaq || [];
  _gaq.push(['_setAccount', '",account,"']);
  _gaq.push(['_setDomainName', 'shinyapps.io']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

  </script>", sep=""))
}

shinyUI(
div(class="",
fluidPage(
googleAnalytics(),
  withTags(
    div(class="masthead",
      div(class="header",
        img(src = "logo-small.png", height=55, width=138),
        h3("Open Data Explorer", id="headertext")
      )
    )  
  ),

  navbarPage("", id="nav",
 
#   tags$span(id="file", 
#     fileInput('file', 'Please Select a .csv File to Upload:', accept = c('.csv'))
# ),
  tabPanel("Interactive map", 
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeCSS("http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css")#,
        #includeScript("www/gomap.js")
        
      ),
      
      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "http://{s}.tiles.mapbox.com/v3/kseibel.jfenn0bc/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
            center = c(39.8106, -128.5561),
            zoom = 4,
            detectRetina=TRUE,
            reuseTiles=TRUE
        )
      ),

      absolutePanel(id = "showhide", class = "modal", fixed = TRUE, draggable = FALSE,
          top = 171, left = 290, right = "auto", bottom = "auto",
          width = 26, height = 26,

            withTags({
              button(type="button", id="showhidecontrolbutton", class="btn btn-default",
                i(class="fa fa-filter"),     
                script('$( showhidecontrolbutton ).click(function() {
                  $( "#togglecontrol" ).toggle( "slide" );
                  });')
              )
             
            })
      ),
      
      absolutePanel(id = "showhide", class = "modal", fixed = TRUE, draggable = FALSE,
          top = 207, left = 290, right = "auto", bottom = "auto",
          width = 26, height = 26,   

            withTags({
              button(type="button", id="showhidegraphbutton", class="btn btn-default",
                i(class="fa fa-bar-chart"),     
                script('$( showhidegraphbutton ).click(function() {
                  $( "#togglegraph" ).toggle( "slide" );
                  });')
              )
             
            })     
      ),



      tags$div(id="togglegraph",
      
        absolutePanel(id = "graphs", class = "modal", fixed = TRUE, draggable = TRUE,
          top = 106, left = 330, right = "auto", bottom = "auto",
          width = 530, height = "auto",

          h2("Analysis"),

          fluidRow(
            showOutput("plot1", "highcharts")
          ),
          fluidRow(
            showOutput("plot2", "highcharts")
          )

        
      )      
    ),

      tags$div(id="togglecontrol",
      
        absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
          top = 156, left = 380, right = "auto", bottom = "auto",
          width = 530, height = "auto",

          h2("Controls"),
          
          fluidRow(
            fileInput('file', 'Please Select a .csv File to Upload:', accept = c('.csv'))         
          ),
          br(),
          fluidRow(
            column(6,
              uiOutput("eval")
            ),
            column(6,
              uiOutput("evalfilter") 
            )
          ),
          fluidRow(
            column(6, 
              uiOutput("time")
            ),
            column(6,
              uiOutput("timefilter")
            )
          )
        )      
      ),
      
      tags$div(id="cite",
         'The Alacer Group, 2014.'
      )
    )   
  ),

  tabPanel("Data explorer",
     dataTableOutput("outtable")
  ),

  # tabPanel("Analysis",

    #fileInput('file', 'Please Select a .csv File of Predictions Data to Upload:', accept = c('.csv')),

  #   column(6,
  #     showOutput("plot1", "highcharts")
  #   ),
  #   column(6,
  #     fluidRow(
  #       showOutput("plot2", "highcharts")
  #     ),
  #     fluidRow(
  #       showOutput("plot3", "highcharts")
  #     )
  #   )  
  # ),

  tabPanel("About",
    fluidPage(
      div(class="about", 
      h4("Welcome to the Jersey City Demo Open Data Exploratory Tool."),
      h5("Instructions:"), 
      p("1) Upload a .csv file on the 'Interactive Map' Tab.  The .csv should include timestamp information (labeled as TR), 
        and geocoded latitude and longitude (labeled as LAT and LONG, respectively)."), 
      p("2) Select any category to further evaluate.  By default, all categorical data are included as dimensions for evaluation."),
      p("3) Select any time period to analyze.  By default, the time field is split in different ways to allow more user flexibility in analysis."),
      br(),
      h5("Views:"),
      p("Map: Zoom in and out to view different neighborhoods.  The data displayed in the graphs changes based on the data viewed on the map."),
      p("Cases: The graph presents the actual number of cases, broken down by the evaluation and time period selections.  
        Filter for finer-grained analysis."),
      p("Average Cases: The graph presents the average number of cases for each time period to reveal trends over time."),
      br(),br(),br(),
      p("Developed Using", a("Shiny", href="http://shiny.rstudio.com/"), "for", a("RStudio", href="http://www.rstudio.com/"), br(),
        "Map Courtesy of ", a("Mapbox", href="https://www.mapbox.com/"), "and", a("OpenStreetMaps", href="http://www.openstreetmap.org/")," and developed with ", a("Leaflet", href="http://leafletjs.com/"), br(), 
        "Geocoding completed using Nominatim Search Courtesy of", a("MapQuest", href="http://www.mapquest.com/")
      )
     

    )
    )
  ),
  
  conditionalPanel("false", icon("crosshair"))
  ))
))