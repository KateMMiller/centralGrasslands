library(shiny)
library(leaflet)
library(DT)

ui <- shinyUI(
  # Banner at the top
  navbarPage(
    tags$head(includeCSS("./www/mapstyles.css")),
    tags$head(
      tags$style(HTML('.navbar-nav, .navbar-brand{
                         padding-top:7px; padding-bottom:2px; height: 65px; font-size: 24px;
                         font-family: Frutiger, "Frutiger Linotype", Calibri, "Helvetica Neue",
                         Helvetica,Arial, sans-serif}
                     .navbar {min-height:65px !important;}
                     .alignbottom{vertical-align:text-bottom;}
                     .aligncenter{vertical-align:center;}'))),

    title = HTML("<div><img src='ah_small_flat_4c_blackbkgr_k100.svg', height='50', alt='central grasslands'>
                 NPS Central Grasslands Dashboard</div>"),
    position = "static-top", inverse = TRUE, collapsible = FALSE, fluid = TRUE,
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css",
    windowTitle = "NPS Central Grasslands Dashboard", id = "MainNavBar",

    #--------------------------------------------------------------
    #  FluidRows to view map and photos
    #--------------------------------------------------------------
    fluidRow(
      column(2, style = 'padding:0 1px 1px 3px;',
             div(id = "MapPanel", class = "panel panel-default controls",
                 h4('Map Controls', class = 'panel-heading'),
                 tags$style(type='text/css',
                            ".selectize-input{font-size: 12px;}
                               .selectize-dropdown{font-size: 12px;}"),

                 tags$div(title = 'Filter by network',
                          style = 'padding:5px 1px 1px 3px',
                          selectizeInput(
                            inputId = 'network',
                            label = h5('Filter by network:'),
                            choices = c("Choose a network" = "",
                                        network_list),
                                        # "CHDN", "GLKN", "GRYN", "GULN", "HTLN", "NCPN",
                                        # "NGPN", "ROMN", "SCPN", "SODN", "SOPN", "UCBN"),
                            selected = NULL)),

                 tags$div(style = 'display:inlin-block',
                          h5("Or")),

                 tags$div(title = 'Zoom to a park:',
                          style = 'padding:0 1px 1px 3px',
                          uiOutput("park_df"))),


             tags$div(title = "Reset the Map",
                      actionButton('reset_view', "Reset Map",
                                   style="color:white;background-color: #5F9EA0;
                         border-color:#436e70;font-size:11px;width:90px;")),

             tags$div(title = "About",
                      actionButton("view_about", "About",
                                   style="color:white:background-color: #484848;
                                     border-color:#436e70;font-size:11px;width:90px"))
      ),

      column(5, style = "padding: 10px 5px 5px 5px",
             tags$div(title = "Map of Central Grassland parks",
                      div(style = "width:95%",
                           HTML("Map of national parks within <a href='https://www.grasslandsroadmap.org/'>
                           Central Grasslands Roadmap Boundary</a>. Zoom to an individual park by selecting
                           a network or park in the Map Controls dropdowns. Clicking on a park will provide
                           additional information in a popup. Click on the radio buttons in the
                           bottom left of the map to change basemaps, and turn off layers by unchecking the
                           layer's box in the bottom left. To reset the view, click on the Reset Map button.")),
                      br(),
                      div(leafletOutput("CGIMap",
                                        height = "810px",
                                        width = "95%")
                      ))),
      column(5, style = "padding: 10px 5px 5px 5px",
             tags$div(title = "Table of Central Grassland parks",
                      HTML("Table of national parks within <a href='https://www.grasslandsroadmap.org/'>
                           Central Grasslands Roadmap Boundary</a>. Columns with % of a given habitat or acres of habitat were derived from
                           <a href='https://www.grasslandsroadmap.org/mapping'>CGRI Assessment Map datashare V2</a>.
                           Park Code is the 4-letter abbreviated park code.
                           Network is the 4-letter abbreviated network code. Total acres are based on
                           <a href='https://irma.nps.gov/DataStore/Reference/Profile/2314463'>
                           Administrative Boundaries of National Park System Units</a>. CGR Bound indicates if the park is entirely within
                           the Central Grasslands Roadmap Boundary (1), or not fully within the CGR Boundary (0).
                           2024 Visitation represents the annual visitation recorded for a given park (note that not all parks have data),
                           as downloaded from the National Park Service Visitor Use Statistics
                           <a href='https://irma.nps.gov/Stats/SSRSReports/National%20Reports/Query%20Builder%20for%20Public%20Use%20Statistics%20(1979%20-%20Last%20Calendar%20Year)'>
                           query builder</a>. Parks with vegetation monitoring by the Inventory and Monitoring Division are indicated
                           by a 1 in the IMD Veg. Monitoring column. Long/Lat coordinates are in WGS84 and represent the park centroids.
                           Columns can be filtered and sorted and data can be copied to clipboard, exported as csv, or printed.<br>"),
                      br(),
                      div(DTOutput("prop_hab_dt",
                                          height = "480px",
                                          width = "90%")))),
      br()
    ) # end fluidRow

  )# end of navbar
)
