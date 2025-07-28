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
                                        "CHDN", "GULN", "HTLN", "NGPN",
                                        "ROMN", "SCPN", "SODN", "SOPN"),
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
                      div(leafletOutput("CGIMap",
                                        height = "775px",
                                        width = "95%")
                      ))),
      column(5, style = "padding: 10px 5px 5px 5px",
             tags$div(title = "Table of Central Grassland parks",
                      div(DTOutput("prop_hab_dt",
                                          height = "600px",
                                          width = "90%")))),
      br()
    ) # end fluidRow

  )# end of navbar
)
