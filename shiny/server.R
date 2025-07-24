library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(shinyjs)
library(htmltools)

shiny_server <- function(session, input, output){

  observeEvent(input$view_about, {
    showModal(modalDialog(
      includeHTML("./www/About.html")
    ))
  })

  output$park_df <- renderUI({
    parks2 <- ifelse(input$network %in% network_list,
                     nps_im_df |> filter(NETCODE %in% input$network) |>
                       dplyr::select(UNIT_CODE) |> unique() |> c(),
                     nps_im_df |> dplyr:: select(UNIT_CODE))
    selectizeInput(inputId = 'park',
                   label = h5("Zoom to a park"),
                   choices = c("Choose a park" = "",
                               #park_list))
                               park_list[park_list %in% parks2[[1]]]))
  })

  NPSbasic <- "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  ESRIimagery <- "http://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  ESRItopo <- "http://services.arcgisonline.com/arcgis/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
  ESRINatGeo <- "http://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}"

  pal <- colorNumeric(c("white", "#728946", "#EDECE6", "#FBD82B", "#51284D", "black", "#E40013", "#37618D"),
                      c(0, 5, 7, 100, 500, 1000, 2000, 5000), na.color = 'transparent')

  output$CGIMap <- renderLeaflet({
    leaflet() %>%
      setView(lng = cent[,1], lat = cent[,2], zoom = 5) %>%
      setMaxBounds(lng1 = nps_bbox$xmax*1.01,
                   lng2 = nps_bbox$xmin*1.01,
                   lat1 = nps_bbox$ymax*1.01,
                   lat2 = nps_bbox$ymin*1.01) %>%
      addTiles(
        group = "Map",
        urlTemplate = NPSbasic) %>%
      addTiles(
        group = "Imagery",
        urlTemplate = ESRIimagery) %>%
      addTiles(
        group = "Topo",
        urlTemplate = ESRItopo) %>%
      addTiles(
        group = "NatGeo",
        urlTemplate = ESRINatGeo) %>%
      addLegend(pal = pal, values = c(0,5,7,100,500,1000,2000,5000),
                title = "CGR Vulnerability Assessment") %>%
      addPolygons(data = nps_im, color = "#33CB46", fill = NA, weight = 2,
                  group = 'NPS units') %>%
      addLayersControl(
        map = .,
        baseGroups = c("Map", "Imagery", "Topo", "NatGeo"),#, "CGR_Map"),
        options = layersControlOptions(collapsed = T))


  })

  # Set up ability to zoom to given park
  observeEvent(input$parkZoom, {
    req(input$park)

    park_selected <- nps_im_df %>% filter(NETCOD == input$network)

    leafletProxy('CGIMap') %>%
      clearControls() %>%
      #clearPopups() %>%
      setView(
        lng =  park_selected$long,
        lat = park_selected$lat,
        zoom = 12)})


  # Zoom to a park
  # observe({
  #   req(input$CGIMap_zoom)
  #
  #   leafletProxy("CGIMap") %>%
  #     addCircleMarkers(
  #       data = park_df,
  #       radius = 4,
  #       lng = park_df$long,
  #       lat = park_df$lat,
  #       layerId = park_df$UNIT_CODE,
  #       label = if(input$CGIMap_zoom > 12) park_df$UNIT_NAME else UNIT_CODE,
  #       labelOptions = labelOptions(noHide = TRUE,
  #                                   textOnly = TRUE,
  #                                   direction = "bottom",
  #                                   textsize = "11px"),
  #       fillColor = "#33CB46",
  #       fillOpacity = 0.75,
  #       weight = 1,
  #       color = "DimGrey"
  #     )
  #
  # })

  # Reset view of map panel
  observeEvent(input$reset_view, {
    reset("parkZoom")

    updateSelectizeInput(session, 'network',
                         choices = c("Choose a network" = "ALL",
                                     "CHDN", "GULN", "HTLN", "NGPN",
                                     "ROMN", "SCPN", "SODN", "SOPN"))

    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     c(park_list)))

    leafletProxy("CGIMap") %>%
      clearPopups() %>%
      clearControls() %>%
      setView(lng = cent[,1], lat = cent[,2], zoom = 5)


  })
}
