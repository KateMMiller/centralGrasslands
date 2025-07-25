library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(shinyjs)
library(htmltools)
library(leaflet.minicharts)

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
      # setMaxBounds(lng1 = nps_bbox$xmax*1.01,
      #              lng2 = nps_bbox$xmin*1.01,
      #              lat1 = nps_bbox$ymax*1.01,
      #              lat2 = nps_bbox$ymin*1.01) %>%
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
      # addLegend(pal = pal, values = c(0,5,7,100,500,1000,2000,5000),
      #           title = "CGR Vulnerability Assessment") %>%
      addPolygons(data = nps_im,
                  # lng = nps_im_df$long[nps_im_df$UNIT_CODE == input$park],
                  # lat = nps_im_df$lat[nps_im_df$UNIT_CODE == input$park],
                  color = "#33CB46", fill = NA, weight = 2,
                  group = 'CG parks') %>%
      # addPolygons(data = cgr_bound, color = "dimgrey", fill = "dimgrey",
      #             opacity = 0.2,
      #             weight = 2.5, group = "CGR boundary") %>%
      # addRasterImage(cgr_ras, colors = pal) %>%
      addLegend(pal = pal, values = values(cgr_ras)) %>%
      # addPolygons(data = cgr_shp,
      #             fillColor = c("#51284D", "#728946", "#EDECE6",
      #                           "#E40013", "black", "#FBD82B", "#37618D"),
      #             color = cgr_shp$gridcode,
      #             group = "CGR Vuln. Assmnt.") %>%
      #hideGroup("CGR Vuln. Assmnt.") %>%
      addMinicharts(
        type = 'pie',
        lng = park_prop_hab_wide2$long,
        lat = park_prop_hab_wide2$lat,
        park_prop_hab_wide2[,c("prop_Converted/Altered Grasslands",
                              "prop_Core Grassland",
                              "prop_Desert/Shrub",
                              "prop_Developed",
                              "prop_Forest",
                              "prop_Vulnerable Grasslands",
                              "prop_Water")],
        colorPalette = c("#51284D", "#728946", "#EDECE6",
                         "#E40013", "black", "#FBD82B", "#37618D"),
        width = park_prop_hab_wide2$pie_size,
        legendPosition = "bottomright"
        ) %>%
      addLayersControl(
        map = .,
        baseGroups = c("Map", "Imagery", "Topo", "NatGeo"),
        options = layersControlOptions(collapsed = F),
        overlayGroups = c("CG parks", "CGR boundary"))
  })

  # Zoom to a park; first add invisible circle markers to zoom to centroid
  observe({
    req(input$CGIMap_zoom)

    leafletProxy("CGIMap") %>%
      addCircleMarkers(
        data = nps_im_df,
        radius = 20,
        opacity = 0,
        fillOpacity = 0,
        lng = nps_im_df$long,
        lat = nps_im_df$lat,
        layerId = nps_im_df$UNIT_CODE#,
        #label = nps_im_df$UNIT_CODE,
        # labelOptions = labelOptions(noHide = TRUE,
        #                             textOnly = TRUE,
        #                             direction = "bottom",
        #                             textsize = "11px")
        )
  })


  # Set up ability to zoom to given park
  observeEvent(input$parkZoom, {
    req(input$park)

    park_selected <- nps_im_df[nps_im_df$UNIT_CODE == input$park,]

    leafletProxy('CGIMap') %>%
      clearControls() %>%
      #clearPopups() %>%
      setView(
        lng =  park_selected$long,
        lat = park_selected$lat,
        layerId = park_selected$UNIT_CODE,
        zoom = 12)})


  # Set up zoom
  observeEvent(input$park, {
    req(input$park)

    park_coords <- nps_im_df[nps_im_df$UNIT_CODE == input$park,]

    zoom_level <- 10

    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     nps_im_df$UNIT_CODE))

    leafletProxy('CGIMap') %>%
      #clearControls() %>%
      clearPopups() %>%
      setView(lng = park_coords$long,
              lat = park_coords$lat,
              zoom = zoom_level)
  })

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
