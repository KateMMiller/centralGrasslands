library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
#library(leaflet.minicharts)
library(crosstalk)
library(DT)

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
      addTiles(group = "Map", urlTemplate = NPSbasic) %>%
      addTiles(group = "Imagery", urlTemplate = ESRIimagery) %>%
      addTiles(group = "Topo", urlTemplate = ESRItopo) %>%
      addTiles(group = "NatGeo", urlTemplate = ESRINatGeo) %>%
      # addLegend(pal = pal, values = c(0,5,7,100,500,1000,2000,5000),
      #           title = "CGR Vulnerability Assessment") %>%
      addPolygons(data = nps_im,
                  # lng = nps_im_df$long[nps_im_df$UNIT_CODE == input$park],
                  # lat = nps_im_df$lat[nps_im_df$UNIT_CODE == input$park],
                  color = "#33CB46", fill = NA, weight = 2,
                  group = 'CG parks') %>%
      addPolygons(data = cgr_bound, color = "#B5AF4D", fill = "#E3E3B3",
                  opacity = 0.2,
                  weight = 2.5, group = "CGR boundary") %>%
      addLayersControl(
        map = ., position = "bottomleft",
        baseGroups = c("Map", "Imagery", "Topo", "NatGeo"),
        options = layersControlOptions(collapsed = F),
        overlayGroups = c("CG parks", "CGR boundary"))
  })

  # Zoom to a park; first add invisible circle markers to zoom to centroid
  observe({
    req(input$CGIMap_zoom)

    leafletProxy("CGIMap") %>%
      addCircleMarkers(
        data = park_prop,
        radius = 20,
        opacity = 0,
        fillOpacity = 0,
        lng = park_prop$long,
        lat = park_prop$lat,
        layerId = park_prop$UNIT_CODE,
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

    park_selected <- park_prop[park_prop$UNIT_CODE == input$park,]

    leafletProxy('CGIMap') %>%
      clearControls() %>%
      #clearPopups() %>%
      setView(
        lng =  park_selected$long,
        lat = park_selected$lat,
        layerId = park_selected$UNIT_CODE,
        zoom = park_selected$zoom)})


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

  output$prop_hab_dt <-
    renderDataTable({datatable(df_shared,
                              class = 'cell-border stripe', rownames = FALSE,
                              extensions = c("FixedColumns", "Buttons"),
                              colnames = c("Park Code", "Park Name", "Network", "Total acres",
                                           "% Core grassland", "% Vulnerable grassland",
                                           "% Conv./alt. grassland", "% Desert/shrub",
                                           "% Developed", "% Forest", "% Water",
                                           "Acres core", "Acres vulnerable",
                                           "Acres conv./alt.", "Acres desert/shrub",
                                           "Acres forest", "Acres developed",
                                           "Acres water", "Long", "Lat", "zoom"),
                           options = list(
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               "$('body').css({'font-size': '11px'});",
                               "$('body').css({'font-family': 'Arial'});",
                               "$(this.api().table().header()).css({'font-size': '11px'});",
                               "$(this.api().table().header()).css({'font-family': 'Arial'});",
                               "}"),
                             pageLength = 28,
                             autoWidth = FALSE, scrollX = '850px',
                             scrollY = '600px', scrollCollapse = TRUE,
                             fixedColumns = list(leftColumns = 1),
                             dom = "Blfrtip", buttons = c('copy', 'csv', 'print'),
                             columnDefs = list(#list(width = '200px', targets = 1),
                               list(className = 'dt-center', targets = c(0, 2:17)))
                             ),
                           filter = list(position = c('top'), clear = FALSE)
  ) %>% formatCurrency("acres",currency = "", mark = ",", digits = 1)}, server = F)

}
