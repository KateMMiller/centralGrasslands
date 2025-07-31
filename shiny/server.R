library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(shinyjs)
library(DT)

shiny_server <- function(session, input, output){

  # park_react <- reactive({
  #   #req(input$park, input$network)
  #  if(input$network == "" & input$park == ""){park_prop
  #   } else if(!input$park == ""){park_prop[park_prop$UNIT_CODE %in% input$park,]
  #   } else if(!input$network == ""){park_prop[park_prop$NETCODE %in% input$network,]
  #   }
  # })
#
#   park_shp_react <- reactive({
#     req(input$park)
#     cgr_shp |> filter(UNIT_CODE == input$park)
#   })

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

  pal_net1 <- colorRampPalette(colors =
                                c("#9EB39C", "#CECAB6", "#A99895", "#B8A096", "#455557",
                                  "#7B8661", "#7bb5a2", "#446072", "#E0C9BC", "#908171",
                                  "#77979C", "#644d3c", "#c6c97b", "#a7b39b", '#bec991',
                                  "#5f8971", "#5a8b5d", "#488691", "#1F4976", "#E8B487"))

  pal_net <- pal_net1(32)

  output$CGIMap <- renderLeaflet({
    leaflet() %>%
      # setView(lng = if(is.null(input$park)){cent[,1]} else {park_df()$long},
      #         lat =  if(is.null(input$park)){cent[,2]} else {park_df()$lat},
      #         zoom =  if(is.null(input$park)){5} else {park_df()$zoom}) %>%
      setView(cent[,1], cent[,2], zoom = 5) %>%
      addTiles(group = "Map", urlTemplate = NPSbasic) %>%
      addTiles(group = "Imagery", urlTemplate = ESRIimagery) %>%
      addTiles(group = "Topo", urlTemplate = ESRItopo) %>%
      addTiles(group = "NatGeo", urlTemplate = ESRINatGeo) %>%
      addPolygons(data = networks, group = "IMD networks",
                  color = pal_net, weight = 1, fillOpacity = 0.4,
                  popup = paste0(networks$NETNAME, " Network (", networks$ALPHACODE, ")"),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)) %>%
      addPolygons(data = cgr_bound, color = "#B5AF4D", fill = "#E3E3B3",
                  opacity = 0.2,
                  weight = 2.5, group = "CGR boundary") %>%
      addPolygons(data = nps_im, layerId = nps_im$UNIT_CO,
                  # lng = nps_im_df$long[nps_im_df$UNIT_CODE == input$park],
                  # lat = nps_im_df$lat[nps_im_df$UNIT_CODE == input$park],
                  color = "#33CB46", fill = NA, weight = 2,
                  group = 'CG parks') %>%
      hideGroup("IMD networks") %>%
      # addPolygons(data = park_shp_react(),
      #             fillColor = c("#51284D", "#728946", "#EDECE6",
      #                           "#E40013", "black", "#FBD82B", "#37618D"),
      #             color = park_shp_react()$gridcode,
      #             group = "CGR Vuln. Assmnt."
      # )
     addLayersControl(
        map = ., position = "bottomleft",
        baseGroups = c("Map", "Imagery", "Topo", "NatGeo"),
        options = layersControlOptions(collapsed = F),
        overlayGroups = c("CG parks", "CGR boundary", "IMD networks")) %>%
     addScaleBar()
  })

  # Zoom to a park; first add invisible circle markers to zoom to centroid
  observe({
    req(input$CGIMap_zoom)

    leafletProxy("CGIMap") %>%
      addCircleMarkers(
        data = park_prop,
        radius = 0,
        opacity = 0,
        fillOpacity = 0,
        lng = park_prop$long,
        lat = park_prop$lat,
        layerId = park_prop$UNIT_CODE
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
                                     sort(nps_im_df$UNIT_CODE)))

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
                         choices = c("Choose a network" = "",
                                     "CHDN", "GLKN", "GRYN", "GULN", "HTLN", "NGPN",
                                     "ROMN", "SCPN", "SODN", "SOPN", "UCBN"))

    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     c(sort(park_list))))

    leafletProxy("CGIMap") %>%
      clearPopups() %>%
      clearControls() %>%
      setView(lng = cent[,1], lat = cent[,2], zoom = 5)


  })

  observeEvent(input$CGIMap_click, {
    point_click <- input$CGIMap_click

    point.sf <- st_as_sf(data.frame(lng = point_click$lng,
                                    lat = point_click$lat),
                         coords = c("lng", "lat"), crs = 4326)

    nps_filt <- st_filter(nps_im, point.sf)

    content <-
    if(nrow(nps_filt) == 0){paste0("No park selected")
      } else {paste0("Park Code: ", nps_filt$UNIT_CODE, "<br>",
                     "Park Name: ", nps_filt$UNIT_NAME, "<br>",
                     "Network: ", nps_filt$NETCODE, "<br>",
                     "Total Acres: ", round(nps_filt$acres, 1), "<br>")
      }

    leafletProxy("CGIMap") %>%
      addPopups(lat = point_click$lat,
                lng = point_click$lng,
                popup = content)

  })

  output$prop_hab_dt <-
    renderDT({
      datatable(park_prop2,
                class = 'cell-border stripe', rownames = FALSE,
                extensions = c("FixedColumns", "Buttons"),
                colnames = c("Park Code", #"Park Name",
                             "Network", "CGR Bound", "Total acres",
                             "2024 Visitation",  "IMD Veg. Monitoring",
                             "% Core grassland", "% Vulnerable grassland",
                             "% Conv./alt. grassland", "% Desert/shrub",
                             "% Developed", "% Forest", "% Water",
                             "Acres core", "Acres vulnerable",
                             "Acres conv./alt.", "Acres desert/shrub",
                             "Acres forest", "Acres developed",
                             "Acres water", "Long", "Lat"#, "zoom"
                             ),
               options = list(
                              initComplete = htmlwidgets::JS(
                               "function(settings, json) {",
                               "$('body').css({'font-size': '11px'});",
                               "$('body').css({'font-family': 'Arial'});",
                               "$(this.api().table().header()).css({'font-size': '11px'});",
                               "$(this.api().table().header()).css({'font-family': 'Arial'});",
                               "}"),
                             pageLength = nrow(park_prop2),
                             autoWidth = FALSE, scrollX = '850px',
                             scrollY = '600px', scrollCollapse = TRUE,
                             fixedColumns = list(leftColumns = 1),
                             dom = "Blfrtip", buttons = c('copy', 'csv', 'print'),
                             columnDefs = list(#list(width = '200px', targets = 1),
                               list(className = 'dt-center', targets = c(0:2, 5:20)),
                               list(className = 'dt-right', targets = c(3:4)))
                             ),
                           filter = list(position = c('top'), clear = FALSE)) %>%
        formatCurrency("acres", currency = "", mark = ",", digits = 1) %>%
        formatCurrency("Recreation.Visits", currency = "", mark = ",", digits = 1)},
        server = F)

  # cgr_shp_park <- reactive(
  #   if(is.null(input$park)){cgr_bound
  #     } else {cgr_bound |> filter(UNIT_NAME == input$park)}
  # )

}
