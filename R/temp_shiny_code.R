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
    # addRasterImage(cgr_ras, colors = pal) %>%
    # addLegend(pal = pal, values = values(cgr_ras)) %>%
    addPolygons(data = cgr_shp,
                fillColor = c("#51284D", "#728946", "#EDECE6",
                              "#E40013", "black", "#FBD82B", "#37618D"),
                color = cgr_shp$gridcode,
                group = "CGR Vuln. Assmnt."
    ) %>%
    #hideGroup("CGR Vuln. Assmnt.") %>%
    # addMinicharts(
    #   type = 'pie',
    #   lng = park_prop_hab_wide2$long,
    #   lat = park_prop_hab_wide2$lat,
    #   park_prop_hab_wide2[,c("prop_Converted/Altered Grasslands",
    #                         "prop_Core Grassland",
    #                         "prop_Desert/Shrub",
    #                         "prop_Developed",
    #                         "prop_Forest",
    #                         "prop_Vulnerable Grasslands",
    #                         "prop_Water")],
    #   colorPalette = c("#51284D", "#728946", "#EDECE6",
    #                    "#E40013", "black", "#FBD82B", "#37618D"),
    #   width = park_prop_hab_wide2$pie_size,
    #   legendPosition = "bottomright"
    #   ) %>%
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
