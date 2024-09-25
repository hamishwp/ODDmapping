library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(magrittr)
library(xtable)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = 180, lat = -17.7, zoom = 3) %>%
      addResetMapButton() %>%
      
      # addLayersControl(
      #   overlayGroups = c('Population'),
      #   options = layersControlOptions(collapsed = T),
      #   position = "bottomleft"
      # ) %>%
      
      addRasterImage(population,group = "Population",colors = qpal,opacity=0.2, attribution = "SEDAC @ Columbia University") %>% 
      
      addLegend(position = "bottomleft",colors = leg_pal, labels = leg_val, 
                na.label = "NO DATA",title = "Population Count", opacity = 1, group = "Population")%>%
      
      # addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
      #                circleMarkerOptions = F, polygonOptions = F) 
      addDrawToolbar(polylineOptions = F,markerOptions = F,circleMarkerOptions = F,
        targetGroup='Bounding Box',singleFeature = T,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('Population','Bounding Box'), 
                       options = layersControlOptions(collapsed=FALSE),
                       position = "bottomleft") %>%
      addStyleEditor()
    
  })

  # Filter the database by input values
  DateRanges <- reactive({
    if (is.null(input$dates)) return(DispData)
      # return(filter(dfGDACS,sdate>(Sys.Date()-90)))
      
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    if(!input$haz=="All") DispData%<>%filter(hazard==input$haz)
    
    filter(DispData,
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2] &
             sdate>=input$dates[1] & sdate<=input$dates[2])# &
             # alertscore>=input$alertvals[1] & alertscore<=input$alertvals[2])
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    modDF<-DateRanges()
    if(NROW(modDF)>0){
      leafletProxy("map", data=modDF) %>% clearMarkers() %>% # addTiles() %>%
        # addAwesomeMarkers(~Longitude, ~Latitude, icon=~Icons[hazard_type], label=~as.character(alertscore))
        addAwesomeMarkers(~Longitude, ~Latitude, 
                          # icon=~icon("creative-commons-sampling"),
                          icon=~Icons[hazard],
                          label=~as.character(gmax), 
                          layerId = ~eventid,
                          group = "markers")
    } else leafletProxy("map", data=modDF) %>% clearMarkers()
    
  })

  GenerateErrorMessage<-function() {
    tags$code(sprintf("No Displacement Data Available"))
  }
  
  showHazardFull <- function(id) {
    
    ODDy<-tryCatch(readRDS(unique(DispData$filename[DispData$eventid==id])), error = function(e) NA)
    # if(is.na(ODDy)) {GenerateErrorMessage() ; break}
    if(is.na(ODDy)) break
    
    # ODDy@data$Disp[ODDy@data$Dis<1]<-NA
    
    cIndies<-ODDy@cIndies
    predictDisp<-PrepPred(ODDy@predictDisp)
    hazdates<-ODDy@hazdates
    
    # output$predictedDisp<-renderTable({
    #   txt<-xtable(predictDisp,caption="Observed displacements from IDMC database vs predicted from IIDIPUS")#,include.rownames=FALSE)
    # })
    
    if(ODDy@hazard=="EQ"){

      df<-data.frame()
      for (mag in (ceiling(ODDy@I0)):(floor(max(ODDy@data$hazMean1,na.rm = T)))) {
        df%<>%rbind(data.frame("Intensity"=mag,"Population Exposed"=as.integer(sum(floor(ODDy$Population[ODDy$hazMean1>=mag]/10),na.rm = T))))
      }
      if(nrow(df)>3) df<-df[(nrow(df)-2):nrow(df),]
      output$affectedpop<-renderTable({
        txt<-xtable(df)#,include.rownames=FALSE)
      })
    }
    
    if(input$ODDvariable=="Disp") ODDy[[input$ODDvariable]][ODDy[[input$ODDvariable]]==0]<-NA
    
    if(input$ODDvariable=="isochrone") {
      locz<-convert2sf(input$map_draw_all_features$features[[1]][[3]]$coordinates)
      tmp<-osrmIsochrone(loc=locz,
                         breaks=seq(from=0,to=60,length.out=4),res = 50,returnclass = "sf")
    } else {
      
      if(is.null(ODDy[[input$ODDvariable]])) {GenerateErrorMessage() ; break}
      
      tmp<-ODDy[input$ODDvariable]%>%as.raster.ODD()
      rang<-range(ODDy[[input$ODDvariable]],na.rm = T)
    }
    
    if(startsWith(input$ODDvariable,"haz")) ppp<-0:50/50
    else ppp<-c(0,0.01,0.03,0.05,0.1,0.3,0.5,1)
    
    # print(summary(ODDy))
    # print(sum(ODDy[["Disp"]],na.rm = T))
    # print(input$ODDvariable)
    ####### Create a tickbox to choose whether to plot hazard or displacement, default is displacement
    coly<-colorQuantile("plasma", domain = rang,
                         probs = ppp,
                         na.color = "transparent")
    # leafletProxy("map", data=modDF) %>% clearImages(group="Population") %>% 
    leafletProxy("map") %>% clearImages() %>%
    addRasterImage(x = tmp, opacity = 0.5,colors = coly, group = "IIDIPUS") %>%
      addLegend(position = "topleft", colors=coly(rev(ppp[-1]*rang[2])), title = "Value",
                labels=rev(formatC(ppp[-1]*rang[2],digits=1,format="f"))) %>% #,group="IIDIPUS",
                # na.label = "NO DATA",title = "Value", opacity = 1)%>%
      flyToBounds(ODDy@bbox[1],ODDy@bbox[2],ODDy@bbox[3],ODDy@bbox[4])
    
    output$sumLocation<- renderUI({
      
      tagList(
        textInput("sum_place","Sum all displacements in a given region:",
                  value = "'place, country' ('Bath, UK')"),
        actionBttn("search_sum","Submit Boundary Request (ORS)",
                   color="primary",style = "simple",size = "sm")
        # helpText("In the form of 'place, country' ('Bath, UK')")
      )
      
    })
    
    # xxchange <- reactive({
    #   paste(req(input$search_sum!=0) || req(!is.null(input$map_draw_all_features)))
    # })
    
    # output$hazy<-renderUI({paste0(input$search_sum)})
    
    ShapeDisp<-eventReactive(req(input$map_draw_all_features),{
      
      # return(tagList(
      #   tags$h5(paste0(input$sum_place))#,tags$br(),
      # ))
      
      # if(!is.null(input$map_draw_all_features)){
      # output$hazy<-renderPrint({input$map_draw_all_features})
      shapez<-input$map_draw_all_features$features[[1]]
      if(shapez$properties$feature_type=="circle") {
        # long<-shapez[[3]]$coordinates[1]
        # lat<-shapez[[3]]$coordinates[2]
        disty<-geosphere::distm(as.numeric(shapez[[3]]$coordinates), ODDy@coords, fun = distHaversine) 
        indy<-disty<=shapez[[2]]$radius
        dispy<-as.integer(ceiling(sum(ODDy@data$Disp[indy],na.rm = T)))
        # output$hazy<-renderUI({
        return(tagList(
          # tags$h4(paste0("Number of people displaced = ",dispy)),
          tags$code(sprintf("%s people displaced inside circle", formatC(dispy,format="d", big.mark=",")))#,tags$br(),
          # tags$h5(paste0("Circle of radius ",shapez[[2]]$radius," and location {",
          #                strcat(as.character(shapez[[3]]$coordinates),collapse = ","),"}"))
          # tags$h5(sprintf("Circle of radius %s km", formatC(shapez[[2]]$radius/1000,digits=2,format="f", big.mark=","))),#tags$br(),
          # tags$h5(sprintf("Location long=%s, lat=%s", formatC(as.numeric(shapez[[3]]$coordinates[1]),format="f",digits=2),
          # formatC(as.numeric(shapez[[3]]$coordinates[2]),format="f",digits=2)))
        ))
        # })
        
      } else if(shapez$properties$feature_type=="polygon"){
        
      } else if(shapez$properties$feature_type=="rectangle"){
        indy<-ODDy@coords[,1]>=shapez[[2]]$coordinates[1] &
          ODDy@coords[,1]<=shapez[[2]]$coordinates[3] &
          ODDy@coords[,2]>=shapez[[2]]$coordinates[2] &          
          ODDy@coords[,2]<=shapez[[2]]$coordinates[4] &
          dispy<-as.integer(ceiling(sum(ODDy@data$Disp[indy],na.rm = T)))
          # output$hazy<-renderUI({
          return(tagList(
            # tags$h4(paste0("Number of people displaced = ",dispy)),
            tags$code(sprintf("%s people displaced inside rectangle", formatC(dispy,format="d", big.mark=",")))#,tags$br(),
            # tags$h5(paste0("Circle of radius ",shapez[[2]]$radius," and location {",
            #                strcat(as.character(shapez[[3]]$coordinates),collapse = ","),"}"))
            # tags$h5(sprintf("Circle of radius %s km", formatC(shapez[[2]]$radius/1000,digits=2,format="f", big.mark=","))),#tags$br(),
            # tags$h5(sprintf("Location long=%s, lat=%s", formatC(as.numeric(shapez[[3]]$coordinates[1]),format="f",digits=2),
            #                 formatC(as.numeric(shapez[[3]]$coordinates[2]),format="f",digits=2)))
          ))
          # })
      } else return(tagList(tags$h5("Unknown selection shape"))) #output$hazy<-renderPrint({"Unknown selection shape"})
      
    })
    
    output$shapes<-renderUI({ShapeDisp()})
    
    AreaDisp<-eventReactive(req(input$search_sum!=0),{
      
      
      if(!is.null(input$sum_place)) {
        # charz<-getbb(input$sum_place,)
        # if (!any(is.na(charz)) & length(charz)==4) {
        #   scoordz<-0.5*c(charz[1]+charz[3],charz[2]+charz[4])
        # } else return(tagList(tags$h5("Unknown region")))
        
        bbox<-pracma::strcat(as.character(ODDy@bbox[1:4]),collapse = ",")
        # Search for the bounding box of the place
        bb <- getbb(input$sum_place,viewbox=bbox,format_out = 'sf_polygon')
        
        # In case the location is outside the zoomed in area, search for the bounding box of the place worldwide
        if(any(is.na(st_coordinates(bb)))) bb <- getbb(input$sum_place, format_out = 'sf_polygon')
        if(any(is.na(st_coordinates(bb)))) return(tagList(tags$code("No such region/area exists")))
        
        coordz<-st_coordinates(bb)
        if(ncol(coordz)>3) {polysum<-coordz[rowMeans(coordz[,3:ncol(coordz)])==1,1:2]
        } else polysum<-coordz[1:2]
        
        notnan<-!is.na(ODDy@data$Disp)
        dtmp<-ODDy@data$Disp[notnan]
        total<-sum(dtmp[point.in.polygon(ODDy@coords[notnan,1],
                                         ODDy@coords[notnan,2],
                                         polysum[,1],
                                         polysum[,2])>0],na.rm = T)
        rm(dtmp,notnan,bb)
        
        leafletProxy("map") %>% clearShapes() %>% addPolygons(lng = polysum[,1],
                                                              lat = polysum[,2],
                                                              weight = 5,opacity = 0.5) %>%
          flyToBounds(min(polysum[,1]),min(polysum[,2]),max(polysum[,1]),max(polysum[,2]))
        
        return(tagList(
          tags$code(sprintf(paste0("%s people displaced inside ",input$sum_place), 
                            formatC(total,format="d", big.mark=",")))
        ))
      }
    })
    
    output$places<-renderUI({AreaDisp()})
    
    output$shelter<- renderUI({
      
      tagList(
        textInput("place", "Insert shelter location:", 
                  value = "'place, country' ('Bath, UK')  or 'longitude,latitude' ('51.3,174.2')"),
        actionBttn("searcher","Submit Location Request (ORS)",color="primary",style = "simple",size = "sm")
        # helpText("In the form of 'place, country' ('Bath, UK')"," or 'longitude,latitude' ('51.3,174.2')")
      )
      
    })
    
    checkerz<-eventReactive(input$searcher,{
      
      numz<-extractnumbers(input$place)
      charz<-as.numeric(getbb(input$place))
      
      # Perform checks
      if(length(numz)==2 & abs(numz[2])<=90 & abs(numz[1])<=180) {
        scoordz<-numz
      } else if (!any(is.na(charz)) & length(charz)==4) {
        scoordz<-0.5*c(charz[1]+charz[3],charz[2]+charz[4])
      } else return("")
      
      # if(!is.null(input$map_draw_all_features)){
        # dcoordz<-input$map_draw_all_features$features[[1]][[3]]$coordinates
      shapez<-input$map_draw_all_features$features[[1]]
      disty<-geosphere::distm(as.numeric(shapez[[3]]$coordinates), ODDy@coords, fun = distHaversine) 
      indy<-disty<=shapez[[2]]$radius
      dcoordz<-unname(ODDy@coords[which(indy)[which.max(ODDy@data$Disp[indy])],])
      
      # return(strcat(as.character(unname(ODDy@coords[which(indy)[which.max(ODDy@data$Disp[indy])],])),collapse = ","))
      # } else if (req(input$sum_place)!="'place, country' ('Bath, UK')") {
      #   bbox<-strcat(as.character(ODDy@bbox[1:4]),collapse = ",")
      #   bb<-getbb(input$sum_place,viewbox=bbox)
      #   return(strcat(as.character(bb),collapse = ","))
      #   if(!any(is.na(bb)))  {dcoordz<-colMeans(cbind(bb[c(1,3)],bb[c(2,4)]))
      #   } else return("Error in coords at place location (above)")
      # }
      
      # NEED CENTER OF POLYGON SELECTED - EITHER VIA max(disp) OR centroid
      # if(length(dcoordz)!=2) dcoordz<-colMeans(cbind(input$map_bounds[[c(2,4)]],input$map_bounds[[c(1,3)]]))
      if(length(dcoordz)!=2) return("Have you selected an event?")
        
      # return(strcat(as.character(rbind(scoordz,dcoordz)),collapse = ","))
        # return(strcat(as.character(dcoordz),collapse = ","))

      route <- osrm::osrmRoute(loc=convert2sf(rbind(scoordz,dcoordz)),
                          returnclass = "sf")
      
      if(is.null(route)) return("Search not successful, use shape/area first?")
      
      leafletProxy("map") %>% addPolylines(lng = st_coordinates(route)[,1],
                                           lat = st_coordinates(route)[,2],
                                           color = "black",weight = 10,opacity = 1)
      
      oroute<-st_drop_geometry(route)[3:4]

      return(paste0("Driving time = ",as.integer(oroute[[1]])," mins, distance = ",as.integer(oroute[[2]])," km"))
      
    })
    
    output$disttime<-renderText({checkerz()})
    
  }
  
  # Show a popup at the given location
  showHazardPopup <- function(id,lat,lng) {
    
    poly<-polyframe[polyframe$eventid==id,] # polyframe%>%dplyr::filter(eventid==id)
    poly[poly$date==min(poly$date),]
    hsub<-helix%>%filter(event_id==unique(poly$helix_id)[1])
    tsev<-severity%>%filter(eventid==unique(hsub$event_id))
    
    # output$hazy<-renderPrint({paste0(c(min(poly$Longitude), min(poly$Latitude), max(poly$Longitude), max(poly$Latitude)))})
    
    minimapz<-get_stamenmap(bbox = c(min(poly$Longitude), min(poly$Latitude), max(poly$Longitude), max(poly$Latitude)),
                            zoom = 6,maptype = "terrain-background")
    p<-ggmap(minimapz) + labs(x="Longitude", y="Latitude") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
    for (j in unique(poly$ncontour)){
      p<-p+geom_polygon(data = filter(poly,ncontour==j),aes(x=Longitude,y=Latitude,group=Intensity,colour=Intensity),
                        alpha=0,na.rm = T,size=1)
    }
    p
    # p<-p+geom_label(data = cities, aes(long, lat, label = name), size = 4, fontface = "bold", nudge_x = 0.15,nudge_y = -0.15)

    svg(filename= paste(idirectory,"plot.svg", sep = "/"),
        width = 500 * 0.007, height = 300 * 0.007)
    print(p)
    dev.off()

    strz<-strsplit(hsub$event_name[1],split = " - ")[[1]]
    if(is.na(strz[3])) strz[3]<-format(min(hsub$start_date,na.rm = T), "%d/%m/%Y")
    if(is.na(strz[3])) strz[3]<-format(min(hsub$date,na.rm = T), "%d/%m/%Y")
    if(is.na(strz[2])) strz[2]<-paste0("(",round(mean(poly$Longitude[poly$ncontour==1]),2),
                                       " , ",round(mean(poly$Latitude[poly$ncontour==1]),2),")")

    content <- paste0(as.character(tagList(
                          # tags$h4("     GDACS Alertscore:", as.integer(max(poly$alertscore))), tags$br()
      # tags$h4(hsub$event_name[1]), tags$br()
      tags$h4(paste(strz[1],strz[3])),
      tags$h5(paste0("Location: ",strz[2])),
      tags$code(sprintf("Max Displacement: %s", formatC(max(hsub$figure), big.mark=","))), tags$br(),
      tags$code(sprintf("Vulnerability: %s %%", formatC(tsev$VU[1], big.mark=","))), tags$br(),
      tags$code(sprintf("Physical Exposure: %s %%", formatC(tsev$HA.NAT.EQ[1], big.mark=","))),tags$br(), 
      tags$code(sprintf("Coping Capacity (Infrastructure): %s %%", formatC(tsev$CC.INF[1], big.mark=","))), tags$br(),
      tags$code(sprintf("INFORM Index: %s %%", formatC(tsev$INFORM[1], big.mark=","))), tags$br(),
      # tags$code(sprintf("Max GDP-PPP Exposed to >6M: %s $", formatC(max(tsev$MaxGDP_6M,na.rm=T), big.mark=","))), 
      tags$br()
                          # tags$strong(HTML(sprintf("%s, %s %s",
                          #   selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
                          # ))), tags$br(),
                          # sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
                          # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
                          # sprintf("Adult population: %s", selectedZip$adultpop)
                        )),
                      paste(readLines(paste(idirectory,"plot.svg",sep="/")), collapse = ""))

    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id,
                                      options = popupOptions(maxWidth = 500))
    
    output$affectedpop<-renderTable({
      txt<-xtable(data.frame("Magnitude"=cIntMap[2:7],
                              "Population Exposed"=tsev%>%
                                dplyr::select(paste0(cIntMap[2:7],"-PopCounts"))%>%t()%>%as.character))#,include.rownames=FALSE)

      # txt%>%str_replace(pattern="rll",replacement = "r|ll")
    })
    
    # output$IDPs <- renderPlot({
    #   
    #   hsub%>%filter(type=="IDPs (Stock)")%>%group_by(date)%>%summarise(stock=max(figure)) %>%
    #     ggplot(aes(date,stock)) + geom_point() + xlab("Date") + ylab("IDP Stock")
    #   # hsub%>%filter(type=="IDPs (Stock)")%>%
    #   #   ggplot(aes(date,figure)) + geom_point() + xlab("Date") + ylab("IDP Stock")
    #   
    # })
    
    # leafletProxy("map") %>% addPopupGraphs(list(p), group = "markers")
    # leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    
    # selectedZip <- allzips[allzips$zipcode == zipcode,]
    # content <- as.character(tagList(
    #   tags$h4("Score:", as.integer(selectedZip$centile)),
    #   tags$strong(HTML(sprintf("%s, %s %s",
    #     selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #   ))), tags$br(),
    #   sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    #   sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    #   sprintf("Adult population: %s", selectedZip$adultpop)
    # ))
    # leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observeEvent(input$map_marker_click,{
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    
    if (is.null(event))
      return()

    isolate({
      
      # showHazardPopup(event$id,event$lat,event$lng)
      showHazardFull(event$id)
      
    })
  })

  ## Data Explorer ###########################################

  TableFilter <- reactive({
    
    if (!is.null(input$iso3s)) DispData%<>%filter(iso3%in%input$iso3s)
    if (!is.null(input$tabledates)) DispData%<>%filter(sdate>=input$tabledates[1] & sdate<=input$tabledates[2])
    if (!is.null(input$tablehazard)) DispData%<>%filter(hazard%in%input$tablehazard)
    
    return(DispData)
    
  })
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectizeInput(session, "cities", choices = cities,
  #     selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #         is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #     selected = stillSelected, server = TRUE)
  # })

  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     id <- input$goto$id
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showHazardPopup(id, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })

  output$disptable <- DT::renderDataTable({

    # action <- DT::dataTableAjax(session, df, outputId = "ziptable")

    # DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    # DT::datatable(
    #   distinct(distinct(filter(arrange(distinct(dplyr::select(TableFilter(),c(iso3,sdate,hazard,hazard_severity,hazard_alert,gmax,HD,Shelt))),
    #                           desc(gmax)),gmax>0),gmax,iso3,.keep_all = T),sdate,.keep_all = T),
    #   escape = FALSE, 
    #               colnames=c("Country","Event Date", "Hazard", "Magnitude", "GDACS Alertscore", "Max. Displaced (IDMC)","Housing Damage Displacements", "No. Sheltered"))
    DT::datatable(
      arrange(dplyr::select(TableFilter(),-c(filename,Longitude,Latitude,fdate)),desc(sdate)),
      escape = FALSE, 
      colnames=c("Country","Max. Displaced (IDMC)", "Hazard","Event Date","ID","Value Confidence"))
  })
}
