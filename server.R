server <- function(input, output, session) { 

  entire_home=0;
  
  # START PAGE 1: Find Map --------------------------------------------------------------------------------------------------------------
  
  output$map_airbnb <- renderLeaflet({
    map.data <- airbnb %>% 
      filter(
              price_per_night >= input$inp_map_price[1], 
             price_per_night <= input$inp_map_price[2], 
             min_nights >= input$inp_map_night) 
    
    if(input$inp_map_neighgroup != "All"){
      map.data <- airbnb %>%
        filter(neighbourhood_group == input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1], 
               price_per_night <= input$inp_map_price[2], 
               min_nights >= input$inp_map_night
        )


    }
    
    
    
    #popup
    labels <- sprintf(
      "<strong>%s</strong><br/>neighbourhood: %s <br>Room type: %s <br>Min. Nights: %g <br/>Price: $ %g /night",
      map.data$name, 
      map.data$neighbourhood, 
      map.data$room_type, 
      map.data$min_nights,
      round(map.data$price_per_night,1)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(
      map.data %>% select(longitude,neighbourhood_group,neighbourhood,latitude,price),
      options = leafletOptions(minZoom = 2)) %>%
      setView(lng = -73.95, lat = 40.73,zoom = 10) %>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      clearShapes() %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
    
    
    
  })
  
  
  output$out_map_description <- renderText({
    group = "All Neighbourhood Group"
    
    airbnb.content <- airbnb %>% 
      group_by(room_type) %>%
      filter(price_per_night >= input$inp_map_price[1], 
             price_per_night <= input$inp_map_price[2],
             min_nights >= input$inp_map_night) %>% 
      summarise(
        jumlah = n() 
      ) %>% 
      ungroup() %>%
      arrange(jumlah) 
    
    entire_home <-  airbnb.content %>% filter(room_type == "Entire home") %>% select(jumlah)
    private_room <-  airbnb.content %>% filter(room_type == "Private room") %>% select(jumlah)
    shared_room <-  airbnb.content %>% filter(room_type == "Shared room") %>% select(jumlah)
    
    
    if(input$inp_map_neighgroup != "All"){
      airbnb.content <- airbnb %>% 
        filter(neighbourhood_group==input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1], 
               price_per_night <= input$inp_map_price[2],
               min_nights >= input$inp_map_night
        ) %>% 
        group_by(room_type) %>% 
        summarise(jumlah = n()) %>% 
        ungroup() %>%
        arrange(jumlah) 
      
      entire_home <-  airbnb.content %>% filter(room_type == "Entire home") %>% select(jumlah)
      private_room <-  airbnb.content %>% filter(room_type == "Private room") %>% select(jumlah)
      shared_room <-  airbnb.content %>% filter(room_type == "Shared room") %>% select(jumlah)
    }
    
    entire_home <- ifelse(is.na(as.integer(entire_home)),0,as.integer(entire_home$jumlah))
    private_room <- ifelse(is.na(as.integer(private_room)),0,as.integer(private_room$jumlah))
    shared_room <- ifelse(is.na(as.integer(shared_room)),0,as.integer(shared_room$jumlah))
    
    paste("Total Host in ", group," with prices range in ",input$inp_map_price[1]," - ",input$inp_map_price[2]," is ", 
          comma(entire_home+private_room+shared_room)
    )
    
  })
  
  
  output$out_map_max_price <- renderValueBox({
    
    airbnb.maxprice <- airbnb %>%
      filter(price_per_night >= input$inp_map_price[1], price_per_night <= input$inp_map_price[2],min_nights >= input$inp_map_night) %>% 
      arrange(desc(price_per_night)) %>% 
      head(1)
    
    if(input$inp_map_neighgroup != "All"){
      airbnb.maxprice <- airbnb %>%
        filter(neighbourhood_group==input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1], 
               price_per_night <= input$inp_map_price[2],
               min_nights >= input$inp_map_night) %>% 
        arrange(desc(price_per_night)) %>% 
        head(1)
    }
    
    valueBox(
      value = paste("$",round(airbnb.maxprice$price_per_night,1)),
      subtitle = "Highest"
    )
    
  })
  
  
  output$out_map_avg_price <- renderValueBox({
    airbnb.avgprice <- airbnb %>% 
      filter(price_per_night >= input$inp_map_price[1], price_per_night <= input$inp_map_price[2],min_nights >= input$inp_map_night) %>% 
      summarise(avgprice=mean(price_per_night)) 
    
    if(input$inp_map_neighgroup != "All"){
      airbnb.avgprice <- airbnb %>%
        filter(neighbourhood_group==input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1],
               price_per_night <= input$inp_map_price[2],
               min_nights >= input$inp_map_night) %>% 
        summarise(avgprice=mean(price_per_night))
    }
    
    valueBox(
      value = paste("$",round(airbnb.avgprice$avgprice,1)),
      subtitle = "Avg."
    )
    
  })
  
  output$out_map_min_price <- renderValueBox({
    
    airbnb.minprice <- airbnb %>% 
      filter(price_per_night >= input$inp_map_price[1], price_per_night <= input$inp_map_price[2],min_nights >= input$inp_map_night) %>% 
      arrange(price_per_night) %>% 
      head(1)
    
    if(input$inp_map_neighgroup != "All"){
      airbnb.minprice <- airbnb %>%
        filter(neighbourhood_group==input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1], 
               price_per_night <= input$inp_map_price[2],
               min_nights >= input$inp_map_night) %>% 
        arrange(price_per_night) %>% 
        head(1)
    }
    
    valueBox(
      value = paste("$",round(airbnb.minprice$price_per_night,1)),
      subtitle = "Lowest"
    )
    
  })
  
  
  output$out_map_plot_room <- renderPlotly({
    airbnb.map_plot_room <- airbnb %>% 
      group_by(room_type) %>%
      filter(price_per_night >= input$inp_map_price[1], 
             price_per_night <= input$inp_map_price[2],
             min_nights >= input$inp_map_night) %>% 
      summarise(
        jumlah = n(), 
        mean_price = mean(price_per_night), 
        med_avail_365 = median(avail_365), 
        med_min_nights = median(min_nights)
      ) %>% 
      ungroup() %>%
      arrange(jumlah) 
    
    if(input$inp_map_neighgroup != "All"){
      airbnb.map_plot_room <- airbnb %>% 
        filter(neighbourhood_group==input$inp_map_neighgroup,
               price_per_night >= input$inp_map_price[1], 
               price_per_night <= input$inp_map_price[2],
               min_nights >= input$inp_map_night) %>% 
        group_by(room_type) %>% 
        summarise(jumlah = n(), mean_price = mean(price_per_night), med_avail_365 = median(avail_365), med_min_nights = median(min_nights)) %>% 
        ungroup() %>%
        arrange(jumlah) 
      
    }
    
    
    airbnb.map_plot_room <- airbnb.map_plot_room %>% 
      mutate(ratio_jumlah=round(jumlah/(sum(jumlah))*100,1)) %>% 
      mutate(
        popup=glue(
          "{room_type}: {jumlah} ({ratio_jumlah}%)
            Avg. Price: ${round(mean_price,2)}
            Avg. Min Night: {med_min_nights}
            Avg. availability: {med_avail_365} days/year"
        )
      )
    
    airbnb.map_plot_room$room_type = reorder(airbnb.map_plot_room$room_type,airbnb.map_plot_room$jumlah)
    plot.content <-  ggplot(airbnb.map_plot_room,aes(room_type,jumlah,text=popup))+
      geom_col(aes(fill=room_type))+
      coord_flip()+
      labs(
        x = NULL,
        y = NULL,
        title="Room Type Distirbution",
        subtitle = NULL,
        color="CSD"
      )+
      theme_minimal()+
      theme( 
        axis.title=element_text(size=7, face="bold"),
        axis.text.x=element_text(size=7,margin = margin(b=10)),
        axis.text.y=element_text(size=7),
        axis.text.y.left = element_text(margin = margin(l=10))
      )
    
    ggplotly(plot.content , tooltip = "text",height = 175) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(showlegend=FALSE)
    #layout(autosize = F, width = 200, height = 150, margin(t = 5,r = 5,b = 5,l = 5,unit = "pt"))
    
  })
  
  
  
  # END PAGE1 -------------------------------------------------------------------------------------------------------------
  
  
  
  # START PAGE2 ------------------------------------------------------------------------------------------------------
  
  output$out_hc_room <- renderHighchart({
    airbnb.price <- airbnb %>% 
      filter(neighbourhood_group %in% input$inp_neighbourhood_group_growth) %>% 
      group_by(neighbourhood_group,neighbourhood) %>% 
      summarise(jumlah = n(), lowest_price=min(price_per_night), highest_price=max(price_per_night), mean_price = round(median(price_per_night),2), med_min_nights = median(min_nights)) %>% 
      ungroup() %>%
      arrange(desc(jumlah)) %>%
      hchart('treemap', hcaes(x = 'neighbourhood', value = 'jumlah', color = 'jumlah')) %>%
      hc_add_theme(hc_theme_google()) %>% 
      hc_colorAxis(stops = color_stops(n = 10, colors = c("#f39c12","#00a65a","#0073b7"))) %>% 
      hc_title(text="Room Distribution based on Neighbourhood Group") %>% 
      hc_tooltip(shared = TRUE,
                 borderColor = "black",
                 pointFormat = "
                            <strong>Neighbourhood Group:</strong> {point.neighbourhood_group}<br>
                            <strong>Neighbourhood:</strong> {point.neighbourhood}<br>
                            <strong>Total Room:</strong> {point.jumlah}<br>
                            <strong>Highest Price:</strong> {point.highest_price}<br>
                            <strong>Lowest Price:</strong> ${point.lowest_price}<br>
                            <strong>Avg. Price:</strong> ${point.mean_price}<br>
                            ")
    
    airbnb.price
 
  })
  
  output$out_room_growth <- renderPlotly({
    airbnb.room <- airbnb %>% 
      filter(neighbourhood_group %in% input$inp_neighbourhood_group_growth,!is.na(last_review)) %>% 
      group_by(neighbourhood_group, room_type, tahun=year(last_review)) %>% 
      summarise(jumlah=n(),lowest_price=min(price_per_night), highest_price=max(price_per_night), mean_price = round(median(price_per_night),2)) %>% 
      ungroup() %>%
      arrange(neighbourhood_group,room_type,jumlah)%>% 
      mutate(
        popup=glue(
          "Neighbourhood: {neighbourhood_group}
            Year : {tahun}
            Room Type: {jumlah} {room_type} 
            Lowest Price: ${round(lowest_price,2)}
            Higest Price: ${round(highest_price,2)}
            Avg. Price: ${round(mean_price,2)}"
        )
      )
    
    airbnb.room$tahun <-  as.integer(airbnb.room$tahun)
    
    plot.airbnb.room <- ggplot(airbnb.room, aes(tahun ,jumlah))+
      geom_line(aes(color = room_type))+
      geom_point(aes(color = room_type, text=popup))+
      ggtitle("Growth of Rooms accros years")+
      labs(
        x = NULL,
        y = "Count",
        color = NULL
      )+
      facet_grid(~neighbourhood_group, scales = "free")+
      theme_economist() + 
      scale_color_economist()+
      scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2019))+
      theme(axis.text.x = element_text(angle = 90, hjust=1),
            plot.title = element_text(size=10))
    
    ggplotly(plot.airbnb.room,tooltip = "text", height = 250) %>% 
      #layout(legend = list(orientation = 'h',x = 0, y = 1.4)) %>% 
      config(displayModeBar = FALSE) 
  })
  
  
  
  output$out_popular_group <- renderPlotly({
    airbnb.popular <- airbnb %>% 
      filter(last_review >= input$date_range_review[1],last_review <=input$date_range_review[2]) %>% 
      group_by(neighbourhood_group,last_review) %>% 
      summarise(review=sum(reviews)) %>% 
      ungroup() %>% 
      mutate(
        popup = glue(
          "Neighbourhood group: {neighbourhood_group}
          {review} reviews on {last_review}"
        )
      )
    
    if(input$inp_popular_group != "All"){
      airbnb.popular <- airbnb %>%
        filter(neighbourhood_group == input$inp_popular_group,last_review >= input$date_range_review[1],last_review <=input$date_range_review[2]) %>% 
        group_by(neighbourhood_group,last_review) %>% 
        summarise(review=sum(reviews)) %>% 
        ungroup() %>% 
        mutate(
          popup = glue(
            "{review} reviews on {last_review}"
          )
        )
    }
    
    plot.airbnb.popular <- ggplot(airbnb.popular, aes(last_review, review)) +
      geom_point(na.rm=TRUE, aes(color=neighbourhood_group,size=review, text=popup), alpha=0.5)+
      geom_smooth(na.rm=TRUE,method="lm", color = "#fcce00")+
      ggtitle("How popular is Neigbourhood Group?",
              subtitle = "Number of Reviews across years")+
      labs(
        x = NULL,
        y = "Review",
        color = NULL
      )+theme_economist() + 
      scale_colour_wsj("colors6")+
      theme(plot.title = element_text(size=10))
    
    ggplotly(plot.airbnb.popular, tooltip="text", height = 280)%>% 
      config(displayModeBar = FALSE) %>% layout(showlegend=FALSE)
    
    
  })
  
  
  output$out_popular_neighbourhood <- renderPlotly({
    
    airbnb.popular.neighbourhood <- airbnb %>% 
      filter(last_review >= input$date_range_review[1],last_review <=input$date_range_review[2]) %>% 
      group_by(neighbourhood_group,neighbourhood,last_review) %>% 
      summarise(review=sum(reviews)) %>% 
      ungroup() %>% 
      mutate(
        popup = glue(
          "Neighbourhood group: {neighbourhood_group}
          Neighbourhood: {neighbourhood}
          {review} reviews on {last_review}"
        )
      )
    
    plot.airbnb.popular.neighbourhood <- ggplot(airbnb.popular.neighbourhood, aes(last_review, review)) +
      geom_point(na.rm=TRUE, aes(color=neighbourhood_group,size=review, text=popup), alpha=0.3)+
      geom_smooth(na.rm=TRUE,method="lm", color = "#fcce00")+
      ggtitle("How popular is Neigbourhood in this group?",
              subtitle = "Number of Reviews across years")+
      labs(
        x = NULL,
        y = "Review",
        color = NULL
      )+theme_economist() + 
      scale_colour_wsj("colors6")+
      theme(plot.title = element_text(size=10))
    
    if(input$inp_popular_group != "All"){
        airbnb.popular.neighbourhood <- airbnb %>% 
          filter(neighbourhood_group == input$inp_popular_group,last_review >= input$date_range_review[1],last_review <=input$date_range_review[2]) %>% 
          group_by(neighbourhood_group,neighbourhood,last_review) %>% 
          summarise(review=sum(reviews)) %>% 
          ungroup() %>% 
          mutate(
            popup = glue(
              "Neighbourhood group: {neighbourhood_group}
          Neighbourhood: {neighbourhood}
          {review} reviews on {last_review}"
            )
          )
        plot.airbnb.popular.neighbourhood <- ggplot(airbnb.popular.neighbourhood, aes(last_review, review)) +
          geom_point(na.rm=TRUE, aes(color=neighbourhood,size=review, text=popup), alpha=0.3)+
          geom_smooth(na.rm=TRUE,method="lm", color = "#fcce00")+
          ggtitle("How popular is Neigbourhood in this group?",
                  subtitle = "Number of Reviews across years")+
          labs(
            x = NULL,
            y = "Review",
            color = NULL
          )+
        theme_economist() + 
          scale_colour_wsj("colors6")+
          theme(plot.title = element_text(size=10))
      
    }
    
    ggplotly(plot.airbnb.popular.neighbourhood, tooltip="text",height = 280)%>% 
      config(displayModeBar = FALSE) %>% layout(showlegend=FALSE)
  })
  
  # END PAGE2 -----------------------------------------------------------------------------------------------------
  
  
  
  # PAGE3 ------------------------------------------------------------------------------------------------------
  output$table_airbnb_ny <- DT::renderDataTable(DT::datatable({
    
    airbnb.display <- airbnb %>%
      filter(neighbourhood_group %in% input$inp_neighbourhood_group) %>% 
      select(name,host_name,neighbourhood_group,neighbourhood,room_type,price,min_nights,price_per_night) %>% 
      arrange(neighbourhood_group,neighbourhood,price_per_night)
    
    if(input$rb_room_type != "All"){
      airbnb.display <- airbnb.display %>% 
        filter(airbnb.display$room_type == input$rb_room_type)
      
    }
    
    airbnb.display
    
    
  }))
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste("AirbnbNY-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      airbnb.display <- airbnb %>%
        filter(neighbourhood_group %in% input$inp_neighbourhood_group) %>% 
        select(name,host_name,neighbourhood_group,neighbourhood,room_type,price,min_nights,price_per_night) %>% 
        arrange(neighbourhood_group,neighbourhood,price_per_night)
      
      if(input$rb_room_type != "All"){
        airbnb.display <- airbnb.display %>% 
          filter(airbnb.display$room_type == input$rb_room_type)
        
      }
      write.csv(airbnb.display, file)
    }
  )
  
 
    
}