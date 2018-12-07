library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(shinydashboard)
library(leaflet)
library(raster)
library(dplyr)
library(DT)

title=tags$a(
             tags$img(src="ODC.png",height="70",width="70"),
             "ODC", tags$img(src="DATA_expert.png",height='70',width='70'))


ui = dashboardPage(
	dashboardHeader(title = title , titleWidth = 1000),
	dashboardSidebar(
		fileInput(
			"db",
			"Choose a database",
			multiple = FALSE,
			accept = "text/csv"
		),
		actionButton("update", "Visualize")
		
	),
	
	dashboardBody(fluidPage(
		tabsetPanel(
			type = "tab",
			
			tabPanel(
				"Map",
				leafletOutput("m"),
				infoBoxOutput("min")
			)
			,tabPanel("Data", DT::dataTableOutput("data"))
		)
	))
)


server = function(input, output) {
  tnMAP = reactive({
    req(input$db)
    req(input$update)
    T = read.csv(input$db$datapath, sep = ",",stringsAsFactors = F)
    
    data = T[,4:5]
    data=table(factor(data[,2]))
    data=as.data.frame(data)
    data=`colnames<-`(data,c("Gouvernorat","nbr_reclamations"))
    data = arrange(data, by = Gouvernorat)
    
    tnMAP = getData(name = "GADM",
                    country = "TUN",
                    level = 1)
    
    tnMAP = cbind.Spatial(tnMAP, data)
    tnMAP
  })
	
  prices = reactive({
    req(input$db)
    req(input$update)
    return(tnMAP()@data[, 15])
  })
  
  
  
  output$data = DT::renderDataTable(as.data.frame(tnMAP()@data[,c(14,15)]))
  
  
  
  output$m=renderLeaflet(	leaflet(tnMAP()) %>%
                            addProviderTiles(providers$CartoDB.Positron) %>%
                            addPolygons(
                              fillColor = ~ colorNumeric("Reds", tnMAP()@data[, 15])(tnMAP()@data[, 15]),
                              fillOpacity = 10,
                              col = "black",
                              weight = 1.1,
                              opacity = 0.7,
                              highlight = highlightOptions(
                                weight = 4.0,
                                color = "#FFFFFF",
                                fillOpacity = 0.7,
                                bringToFront = TRUE
                              ),
                              label = sprintf(
                                "<strong>%s</strong><br/>%g",
                                tnMAP()@data$NAME_1,
                                tnMAP()@data[, 15]
                              ) %>% lapply(htmltools::HTML) 
                              ,
                              labelOptions = labelOptions(
                                style = list("font-weight" = "normal",
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"
                              )
                            ) %>%
                            addLegend(
                              pal =  colorNumeric("Reds", tnMAP()@data[, 15]),
                              values = tnMAP()@data[, 15],
                              opacity = 1.5,
                              position = "bottomright",
                              title = "Prix"
                            ) %>%
                            setView(lat = 33.861105,lng = 9.297884,zoom = 6)%>% 
                            addProviderTiles("Esri.WorldImagery")
                          
                          
  )
  
  
	
  output$min =  renderInfoBox({
    infoBox(
      "Min",
      paste(tnMAP()@data[which(prices() == min(prices())), 14][1], min(prices()), sep =
              " \n"),
      color = "aqua",
      fill = TRUE
    )
  })
  output$max =  renderInfoBox({
    infoBox(
      "Max",
      paste(tnMAP()@data[which(prices() == max(prices())), 14][1], max(prices()), sep =
              " \n"),
      color = "red",
      fill = TRUE
    )
  })
 

  
  
	
}


shinyApp(ui = ui, server = server)

