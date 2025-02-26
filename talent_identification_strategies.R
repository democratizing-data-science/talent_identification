# Replication data and code for talent identification strategies
## First specify the packages of interest
packages = c("ggplot2", "tigris", "plotly", "leaflet", "zctaCrosswalk")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
options(tigris_use_cache = TRUE)


mp_math <- read.csv("https://raw.githubusercontent.com/democratizing-data-science/talent_identification/main/math_simulated.csv")

mphu <-ggplot(mp_math, aes(x=math_i, y=math_j, text=labels)) + geom_point(shape=1, alpha=0) + 
    geom_hline(yintercept=mean(mp_math$wx), lty=2) + 
    geom_vline(xintercept=mean(mp_math$x), lty=2) + theme_minimal() + 

    geom_point(data=mp_math[(mp_math$wx>=mean(mp_math$wx)&mp_math$x>=mean(mp_math$x))&mp_math$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=mp_math[(mp_math$x>=mean(mp_math$x)&mp_math$wx>=mean(mp_math$wx))&mp_math$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +
	
    geom_point(data=mp_math[(mp_math$x<mean(mp_math$x)&mp_math$wx>=mean(mp_math$wx))&mp_math$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=mp_math[(mp_math$x<mean(mp_math$x)&mp_math$wx>=mean(mp_math$wx))&mp_math$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +

    geom_point(data=mp_math[(mp_math$x<mean(mp_math$x)&mp_math$wx<mean(mp_math$wx))&mp_math$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.1) +
    geom_point(data=mp_math[(mp_math$x<mean(mp_math$x)&mp_math$wx<mean(mp_math$wx))&mp_math$is_inf==TRUE,], aes(x=x, y=wx), shape=9, alpha=.4) +

    geom_point(data=mp_math[(mp_math$x>=mean(mp_math$x)&mp_math$wx<mean(mp_math$wx))&mp_math$is_inf==FALSE,], aes(x=x, y=wx), shape=1, alpha=.3) +
    geom_point(data=mp_math[(mp_math$math_i>=mean(mp_math$math_i)&mp_math$math_j<mean(mp_math$math_j))&mp_math$is_inf==TRUE,], aes(x=math_i, y=math_j), shape=9, alpha=.8, colour=rgb(255, 0, 126, max=255, 255/1)) +
    xlab("Math Score Test Taker (math_i)") + ylab("Math Score Test Taker's Neighbors (math_j)")#+
ggplotly(mphu)#%>%htmlwidgets::prependContent(html_fix)

# Mapping talent
z<-read.csv("https://raw.githubusercontent.com/democratizing-data-science/talent_identification/main/ACS_FBI_IRS_ZCTA_county_bank.csv")
z$minority <- paste(round(z$BlackNH + z$NativeNH + z$PopHisp, 3), "%", sep="")
zip<-zctas() #all PHUDCFILY
m<-get_zctas_by_state("Georgia")
zip$keep <- zip$ZCTA5CE20%in%m
zip<-zip[zip$keep==TRUE,]
zip$pov <- paste(round(z$pro_belowPov[match(zip$ZCTA5CE20, z$zipcode)], 3)*100, "%", sep="")
zip$pove <- round(z$pro_belowPov[match(zip$ZCTA5CE20, z$zipcode)], 3)
zip$unemployment <- paste(z$PctUnemployed[match(zip$ZCTA5CE20, z$zipcode)], "%", sep="")
zip$minority <- z$minority[match(zip$ZCTA5CE20, z$zipcode)]

qpals <- colorQuantile("Greys", zip$pove, n = 9, na.color = "#ff2200")
labelst <- 
  paste0(
    "<b>Poverty level: </b>",
    zip$pov, "<br/>",
    "<b>Minority level: </b>",
    zip$minority, "<br/>",
	"<b>Unemployment: </b>",
    zip$unemployment) %>%
  lapply(htmltools::HTML)

dotsf <- mp_math[(mp_math$math_i>=mean(mp_math$math_i)&mp_math$math_j<mean(mp_math$math_j))&mp_math$is_inf==TRUE,]
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

label_i <- paste0(
    "<b>Math Score: </b>",
    dotsf$math_i, "<br/>",
"<b>Neighbors' Math Score: </b>",
    dotsf$math_j, "<br/>",
"<b>Poverty: </b>",
    dotsf$poverty, "<br/>",
"<b>Minority: </b>",
    dotsf$minority, "<br/>",
	"<b>Single Mom Composition: </b>",
    dotsf$single_mom, "<br/>",
	"<b>Unemployment: </b>",
    dotsf$unemployed) %>%
  lapply(htmltools::HTML)
qpal <- colorQuantile("inferno", dotsf$math_i, n = 10)

mymap<-zip %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("CartoDB") %>% #CartoDB.DarkMatter dark PHUDCFILY
  # add zip codes
addPolygons(fillColor = ~qpals(pove),
weight = 1,
				opacity = .25,
				color = "white",
				dashArray = "3",
				fillOpacity = 0.7, 
				label = labelst,
				highlight = highlightOptions(weight = 2,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = FALSE))%>%
			addControl("<b>Outstanding High Achievers in Math</b><br>Display Contextualized information by clicking on a given dot.<br>Message box shows Own Math and Neighbors' Math Score, <br>Poverty, Unemployment, Minority, and Single Mom Households", 
			position = "topright" ) %>%
addCircleMarkers(data = dotsf,
                    lng = ~longitude, lat = ~latitude, radius = ~log(math_i/50), color = rgb(255, 0, 126, max=255, 255/1), label = label_i,
  weight = 2.5, opacity = 1, fill = TRUE, 
                    labelOptions = labelOptions(interactive = TRUE, direction = 'top', textOnly = F))%>%
  addLegend(pal = qpals, values = ~round(zip$pove,2), opacity = 1, title = "Pct. Living in Poverty", na.label = "No Data on Poverty")%>%
#  addLegend(pal = qpal, values = ~round(dotsf$math_i,2), opacity = 1, title = "Predicted Net Price")%>%
      setView(lng = mean(dotsf$longitude),
              lat = mean(dotsf$latitude),
              zoom = 6) %>%
htmlwidgets::prependContent(html_fix)   
mymap
