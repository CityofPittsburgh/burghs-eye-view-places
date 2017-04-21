# Burgh's Eye View Places
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)
library(xml2)

#"Dogfooding" Packages
library(httr)
library(jsonlite)
library(readr)
library(curl)

# Visuals Libraries
library(leaflet)
library(DT)
library(maptools)
library(htmltools)
library(htmlwidgets)
library(rgeos)
library(geojsonio)

# Data Transform
library(plyr)
library(zoo)
library(lubridate)
library(tools)

# Turn off Scientific Notation
options(scipen = 999)

ckan_api <- jsonlite::fromJSON("key.json")$ckan_api

# Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

getWidth <- '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

# Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}

# Function to download WPRDC Data
ckan <- function(id) {
  x <- paste0("https://data.wprdc.org/datastore/dump/", id)
  r <- GET(x, add_headers(Authorization = ckan_api))
  content(r)
}

# City Boundary
city.boundary <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/a99f25fffb7b41c8a4adf9ea676a3a0b_0.geojson", what = "sp")

# Load facilities
load.facilities_images <- ckan("07bf416f-9df2-4d70-b48d-682f608f9a6b")
attr(load.facilities_images, "spec") <- NULL
# Remove Inactive Facilities
load.facilities <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/b9c387d024944503ad27549e196d7260_0.geojson", what = "sp")
load.facilities@data <- merge(load.facilities@data, load.facilities_images, all.x = TRUE, by = "name", sort = FALSE)
# Create Adress Column (checks to see if Address No. is valid, to add number and add space between street name)
load.facilities@data$address <- paste0(ifelse(is.na(load.facilities@data$address_number), "", paste0(load.facilities@data$address_number, " ")), ifelse(is.na(load.facilities@data$street), "", load.facilities@data$street))
# Clean NA's in Facility Type
load.facilities@data$facility_type <- ifelse(load.facilities@data$name == "Southsnamee Park Third Base Dugout" | load.facilities@data$name == "Josh Gibson  1 Third Base Dugout", "DUGOUT",load.facilities@data$facility_type)
load.facilities@data$facility_type <- ifelse(load.facilities@data$name =="Herschel Upper  Building", "DUGOUT", load.facilities@data$facility_type)
load.facilities@data$facility_type <- ifelse(load.facilities@data$name =="Martin Luther King Field Field House" | load.facilities@data$name =="Frick Park Entrance Gate", "ACTIVITY", load.facilities@data$facility_type)
load.facilities@data$facility_type <- as.factor(load.facilities@data$facility_type)
# Clean Facility Type for humans
load.facilities@data <- transform(load.facilities@data, usage = as.factor(mapvalues(facility_type, c("ACTIVITY", "CABIN", "COMMUNITY", "CONCESSION", "DUGOUT", "FIREHOUSE" , "MEDIC STATION", "OFFICE", "POLICE", "POOL", "POOL CLOSED", "POOL/REC", "REC", "RECYCLING", "RESTROOMS", "SALT DOME", "SENIOR", "SERVICE", "SHELTER", "STORAGE", "TRAINING", "UTILITY", "VACANT", NA),
                                                                                    c("Activity", "Cabin", "Community", "Concession", "Dugout", "Firehouse", "Medic Station", "Office", "Police", "Pool", "Pool - Closed", "Pool/Recreation", "Recreation", "Recycling", "Restrooms", "Salt Dome", "Senior Center", "Service", "Shelter", "Storage", "Training", "Utility", "Vacant", "Storage"))))
# Create Tooltip
load.facilities@data$rentable <- as.factor(load.facilities@data$rentable)
load.facilities@data$url <- ifelse(load.facilities@data$rentable == "True", '<br><center><a href="https://registerparks.pittsburghpa.gov/" target="_blank">Rent this facility</a></center>', "")

load.facilities <- load.facilities[load.facilities$inactive == "False",]

load.recfacilities <- load.facilities[load.facilities@data$usage %in%  c("Recreation", "Pool/Recreation"),]
load.pools <- load.facilities[load.facilities@data$usage %in%  c("Pool", "Pool - Closed"),]

load.facilities <- load.facilities[!load.facilities@data$usage %in%  c("Recreation", "Pool/Recreation", "Pool", "Pool - Closed"),]
load.facilities@data$usage <- as.character(load.facilities@data$usage)
load.facilities@data$usage <- as.factor(load.facilities@data$usage)

# Load Recreation
# Load Greenways
load.greenways <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dcef4d943c1b44129b967d97def9e8c4_0.geojson", what = "sp")
load.greenways@data$layer <- "Greenway"
load.greenways@data <- subset(load.greenways@data, select = c(name, layer))

# Load Environmental 
# Load Flood Zones
floodzones <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/fb45ed8f9e0d463aadfab95620ffa303_0.geojson", what = "sp")
floodzones$name <- NA
floodzones$layer <- "Flood Zone"
floodzones@data <- subset(floodzones@data, select = c(name, layer))

# Load Landslide Prone
landslide <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/c5b8bed5963746d4844dcfea7c2053e7_0.geojson", what = "sp")
landslide$name <- NA
landslide$layer <- "Landslide Prone"
landslide@data <- subset(landslide@data, select = c(name, layer))

# Load Undermined Areas
undermined <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/a065e686e9144110ac6ccfe7bb43fd98_0.geojson", what = "sp")
undermined$layer <- "Undermined Area"
undermined$name <- NA
undermined@data <- subset(undermined@data, select = c(name, layer))

# Merge & Clean Environmental 
load.environmental <- rbind(floodzones, landslide, undermined, makeUniqueIDs = TRUE)  
load.environmental@data$layer <- as.factor(load.environmental@data$layer)

# Load Economic
# Load URA Main St
mainst <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/ab9b192ab4ba46d88144bacf5a0252e0_0.geojson", what = "sp", disambiguateFIDs= TRUE)
mainst$name <- as.character(mainst$name)
mainst$name <- ifelse(mainst$name == "", NA, mainst$name)
mainst$layer <- "URA Main St"
mainst@data <- subset(mainst@data, select = c(name, layer))

# Load Historic Districts
histdist <-  geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/71df883dcf184292b69d5721df39b5dd_0.geojson", what = "sp")
histdist$layer <- "Historic District"
histdist$name <- histdist$NAME
histdist@data <- subset(histdist@data, select = c(name, layer))

# Merge & Clean Economic
load.economic <- rbind(mainst, histdist, makeUniqueIDs = TRUE)  
load.economic@data$layer <- as.factor(load.economic@data$layer)

# Load Water Features
load.wf <- ckan("1b74a658-0465-456a-929e-ff4057220274")
# Remove Inactive Water Features
load.wf <- subset(load.wf, inactive == "False")
# Prepare for Merge to Facilities
load.wf <- transform(load.wf, feature_type = as.factor(mapvalues(feature_type, c("Spray", "Decorative", "Drinking Fountain"), c("Spray Park", "Decorative Water Fountain", "Drinking Fountain"))))

# Load Signalized Intersections
load.si <- ckan("c864e31e-e2f4-4a1e-946c-50006537e73d")
load.si$description <- gsub("_", " ", load.si$description)
load.si$description <- toTitleCase(tolower(load.si$description))
load.si$description <- gsub("Osm", "OSM", load.si$description, ignore.case = TRUE)
load.si$flash_yellow <- ifelse(is.na(load.si$flash_yellow), NA, toTitleCase(tolower(load.si$flash_yellow)))
load.si$operation_type <- as.factor(load.si$operation_type)
load.si$flash_time <- as.factor(load.si$flash_time)

# Load City Steps
load.steps <- geojson_read("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/742fd30a7df24dc4af257f721cbb72ba_0.geojson", what = "sp")
load.steps@data$num_steps_1 <- as.character(load.steps@data$num_steps_1)
load.steps@data$num_steps_1 <- as.numeric(load.steps@data$num_steps_1)

# this_year
this_year <- format(Sys.Date(), format="%Y")

if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
  # Egg
  X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
  Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
  title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "halloween"
  load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-08")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
  load.egg <- ckan("e17e6a67-2bba-4a1a-aa36-87beb2cd0a3b")
  load.egg <- subset(load.egg, MuniName == "PITTSBURGH")
  load.egg$icon <- "election"
  load.egg$tt <- paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                        <br><b>Location: </b>", load.egg$LocName,
                        "<br><b>Ward: </b>", load.egg$Ward,
                        "<br><b>District: </b>", load.egg$District,
                        "<br><b>Address: </b>", load.egg$NewAddress,
                        '<br><center><a href="https://www.pavoterservices.state.pa.us/pages/pollingplaceinfo.aspx" target="_blank">Find your polling place!</a></center>
                        Clear the search bar to go back to the regular Burgh&#39;s Eye View!</font>'
  )
} else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-09"))) {
  X <- c(-79.9773187, -80.0096757, -80.0109521)
  Y <- c(40.4644031, 40.4406418, 40.4416163)
  title <- c("Herr's Island", "Fort Pitt", "Fort Duquesne")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "thanksgiving"
  load.egg$tt <- "*Gobble gobble* <br> No Results this time. Search again and have a Happy Thanksgiving!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-12-30")) | Sys.Date() <= as.Date(paste0(this_year,"-1-02"))) {
  X <- c(-80.00383, -80.003981)
  Y <- c(40.441558, 40.442340)
  title <- c("Liberty & Stanwix", "Penn & Stanwix")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "new_year"
  load.egg$tt <- "3... 2... 1... Happy New Years! <br>Looks like a fresh start to the New Year, and a fresh blank map! Try something else in the search bar!"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-02-01")) & Sys.Date() <= as.Date(paste0(this_year,"-02-15"))) {
  X <-  c(-80.002398,  -80.017794, -79.964644, -79.964708, -79.983140, -79.991428)
  Y <- c(40.440397, 40.437650, 40.428210, 40.461866, 40.452217, 40.456897)
  title <- c("Market Square", "Mt. Washington", "SouthSide Works", " Church Brew Works", "The Strip", "Penn Brewery")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "valentine"
  load.egg$tt <- "Love is in the air, but doesn't look like any results are! <br>Would you be my Valentine?"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-03-01")) & Sys.Date() <= as.Date(paste0(this_year,"-03-31"))){
  X <- c(-79.9968604, -80.004055)
  Y <- c(40.4381098, 40.440631)
  title <- c("City County Building", "Market Square")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "patrick"
  load.egg$tt <- "<i>Your search didn't turn up anything, not even my Pot-o-Gold!</i>"
} else if (Sys.Date() >= as.Date(paste0(this_year,"-04-01")) & Sys.Date() <= as.Date(paste0(this_year,"-04-30"))) {
  load.egg <- read.csv("parks.csv")
  load.egg$icon <- "easter_egg"
  load.egg$tt <- "<i>You couldn't find any results, but maybe you can find my eggs.</i>"
} else {
  X <- c(-79.9968604, -80.004055)
  Y <- c(40.4381098, 40.440631)
  title <- c("City County Building", "Market Square")
  load.egg <- data.frame(X,Y,title)
  load.egg$icon <- "snow"
  load.egg$tt <- "Burrr!! The app's not frozen, there's just nothing that fits that description here!"
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 9, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 9, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 9, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 40, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 45, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5)
)

# UI for application
ui <- navbarPage(id = "navTab",
                 windowTitle = "Burgh's Eye View", 
                 selected = "Places",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme("flatly"),
                 title = HTML('<img src="burghs_eyeview_logo_small.png" alt="Burghs Eye View" height="85%">'),
                 position = "static-top",
                 tabPanel(a("Points", href="https://pittsburghpa.shinyapps.io/BurghsEyeView/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel('Places', id = "Places", value = "Places", 
                          # Run script to determine if user is loading from a mobile device
                          tags$script(getWidth),
                          # Google Tag Manager Script to Head
                          tags$head(includeScript("tag-manager-head.js")),
                          # Set favicon
                          tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
                          tags$head(HTML('<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-120x120-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152-precomposed.png" />')),
                          tags$head(HTML('<!-- You can use Open Graph tags to customize link previews.
                                         Learn more: https://developers.facebook.com/docs/sharing/webmasters -->
                                         <meta property="og:url"           content="http://www.your-domain.com/your-page.html" />
                                         <meta property="og:type"          content="website" />
                                         <meta property="og:title"         content="Burgh&#39;s Eye View" />
                                         <meta property="og:description"   content="Pittsburgh&#39;s one stop shop for geographic City Data" />
                                         <meta property="og:image"         content="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png" />')),
                          # Add Google Analytics Script to page
                          tags$head(includeScript("google-analytics.js")),
                          # Add Tag Manager Script to Body
                          tags$body(tags$noscript(tags$iframe(src='https://www.googletagmanager.com/ns.html?id=GTM-TCTCQVD', height = 0, width = 0, style="display:none;visibility:hidden"))),
                          # Hide error codes that may appear
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"),
                          # Background of report.table
                          tags$style(type="text/css", '.report.table {background-color: #fff;}'),
                          # Remove unwanted padding and margins
                          tags$style(type="text/css", ".container-fluid {padding:0;}"),
                          tags$style(type="text/css", ".navbar-header {margin:auto;"),
                          tags$style(type="text/css", ".navbar-static-top {margin-bottom:0;}"),
                          tags$style(type="text/css", ".navbar-brand {height:60px; padding:0;}"),
                          tags$style(type="text/css", ".navbar {border-right-width: 20px;
                                     border-left-width: 65px;}"),
                          # Set max height for pop-ups
                          tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
                          # Edit top bar
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          uiOutput("placesPanel")
                 ),
                 
                 tabPanel(a("Parcels", href="https://pittsburghpa.shinyapps.io/parcel_viewer/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel('Data', id = "Data", value = "Data",
                          # Select Dataset for Export
                          inputPanel(
                            selectInput("report_select", 
                                        tagList(shiny::icon("map-marker"), "Select Layer:"),
                                        choices = c("City Facilities", "City Steps", "Water Features", "Traffic Signals", "Environmental"), # Parks & Playgrounds, Bridges
                                        selected= "City Facilities"),
                            # Define Button Position
                            uiOutput("buttonStyle")
                          ),
                          # Clean up the Data Table CSS
                          tags$style(type = "text/css", ".dataTables_length {margin-left: 10px;}"),
                          tags$style(type = "text/css", ".dataTables_info {margin-left: 10px;}"),
                          tags$style(type = "text/css", ".dataTables_filter {margin-right: 5px;}"),
                          dataTableOutput("report.table")
                 ),
                 
                 tabPanel('About', class = "About", value = "About",
                          includeHTML('about.html'),
                          # Twitter Button
                          tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                                           header.append('<div class =\"twit\" style=\"float:right;margin-top: 15px;\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" align=\"middle\" data-url=\"data.pittsburghpa.gov/BurghsEyeView\" data-text=\"Check out Burgh&#39;s Eye View! A new tool to view city data in Pittsburgh: https://goo.gl/z4cZ30\" data-size=\"large\">Tweet</a></div>');
                                           console.log(header)")),
                          tags$script(HTML("!function(d,s,id){
                                           var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                           if(!d.getElementById(id)){
                                           js=d.createElement(s);
                                           js.id=id;
                                           js.src=p+'://platform.twitter.com/widgets.js';
                                           fjs.parentNode.insertBefore(js,fjs);
                                           }
                                           }(document, 'script', 'twitter-wjs');")),
                # Facebook Button
                HTML('<div id="fb-root"></div>'),
                tags$script(HTML("(function(d, s, id) {
                                 var js, fjs = d.getElementsByTagName(s)[0];
                                 if (d.getElementById(id)) return;
                                 js = d.createElement(s); js.id = id;
                                 js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
                                 fjs.parentNode.insertBefore(js, fjs);
                                 }(document, 'script', 'facebook-jssdk'));")),
                tags$script(HTML('header.append(\'<div class="fb-share-button" style="float:right;margin-top: 15px;margin-right: 5px;" data-href="http://pittsburghpa.shinyapps.io/BurghsEyeView/?utm_source=facebook_button&amp;utm_campaign=facebook_button&amp;utm_medium=facebook%2Fsocial\" data-layout="button" data-size="large" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank" href="https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fpittsburghpa.shinyapps.io%2FBurghsEyeView%2F%23utm_source%3Dfacebook_button%26utm_campaign%3Dfacebook_button%26utm_medium%3Dfacebook%252Fsocial&amp;src=sdkpreparse">Share</a></div>\');
                                 console.log(header)'))
                )
  )

# Define server
server <- shinyServer(function(input, output, session) {
  setBookmarkExclude("GetScreenWidth")
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update page URL
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  output$buttonStyle <- renderUI({
    # Generate search & layer panel & Map (checks for mobile devices)
    if (as.numeric(input$GetScreenWidth) > 800) {
      div(style="margin-top: 20px", downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    } else {
      div(downloadButton("downloadData", paste("Export" , input$report_select), class = "dlBut"))
    }
  })
  # City Map UI
  output$placesPanel <- renderUI({
    # UI for Desktop Users
    if (as.numeric(input$GetScreenWidth) > 800) {
      tagList(
        # Generate Map
        leafletOutput("map"),
        # Map size for Desktop CSS
        tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
        # Add background image
        tags$head(tags$style(type="text/css", '#Places {
                             background-image: url("loading.png");
                             background-repeat: no-repeat;
                             background-position: center;
                             background-size: contain;
                             }')),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '300px',
          wellPanel(id = "tPanel", style = "overflow-y:auto; max-height: calc(100vh - 85px) !important;",
                    textInput("search",
                              value = "",
                              label = NULL, 
                              placeholder = "Search"),
                    HTML('<font color="#ff7f00">'),
                    checkboxInput("toggleFacilities",
                                  label = "City Facilities",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("usage_select",
                                label = NULL,
                                c(`Facility Usage`='', levels(load.facilities$usage)),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("rentable_select",
                                label= NULL,
                                c(`Rentable` = '', levels(load.facilities$rentable)),
                                selectize = TRUE),
                    HTML('<font color="#f781bf">'),
                    checkboxInput("toggleSteps",
                                  label = "City Steps",
                                  value = TRUE),
                    HTML('</font>'),
                    sliderInput("steps_select",
                                label = "Steps",
                                min = min(load.steps$num_steps_1, na.rm = TRUE),
                                max = max(load.steps$num_steps_1, na.rm = TRUE),
                                value = c(min(load.steps$num_steps_1, na.rm = TRUE),max(load.steps$num_steps_1, na.rm = TRUE)),
                                step = 1),
                    HTML('<font color="#377eb8">'),
                    checkboxInput("toggleWf",
                                  label = "Water Features",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("feature_select",
                                label = NULL,
                                c(`Feature Type`='', levels(load.wf$feature_type)),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#e41a1c">'),
                    checkboxInput("toggleTraffic",
                                  label = "Traffic Lights",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("operation_select",
                                label = NULL,
                                c(`Operation Type` ='', levels(load.si$operation_type)),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("flash_select",
                                label = NULL,
                                c(`Flash Time` ='', levels(load.si$flash_time)),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#984ea3">'),
                    checkboxInput("toggleEconomic",
                                  label = "Economic",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("district_select",
                                label = NULL,
                                c(`Distric Type` ='', levels(load.economic$layer)),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#a65628">'),
                    checkboxInput("toggleEnvironmental",
                                  label = "Environmental",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("environmental_select",
                                label = NULL,
                                c(`Region Type` ='', levels(load.environmental$layer)),
                                multiple = TRUE,
                                selectize = TRUE)
          ), style = "opacity: 0.88"
        )
      )
    } else {
      tagList(
        # Input panel for Mobile (stationary at top)
        absolutePanel(top = 65, left = 0, width = '100%' ,
                      wellPanel(id = "tPanel", style ="padding-left: 5px; padding-right: 5px;",
                                # Remove padding from Search Bar
                                tags$style(type= "text/css", "#tPanel {margin-bottom:0px; padding:0px; overflow-y:scroll; max-height: calc(100vh - 60px); !important; min-height: 55px;}"),
                                # Set background color to match panels
                                tags$style(type = "text/css", "body {background-color: #ecf0f1}"),
                                tags$style(type= "text/css", "{width:100%;
                                           margin-bottom:5px;
                                           text-align: center;}
                                           .inner
                                           {display: inline-block;}"),
                                # Div for Search Bar and Expansion
                                HTML('<div id="outer" style="position:absolute;z-index: 9; background-color:#ecf0f1; width:100%;">'),
                                # Set Searchvar width optimal for device
                                tags$style(type = "text/css", paste0('#search {width: ', input$GetScreenWidth - 84, 'px; margin-left:10px;}')),
                                # Inputs
                                div(style="display:inline-block;", 
                                    textInput("search", 
                                              value =  "",
                                              label = NULL, 
                                              placeholder = "Search")),
                                tags$style(style="text/css", chartr0('#placePanel button .fa:before { content: "\\f056";  }
                                                                     #placePanel button.collapsed .fa:before { content: "\\f055";  }')),
                                HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                                     <div id="mobile" class="collapse" style="margin-top:55px;">'),
                                HTML('<font color="#ff7f00">'),
                                checkboxInput("toggleFacilities",
                                              label = "City Facilities",
                                              value = TRUE),
                                HTML('</font>'),
                                selectInput("usage_select",
                                            label = NULL,
                                            c(`Facility Usage`='', levels(load.facilities$usage)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                selectInput("rentable_select",
                                            label= NULL,
                                            c(`Rentable` = '', levels(load.facilities$rentable)),
                                            selectize = TRUE),
                                HTML('<font color="#f781bf">'),
                                checkboxInput("toggleSteps",
                                              label = "City Steps",
                                              value = TRUE),
                                HTML('</font>'),
                                sliderInput("steps_select",
                                            label = "Steps",
                                            min = min(load.steps$num_steps_1, na.rm = TRUE),
                                            max = max(load.steps$num_steps_1, na.rm = TRUE),
                                            value = c(min(load.steps$num_steps_1, na.rm = TRUE),max(load.steps$num_steps_1, na.rm = TRUE)),
                                            step = 1),
                                HTML('<font color="#377eb8">'),
                                checkboxInput("toggleWf",
                                              label = "Water Features",
                                              value = TRUE),
                                HTML('</font>'),
                                selectInput("feature_select",
                                            label = NULL,
                                            c(`Feature Type`='', levels(load.wf$feature_type)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                HTML('<font color="#e41a1c">'),
                                checkboxInput("toggleTraffic",
                                              label = "Traffic Lights",
                                              value = TRUE),
                                HTML('</font>'),
                                selectInput("operation_select",
                                            label = NULL,
                                            c(`Operation Type` ='', levels(load.si$operation_type)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                selectInput("flash_select",
                                            label = NULL,
                                            c(`Flash Time` ='', levels(load.si$flash_time)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                HTML('<font color="#984ea3">'),
                                checkboxInput("toggleEconomic",
                                              label = "Economic",
                                              value = FALSE),
                                HTML('</font>'),
                                selectInput("district_select",
                                            label = NULL,
                                            c(`Distric Type` ='', levels(load.economic$layer)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                HTML('<font color="#984ea3">'),
                                checkboxInput("toggleEnvironmental",
                                              label = "Environmental",
                                              value = FALSE),
                                HTML('</font>'),
                                selectInput("environmental_select",
                                            label = NULL,
                                            c(`Region Type` ='', levels(load.environmental$layer)),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                HTML('</div>')
                      ),
                      # Generate Map
                      div(class="assetsBack", style="position: absolute;
                          width: 100%;z-index: -1;
                          left: 0px;
                          top: 55px;", leafletOutput("map")),
                      # Set map to style for Mobile
                      tags$style(type = "text/css", ".assetsBack {height: calc(100vh - 115px) !important;}"),
                      tags$head(tags$style(type="text/css", '#assetsBack {
                                           background-image: url("loading.png");
                                           background-repeat: no-repeat;
                                           background-position: center;
                                           background-size: contain;}'))
                      )
                    )
    }
  })
  # Water Features Data with filters
  wfInput <- reactive({
    wf <- load.wf
    
    # Feature Filter
    if (length(input$feature_select) > 0) {
      wf <- wf[wf$usage %in% input$feature_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      wf <- wf[apply(wf, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(wf)
  })
  # Signalized Intersections Data with filters
  siInput <- reactive({
    si <- load.si
    
    # Operation Type Filter
    if (length(input$operation_select) > 0) {
      si <- si[si$operation_type %in% input$operation_select,]
    }
    # Flash Time Filter
    if (length(input$flash_select) > 0) {
      si <- si[si$flash_time %in% input$flash_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      si <- si[apply(si, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(si)
  })
  # City Steps data with filters
  stepsInput <- reactive({
    steps <- load.steps
    
    steps <- subset(steps, num_steps_1 <= input$steps_select[1] & num_steps_1 >= input$steps_select[2] | is.na(num_steps_1))
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      steps <- steps[apply(steps@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(steps)
  })
  recfacilitiesInput <- reactive({
    recfacilities <- load.recfacilities
  })
  greenwaysInput <- reactive({
    greenways <- load.greenways
    
    # Usage Filter
    if (length(input$recreation_select) > 0) {
      greenways <- greenways[greenways@data$layer %in% input$recreation_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      greenways <- greenways[apply(greenways@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(greenways)
  })
  economicInput <- reactive({
    economic <- load.economic
    
    # Usage Filter
    if (length(input$environmental_select) > 0) {
      economic <- economic[economic@data$layer %in% input$district_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      economic <- economic[apply(economic@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(economic)
  })
  environmentalInput <- reactive({
    environmental <- load.environmental
    
    # Usage Filter
    if (length(input$environmental_select) > 0) {
      environmental <- environmental[environmental@data$layer %in% input$environmental_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      environmental <- environmental[apply(environmental@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(environmental)
  })
  # City Facilities data with filters
  facilitiesInput <- reactive({
    facilities <- load.facilities
    
    # Usage Filter
    if (length(input$usage_select) > 0) {
      facilities <- facilities[facilities$usage %in% input$usage_select,]
    }
    # Usage Filter
    if (input$rentable_select != "") {
      facilities <- facilities[facilities$rentable %in% input$rentable_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      facilities <- facilities[apply(facilities@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(facilities)
  })
  # Generate table for Data page and export
  # Note all reports do same data process comments only exist for 311
  reportInput <- reactive({
    if (input$report_select == "City Facilities") {
      facilities <- facilitiesInput()
      facilities <- facilities@data
      
      facilities <- subset(facilities, select = c(usage, name, primary_user, address, neighborhood, council_district, public_works_division, police_zone))
      
      colnames(facilities) <- c("Usage", "Description", "Dept", "Location", "Neighborhood", "Council", "Public Works Division", "Police Zone")
      
      report <- facilities
    } 
    return(report)
  })
  downloadInput <- reactive({
    report <- reportInput()
    # Report Table Search Filter
    if (!is.null(input$report.table_search) && input$report.table_search != "") {
      report <- report[apply(report, 1, function(row){any(grepl(input$report.table_search, row, ignore.case = TRUE))}), ]
    }
    
    return(report)
  })
  # Generate Report Table
  output$report.table <- DT::renderDataTable({
    # Load Report dataset
    reportInput()
  }, escape = FALSE, options = list(scrollX = TRUE), rownames= FALSE)
  # Execute download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$report_select, ".csv", sep="") },
    content = function(file) {
      write.csv(downloadInput(), file)
    }
  )
  # Build City Map
  output$map <- renderLeaflet({
    assetsCount <- 0
      map <- leaflet() %>% 
        addProviderTiles("Thunderforest.Pioneer",
                         options = providerTileOptions(noWrap = TRUE), group = "Pioneer") %>%
        addProviderTiles("OpenStreetMap.HOT",
                         options = providerTileOptions(noWrap = TRUE), group = "Huamitarian (OSM)") %>%
        addTiles(options = providerTileOptions(noWrap = TRUE), group = "Mapnik (OSM)") %>%
        addProviderTiles("OpenStreetMap.France",
                         options = providerTileOptions(noWrap = TRUE), group = "France (OSM)") %>%
        addLayersControl(
          baseGroups = c("Pioneer", "Mapnik (OSM)", "Huamitarian (OSM)", "France (OSM)")) %>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
        addPolygons(data = city.boundary, stroke = TRUE, smoothFactor = 0, weight = 2, color = "#000000", opacity = 0.6,
                    fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0)
    } else {
      map <- leaflet() %>% 
        addProviderTiles("OpenStreetMap.HOT",
                         options = providerTileOptions(noWrap = TRUE), group = "Huamitarian (OSM)") %>%
        addTiles(options = providerTileOptions(noWrap = TRUE), group = "Mapnik (OSM)") %>%
        addProviderTiles("OpenStreetMap.France",
                         options = providerTileOptions(noWrap = TRUE), group = "France (OSM)") %>%
        addLayersControl(
          baseGroups = c("Mapnik (OSM)", "Huamitarian (OSM)", "France (OSM)")) %>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="Locate Me",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
        addPolygons(data = city.boundary, stroke = TRUE, smoothFactor = 0, weight = 2, color = "#000000", opacity = 0.6,
                    fill = TRUE, fillColor = "#00FFFFFF", fillOpacity = 0)
    }
    # City Places Layer
    if (input$toggleFacilities) {
      facilities <- facilitiesInput()
      if (nrow(facilities) > 0) {
        assetsCount <- assetsCount + 1
        map <- addPolygons(map, data=facilities, color = "#ff7f00", fillColor = "#ff7f00", fillOpacity = .5,
                           popup = ~(paste(paste0('<center><img id="imgPicture" src="', facilities$image_url,'" style="width:250px;"></center>'),
                                          "<font color='black'><b>Name:</b>", facilities$name,
                                          "<br><b>Location:</b>", facilities$address,
                                          "<br><b>Usage:</b>", facilities$usage,
                                          "<br><b>Dept:</b>", facilities$primary_user,
                                          facilities$url, "</font>"))
        )
      }
    }
    if (input$toggleWf) {
      wf <- wfInput()
      if (nrow(wf) > 0) {
        assetsCount <- assetsCount + 1
        map <- addCircleMarkers(map, data=wf, color = "#377eb8", fillColor = "#377eb8", fillOpacity = .5, lat = ~latitude, lng = ~longitude, radius = 2,
                                popup = ~(paste("<font color='black'><b>Location:</b>", wf$name,
                                                "<br><b>Feature Type:</b>", wf$feature_type,
                                                ifelse(is.na(wf$make), "", paste("<br><b>Make:</b>", wf$make)),
                                                ifelse(is.na(wf$control_type), "", paste("<br><b>Control:</b>", wf$control_type)),"</font>"))
        )
      }
    }
    if (input$toggleTraffic) {
      si <- siInput()
      if (nrow(si) > 0) {
        assetsCount <- assetsCount + 1
        map <- addCircleMarkers(map, data=si, color = "#e41a1c", fillColor = "#e41a1c", fillOpacity = .5, lat = ~latitude, lng = ~longitude, radius = 2,
                                     popup = ~(paste("<font color='black'><b>Location:</b>", si$description,
                                                     ifelse(is.na(si$operation_type), "", paste("<br><b>Operation Type:</b>", si$operation_type)),
                                                     ifelse(is.na(si$flash_time), "", paste("<br><b>Flash Time:</b>", si$flash_time)),
                                                     ifelse(is.na(si$flash_yellow), "", paste("<br><b>Flash Yellow:</b>", si$flash_yellow)),"</font>"))
        )
      }
    }
    if (input$toggleSteps) {
      assetsCount <- assetsCount + 1
      steps <- stepsInput()
      if (nrow(steps) > 0) {
        map <- addPolylines(map, data=steps, color = "#f781bf",
                                popup = ~(paste("<font color='black'><b>Location:</b>", steps$cartegraph_id,
                                                ifelse(is.na(steps$num_steps_1) | steps$num_steps_1 == 0, "<br><b>Steps:</b> Uncounted", paste("<br><b>Steps:</b>", steps$num_steps_1)),
                                                ifelse(is.na(steps$year_1) | steps$year_1 == 0, "<br><b>Year:</b> Unknown", paste("<br><b>Year:</b>", steps$year_1)),
                                                '<br><center><a href="http://pittsburghpa.gov/dcp/steps" target="_blank">Volunteer to Survey City Steps!</a></center></font>'))
        )
      }
    }
    if (input$toggleEconomic) {
      economic <- economicInput()
      if (nrow(economic) > 0) {
        assetsCount <- assetsCount + 1
        map <- addPolygons(map, data=economic, color = "#984ea3", fillColor = "#984ea3", fillOpacity = .5,
                           popup = ~(paste("<font color='black'><b>Region:</b>", economic$layer,
                                           ifelse(is.na(economic$name), "", paste("<br><b>Name:</b>", economic$name)),
                                           '</font>'))
        )
      }
    }
    if (input$toggleEnvironmental) {
      environmental <- environmentalInput()
      if (nrow(environmental) > 0) {
        assetsCount <- assetsCount + 1
        map <- addPolygons(map, data=environmental, color = "#a65628", fillColor = "#984ea3", fillOpacity = .5,
                                 popup = ~(paste("<font color='black'><b>Region:</b>", environmental$layer,
                                                 ifelse(is.na(environmental$name), "", paste("<br><b>Name:</b>", environmental$name)),
                                                 '</font>'))
        )
      }
    }
    if (assetsCount < 1) {
      if (Sys.Date() >= as.Date(paste0(this_year,"-11-01")) & Sys.Date() <= as.Date(paste0(this_year,"-11-08"))) {
        egg <- load.egg
      } else {
        egg <- load.egg[sample(1:nrow(load.egg),1),]
      }
      map <- addMarkers(map, data=egg, ~X, ~Y, icon = ~icons_egg[icon], popup = ~tt) %>% 
        setView(-79.9959, 40.4406, zoom = 10)
    }
    map
  })
})

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")