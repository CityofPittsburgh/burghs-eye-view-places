# Burgh's Eye View Places
# Organization: City of Pittsburgh
# Dept: Innovation & Performance
# Team: Analytics & Strategy
# Author: Geoffrey Arnold

# Load required packages
library(shiny)
library(shinythemes)

#"Dogfooding" Packages
library(httr)
library(jsonlite)
library(R4CouchDB)

# Visuals Libraries
library(leaflet)
library(DT)
library(sp)
library(rgdal)

# Data Transform
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(tools)
library(stringi)
library(tidyr)

# Turn off Scientific Notation
options(scipen = 999)

#Keys
ckan_api <- jsonlite::fromJSON("key.json")$ckan_api
couchdb_un <- jsonlite::fromJSON("key.json")$couchdb_un
couchdb_pw <- jsonlite::fromJSON("key.json")$couchdb_pw
couchdb_url <- jsonlite::fromJSON("key.json")$couchdb_url

# Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

# Function to Check Screenwidth
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
  url <- paste0("http://wprdc.ogopendata.com/datastore/dump/", id)
  r <- RETRY("GET", url)
  content(r)
}

# Query Using SQL
ckanSQL <- function(url) {
  r <- RETRY("GET", url) 
  c <- content(r, "text")
  json <- gsub('NaN', '""', c, perl = TRUE)
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("http://wprdc.ogopendata.com/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  ckanSQL(url)
}

# Get ID's
getIds <- function(phrase) {
  url <- paste0("http://data.wprdc.org/api/action/package_search?q=", gsub(" ", "%20", phrase))
  r <- GET(url)
  raw <- content(r, "text")
  df <- jsonlite::fromJSON(raw)$result$results
  tib <- tibble(df$resources) %>%
    unnest() %>%
    filter(format == "CSV")
  final <- df %>%
    select(id, name) %>%
    right_join(tib, by = c("id" = "package_id"))
  
  return(final)
}

# Facility Usage
facility_usage <- toTitleCase(tolower(ckanUniques("fbb50b02-2879-47cd-abea-ae697ec05170", "type")$type))
facility_usage <- sort(facility_usage[!is.na(facility_usage)])

# Load Recreation Types
court_types <- ckanUniques("a5b71bfa-840c-4c86-8f43-07a9ae854227", "type")$type

field_usages <- ckanUniques("6af89346-b971-41d5-af09-49cfdb4dfe23", "field_usage")$field_usage

# Park Types
parks <- RETRY("GET", "https://services1.arcgis.com/YZCmUqbcsUpOKfj7/arcgis/rest/services/ParksOpenData/FeatureServer/0/query?where=final_cat+is+not+null&geometryType=esriGeometryEnvelope&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=final_cat&returnGeometry=false&returnCentroid=false&multipatchOption=xyFootprint&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnDistinctValues=false&returnZ=false&returnM=false&returnExceededLimitFeatures=false&sqlFormat=standard&f=pjson")
parks_c <- content(parks, "text")
park_df <- jsonlite::fromJSON(parks_c)

park_types <- unique(park_df$features$attributes$final_cat)

rec_types <- sort(c(court_types, field_usages, park_types, "Greenway", "Playground"))

# Environmental Select
enviornmental_layers <- c("Flood Zone", "Landslide Prone", "Undermined Area")

# Pools Select
outdoor <- ckanUniques("5cc254fe-2cbd-4912-9f44-2f95f0beea9a", "type")$type

pool_cat <- levels(as.factor(c(outdoor, "Spray Fountain", "Pool", "Pool - Closed", "Drinking Fountain", "Decorative Water Fountain")))

# Intersections Selections
mark_type <- ckanUniques("f2f0c299-4f7b-4689-be3c-a2ad38252cf4", "type")$type

si_type <- ckanUniques("79ddcc74-33d2-4735-9b95-4169c7d0413d", "operation_type")$operation_type
si_type <- paste("Traffic Signal -", si_type)
si_type <- ifelse(si_type == "Traffic Signal - NA", "Traffic Signal - Other", si_type)

intersection_type <- levels(as.factor(c(mark_type, si_type)))

# Street Sign Types
sign_types <- sort(ckanUniques("d078a6b5-83a3-4723-a3a9-5371cfe1cc0c", "description")$description)

flash_times <- levels(as.factor(c(ckanUniques("79ddcc74-33d2-4735-9b95-4169c7d0413d", "flash_time")$flash_time)))

# Feet Select
max_lngth <- max(as.numeric(ckanSQL("http://wprdc.ogopendata.com/api/3/action/datastore_search_sql?sql=SELECT%20MAX(%22length%22)%20from%20%2243f40ca4-2211-4a12-8b4f-4d052662bb64%22")), as.numeric(ckanSQL("http://wprdc.ogopendata.com/api/3/action/datastore_search_sql?sql=SELECT%20MIN(%22length%22)%20from%20%223e337bde-9997-46fa-b027-481b8f54eb9b%22")))
ft_select <- c(0, max_lngth)

# Waste Material Types
materials <- as.factor(c("Alkaline Batteries", "Automotive Batteries", "Cell Phones", "CFL Lightbulbs", "Clothing", "Collectibles", "Computers and Peripherals", "Construction and Demolition Waste", "Fluorescent Tube Lightbulbs", "Freon Appliances", "General Electronics", "Household Chemicals and Waste", "Household Recyclables", "Ink and Toner", "Motor Oil", "Plastic Bags and Films", "Prescription Medication", "Propane Tanks", "Rechargeable Batteries", "Scrap Metal", "Small Business Recyclables", "Tires", "TVs and Monitors", "Yard Debris"))

# CouchDB Connection
# couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-places")
couchDB <- cdbIni(serverName = couchdb_url, uname = couchdb_un, pwd = couchdb_pw, DBName = "burghs-eye-view-places-dev")

# this_year
this_year <- format(Sys.Date(), format="%Y")

# Presidential Years
presidential_years <- seq(2016, 3000, 4)

# Election Day
nov <- ymd(as.Date(paste0(this_year, "-11-01")))
dow <- sapply(seq(0,7),function(x) format(nov+x, "%a"))
eDay <- nov + which(dow=="Mon")[1]

# Primary Day
if (this_year %in% presidential_years) {
  april <- ymd(as.Date(paste0(this_year, "-04-01")))
  dow <- sapply(seq(0,7),function(x) format(april+x, "%a"))
  firstTuesday <- april + which(dow=="Tue")[1]
  # In Presidential Years PA Primaries are on the 4th Tuesday of April
  pDay <- firstTuesday + 20
} else {
  may <- ymd(as.Date(paste0(this_year, "-05-01")))
  dow <- sapply(seq(0,7),function(x) format(may+x, "%a"))
  firstTuesday <- may + which(dow=="Tue")[1]
  # In Non-Presidential Years PA Primaries are on the 3rd Tuesay of May
  pDay <- firstTuesday + 13
}

icons_egg <- iconList(
  halloween = makeIcon("./icons/egg/pirate.png", iconAnchorX = 31, iconAnchorY = 12.5, popupAnchorX = 0, popupAnchorY = -12.5, iconWidth = 72),
  election = makeIcon("./icons/egg/vote.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  thanksgiving = makeIcon("./icons/egg/thanksgiving.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  snow = makeIcon("./icons/egg/snowboard.png", iconAnchorX = 31, iconAnchorY = 13, popupAnchorX = 0, popupAnchorY = -13, iconWidth = 72),
  new_year = makeIcon("./icons/egg/new_year.png", iconAnchorX = 31, iconAnchorY = 13.5, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  valentine = makeIcon("./icons/egg/valentine.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  patrick = makeIcon("./icons/egg/patrick.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  easter_egg = makeIcon("./icons/egg/easter.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  summer = makeIcon("./icons/egg/summer.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72),
  july_4 = makeIcon("./icons/egg/july_4.png", iconAnchorX = 31, iconAnchorY = 32, popupAnchorX = 0, popupAnchorY = -13.5, iconWidth = 72)      
)

# UI for application
ui <- function(request) {
      navbarPage(id = "navTab",
                 windowTitle = "Burgh's Eye View Places", 
                 selected = "Places",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme("flatly"),
                 title = HTML('<img src="burghs_eyeview_logo_small.png" alt="Burghs Eye View" height="85%">'),
                 position = "static-top",
                 tabPanel(a("Points", href="https://pittsburghpa.shinyapps.io/BurghsEyeView/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel('Places', id = "Places", value = "Places", class = "Places",
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
                          # Layout CSS
                          # Notification Centered and Color Fix
                          tags$head(tags$style(type = "text/css", 
                                               '.shiny-notification {
                                                    position: fixed;
                                                    background: #2c3e50;
                                                    top: calc(50%);;
                                                    left: calc(50%);;
                                                    width: calc(25%);;
                                                    min-width: 200px;
                                                    transform: translate(-50%, 0);}
                                               .shiny-notification-close { color: white; }
                                               .loading:after {
                                                                overflow: hidden;
                                                                 display: inline-block;
                                                                 vertical-align: bottom;
                                                                 -webkit-animation: ellipsis steps(4,end) 900ms infinite;      
                                                                 animation: ellipsis steps(4,end) 900ms infinite;
                                                                 content: "...";
                                                                 width: 0px;
                                                              }
                                              @keyframes ellipsis {
                                                  to { width: 1.25em; }
                                                }
                                               @-webkit-keyframes ellipsis {
                                                  to { width: 1.25em; }
                                              }')),
                          tags$style(type="text/css", ".shiny-output-error { visibility: hidden;}
                                                       .shiny-output-error:before { visibility: hidden; }
                                                       .container-fluid { padding:0; }
                                                       .navbar-header {margin:auto;}
                                                       .navbar-static-top {margin-bottom:0;}
                                                       .navbar-brand {height:60px; 
                                                                      padding:0;}
                                                       .navbar {border-right-width: 20px;
                                                                border-left-width: 65px;}
                                                       .leaflet-popup-content {overflow-y: auto; 
                                                                               max-height: 400px !important;}
                                                       .form-group {margin-bottom: 0px;}
                                                       @media only screen and (min-width: 600px) {
                                                         #map {height: calc(100vh - 55px) !important; 
                                                               z-index: 0;}
                                                         #tPanel {opacity: 0.88;
                                                                  max-height: calc(100vh - 90px);}
                                                         .btn.collapsed {display: none;}
                                                         #mobile {display: initial;}
                                                         #outer {position: relative; padding-bottom: 0px;}
                                                         #search {width: 275px;}
                                                       }
                                                       @media only screen and (max-width: 600px) {
                                                         #map {height: calc(100vh - 115px) !important;
                                                               position: absolute !important;
                                                               top: 60px;
                                                               z-index: 0;}
                                                         .mapBack {height: calc(100vh);}
                                                         #aPanel {top: 60px !important; 
                                                                  left: 0px !important; 
                                                                  width: 100% !important;}
                                                                  .assetsBack {position: absolute;
                                                                  width: 100%;
                                                                  z-index: -1;
                                                                  left: 0px;
                                                                  top: 55px;}
                                                         #tPanel {margin-bottom:0px; 
                                                                  padding:0px !important; 
                                                                  overflow-y:scroll !important; 
                                                                  max-height: calc(100vh - 65) !important; 
                                                                  min-height: 55px !important; 
                                                                  padding-left: 10px !important; 
                                                                  padding-right: 10px !important;
                                                                  border: none;
                                                                  width: 100%;
                                                                  opacity: 1 !important;}
                                                         #search {width: calc(100vw - 85px) !important; margin-left:10px !important;}
                                                         #outer {margin-top: 5px !important; position: absolute;}
                                                         .btn.collapsed {display: in-line !important;}
                                                       }"),
                          # Generate Map
                          div(class="mapBack", style='position: absolute;
                                                      background-image: url("loading.png");
                                                      background-repeat: no-repeat;
                                                      background-position: center;
                                                      background-size: contain;
                                                      width: 100%;
                                                      z-index: -1;
                                                      left: 0px;
                                                      top: 55px', 
                              leafletOutput("map")),
        absolutePanel(
          # Input panel for Desktops (alpha'd)
          top = 70, left = 50, width = '325px', style = "z-index: 1000", id = "aPanel",
          wellPanel(id = "tPanel", style = "overflow-y:auto; min-height: 65px;",
                    HTML('<div id="outer" style="z-index: 9; background-color:#ecf0f1;">'),
                    div(style="display:inline-block;", 
                        textInput("search", 
                                  value = "",
                                  label = NULL, 
                                  placeholder = "Search")),
                    tags$style(style="text/css", chartr0('#tPanel #outer .btn .fa:before { content: "\\f056";  }
                                                         #tPanel #outer .btn.collapsed .fa:before { content: "\\f055";  }')),
                    HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile" stye="display: block;"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                         <div id="mobile" class="collapse" style="margin-top:55px;">'),
                    HTML('<font color="#ff7f00">'),
                    checkboxInput("toggleFacilities",
                                  label = "City Facilities",
                                  value = TRUE),
                    HTML('</font>'),
                    selectInput("usage_select",
                                label = NULL,
                                c(`Facility Usage`='', facility_usage),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("rentable_select",
                                label= NULL,
                                c(`Rentable` = '', c("Yes", "No")),
                                selectize = TRUE),
                    HTML('<font color="#999999">'),
                    checkboxInput("toggleStreets",
                                  label = "Paving Schedule",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("year_select",
                                label = NULL,
                                c(`Paving Year` ='', as.character(c(2009:this_year))),
                                selected = this_year,
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#D4AF37">'),
                    checkboxInput("toggleBridges",
                                  label = "City Bridges",
                                  value = FALSE),
                    HTML('</font>'),
                    HTML('<font color="#4daf4a">'),
                    checkboxInput("toggleRecreation",
                                  label= "Recreation",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("recreation_select",
                                label= NULL,
                                c(`Recreation Type` = '', rec_types),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#b10dc9">'),
                    checkboxInput("toggleLibs",
                                  label = "Carnegie Library of Pittsburgh Locations",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("open_select",
                                label = NULL,
                                c(`Open On` = '', c("Open Now", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))),
                    HTML('<font color="#f781bf">'),
                    checkboxInput("toggleSteps",
                                  label = "City Steps",
                                  value = FALSE),
                    HTML('</font>'),
                    HTML('<font color="#43a1a1">'),
                    checkboxInput("toggleWalls",
                                  label = "City Retaining Walls",
                                  value = FALSE),
                    HTML('</font>'),
                    sliderInput("ft_select",
                                label = "Step/Wall length (ft)",
                                min = ft_select[1],
                                max = ft_select[2],
                                value = ft_select,
                                step = 1),
                    HTML('<font color="#377eb8">'),
                    checkboxInput("togglePools",
                                  label = "Pools, Spray Parks & Water Feautures",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("water_select",
                                label = NULL,
                                c(`Water Category`='', pool_cat),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#099fff">'),
                    checkboxInput("toggleWaste",
                                  label = "Waste Recovery Sites",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("materials_select",
                                label = NULL,
                                c(`Accepted Materials` ='', levels(materials)),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#85144b">'),
                    checkboxInput("toggleDesignations",
                                  label = "Designations"),
                    HTML('</font>'),
                    HTML('<font color="#a65628">'),
                    selectInput("designation_select",
                                label = NULL,
                                c(`Designations` = '', c("Historic Districts", "Residential Parking Areas")),
                                multiple = TRUE,
                                selectize = TRUE),
                    checkboxInput("toggleEnvironmental",
                                  label = "Environmental",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("environmental_select",
                                label = NULL,
                                c(`Region Type` ='', enviornmental_layers),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#000000">'),
                    checkboxInput("toggleSigns",
                                  label = "Street Signs",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("sign_select",
                                label = NULL,
                                c(`Sign Type` ='', sign_types),
                                multiple = TRUE,
                                selectize = TRUE),
                    HTML('<font color="#e41a1c">'),
                    checkboxInput("toggleIntersections",
                                  label = "Intersections",
                                  value = FALSE),
                    HTML('</font>'),
                    selectInput("intersection_select",
                                label = NULL,
                                c(`Intersection Type` ='', intersection_type),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("flash_select",
                                label = NULL,
                                c(`Traffic Signal Flash Time` ='', flash_times),
                                multiple = TRUE,
                                selectize = TRUE),
                    selectInput("basemap_select",
                                label = "Basemap",
                                choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Code for Pittsburgh` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap"),
                                selected = "OpenStreetMap.Mapnik"), 
          HTML("</div>")
          )
        )
                 ),
                 tabPanel(a("Parcels", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewParcels/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
                 tabPanel('Data: Places', id = "Data: Places", value = "Data: Places",
                          # Select Dataset for Export
                          inputPanel(
                            selectInput("report_select", 
                                        tagList(shiny::icon("map-marker"), "Select Layer:"),
                                        choices = c("Carnegie Library of Pittsburgh Locations", "City Facilities", "City Bridges", "City Steps", "City Parks", "City Retaining Walls", "Courts & Rinks", "Pavement Markings", "Paving Schedule","Playgrounds", "Playing Fields", "Pools & Spray Parks", "Recreation Facilities", "Traffic Signals", "Street Signs", "Waste Recovery Sites", "Water Features"), #
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
}

# Define server
server <- shinyServer(function(input, output, session) {
  setBookmarkExclude(c("GetScreenWidth", "report.table_rows_all", "report.table_rows_current"))
  sessionStart <- as.numeric(Sys.time())
  names(sessionStart) <- "sessionStart"
  sessionID <- paste(stri_rand_strings(1, 5), gsub("\\.", "-", sessionStart) , "places", sep="-")
  names(sessionID) <- "sessionID"
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
  # Load All Water Features
  wfLoad <- reactive({
    # Load Water Features
    wf <- ckan("513290a6-2bac-4e41-8029-354cbda6a7b7")
    # Remove Inactive Water Features
    wf <- wf[wf$inactive == "f",]
    # Prepare for Merge to Facilities
    wf <- transform(wf, feature_type = as.factor(mapvalues(feature_type, c("Spray", "Decorative"), c("Spray Fountain", "Decorative Water Fountain"))))
    wf$feature_type <- as.character(wf$feature_type)
    wf$rentable <- "No"
    
    return(wf)
  })
  # Water Features Data with filters
  wfInput <- reactive({
    wf <- wfLoad()
    
    # Feature Filter
    if (length(input$water_select) > 0) {
      wf <- wf[wf$feature_type %in% input$water_select,]
    }
    
    # Rentable Filter
    if (input$rentable_select != "") {
      wf <- wf[wf$rentable %in% input$rentable_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      wf <- wf[apply(wf, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(wf)
  })
  # Load Signalized Intersections
  datSiLoad <- reactive({
    # Load Signalized Intersections
    si <- readOGR("http://wprdc.ogopendata.com/dataset/f470a3d5-f5cb-4209-93a6-c974f7d5a0a4/resource/82ce557f-2388-489f-87e0-0d9d052633c4/download/siimg.geojson")
    # Clean
    si$description <- gsub("_", " ", si$description)
    si$description <- toTitleCase(tolower(si$description))
    si$description <- gsub("Osm", "OSM", si$description, ignore.case = TRUE)
    si$flash_yellow <- ifelse(is.na(si$flash_yellow), NA, toTitleCase(tolower(si$flash_yellow)))
    si$operation_type <- as.character(si$operation_type)
    si$operation_type <- paste("Traffic Signal -", si$operation_type)
    si$operation_type <- ifelse(si$operation_type == "Traffic Signal - ", "Traffic Signal - Other", si$operation_type)
    si$operation_type <- as.factor(si$operation_type)
    si$flash_time <- as.factor(si$flash_time)
    
    return(si)
  })
  # Signalized Intersections Data with filters
  siInput <- reactive({
    si <- datSiLoad()
    
    # Operation Type Filter
    if (length(input$intersection_select) > 0) {
      si <- si[si$operation_type %in% input$intersection_select,]
    }
    # Flash Time Filter
    if (length(input$flash_select) > 0) {
      si <- si[si$flash_time %in% input$flash_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      si <- si[apply(si@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(si)
  })
  # Stop
  signsLoad <- reactive({
    signs <- ckan("d078a6b5-83a3-4723-a3a9-5371cfe1cc0c")
    
    return(signs)
    })  
  signsInput <- reactive({
    signs <- signsLoad()
    
    # Operation Type Filter
    if (length(input$sign_select) > 0) {
      signs <- signs[signs$description %in% input$sign_select,]
    }

    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      signs <- signs[apply(signs, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(signs)
  })
  # Load Markings
  loadCwLoad <- reactive({
    # Load Markings
    mark <- readOGR("http://wprdc.ogopendata.com/dataset/31ce085b-87b9-4ffd-adbb-0a9f5b3cf3df/resource/f86f1950-3b73-46f9-8bd4-2991ea99d7c4/download/markingsimg.geojson")
    return(mark)
  })
  # Markings Data with Filters
  markInput <- reactive({
    mark <- loadCwLoad()
    
    # Operation Type Filter
    if (length(input$intersection_select) > 0) {
      mark <- mark[mark$type %in% input$intersection_select,]
    }

    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      mark <- mark[apply(mark@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(mark)
  })
  # Load CLP
  datLibsLoad <- reactive({
    libs <- ckan("14babf3f-4932-4828-8b49-3c9a03bae6d0")
    libs$full_address <- paste(libs$Address, paste0(libs$City, ","), libs$State, libs$Zip4)
    # Clean Name Start
    libs$Name <- tolower(libs$Name)
    # Format Open/Close TImes
    libs$MoOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$MoOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$MoClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$MoClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$TuOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$TuOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$TuClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$TuClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$WeOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$WeOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$WeClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$WeClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$ThOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$ThOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$ThClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$ThClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$FrOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$FrOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$FrClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$FrClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$SaOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$SaOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$SaClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$SaClose)), format = "%Y-%m-%d %H:%M:%S")
    libs$SuOpen <- as.POSIXct(paste(Sys.Date(), as.character(libs$SuOpen)), format = "%Y-%m-%d %H:%M:%S")
    libs$SuClose <- as.POSIXct(paste(Sys.Date(), as.character(libs$SuClose)), format = "%Y-%m-%d %H:%M:%S")
    
    # Format Open/Close Tooltips
    libs$MoOpen_tt <-format(libs$MoOpen, "%I:%M %p")
    libs$MoClose_tt <- format(libs$MoClose, "%I:%M %p")
    libs$TuOpen_tt <-format(libs$TuOpen, "%I:%M %p")
    libs$TuClose_tt <- format(libs$TuClose, "%I:%M %p")
    libs$WeOpen_tt <-format(libs$WeOpen, "%I:%M %p")
    libs$WeClose_tt <- format(libs$WeClose, "%I:%M %p")
    libs$ThOpen_tt <-format(libs$ThOpen, "%I:%M %p")
    libs$ThClose_tt <- format(libs$ThClose, "%I:%M %p")
    libs$FrOpen_tt <-format(libs$FrOpen, "%I:%M %p")
    libs$FrClose_tt <- format(libs$FrClose, "%I:%M %p")
    libs$SaOpen_tt <- format(libs$SaOpen, "%I:%M %p")
    libs$SaClose_tt <- format(libs$SaClose, "%I:%M %p")
    libs$SuOpen_tt <- format(libs$SuOpen, "%I:%M %p")
    libs$SuClose_tt <- format(libs$SuClose, "%I:%M %p")
    
    # Build URL Hyperlink
    libs$url_name <- gsub("\\(", "", libs$Name) 
    libs$url_name <- gsub("\\)", "", libs$url_name)
    libs$url_name <- gsub("\\&", "and", libs$url_name)
    libs$url_name <- gsub(" library", "", libs$url_name, ignore.case = T)
    libs$url_name <- gsub(" ", "-", libs$url_name)
    libs$url_name <- gsub("downtown-and-business", "downtown-business", libs$url_name)
    libs$url_name <- paste0("https://www.carnegielibrary.org/clp_location/", libs$url_name, "/")
    #Clean Name Finish
    libs$Name <- toTitleCase(libs$Name)
    libs$Name <- gsub(" Library", "", libs$Name)
    libs$Name <- paste("CLP -", libs$Name)
    
    return(libs)
  })
  # Carnegie Library of Pittsburgh Locations
  libsInput <- reactive({
    libs <- datLibsLoad()

    if (input$open_select == "Open Now") {
      if (format(Sys.Date(), "%A") == "Monday") {
        libs <- subset(libs, Sys.time() > MoOpen & Sys.time() < MoClose)
      } else if (format(Sys.Date(), "%A") == "Tuesday") {
        libs <- subset(libs, Sys.time() > TuOpen & Sys.time() < TuClose)
      } else if (format(Sys.Date(), "%A") == "Wednesday") {
        libs <- subset(libs, Sys.time() > WeOpen & Sys.time() < WeClose)
      } else if (format(Sys.Date(), "%A") == "Thursday") {
        libs <- subset(libs, Sys.time() > ThOpen & Sys.time() < ThClose)
      } else if (format(Sys.Date(), "%A") == "Friday") {
        libs <- subset(libs, Sys.time() > FrOpen & Sys.time() < FrClose)
      } else if (format(Sys.Date(), "%A") == "Saturday") {
        libs <- subset(libs, Sys.time() > SaOpen & Sys.time() < SaClose)
      } else if  (format(Sys.Date(), "%A") == "Sunday") {
        libs <- subset(libs, Sys.time() > SuOpen & Sys.time() < SuClose)
      } 
    } else if (input$open_select == "Monday") {
      libs <- subset(libs, !is.na(MoClose))
    } else if (input$open_select == "Tuesday") {
      libs <- subset(libs, !is.na(TuClose))
    } else if (input$open_select == "Wednesday") {
      libs <- subset(libs, !is.na(WeClose))
    } else if (input$open_select == "Thursday") {
      libs <- subset(libs, !is.na(ThClose))
    } else if (input$open_select == "Friday") {
      libs <- subset(libs, !is.na(FrClose))
    } else if (input$open_select == "Saturday") {
      libs <- subset(libs, !is.na(SaClose))
    } else if  (input$open_select == "Sunday") {
      libs <- subset(libs, !is.na(SuClose))
    } 
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      libs <- libs[apply(libs, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(libs)
  })
  datStepsLoad <- reactive({
    # Load City Steps
    steps <- readOGR("http://wprdc.ogopendata.com/dataset/e9aa627c-cb22-4ba4-9961-56d9620a46af/resource/ff6dcffa-49ba-4431-954e-044ed519a4d7/download/stepsimg.geojson")
    steps$installed<-  as.numeric(format(as.Date(steps$installed), "%Y"))
    
    return(steps)
  })
  # City Steps data with filters
  stepsInput <- reactive({
    steps <- datStepsLoad()
    
    # Step Filter
    steps <- subset(steps, length >= input$ft_select[1] & length <= input$ft_select[2] | is.na(length))

    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      steps <- steps[apply(steps@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(steps)
  })
  datWallsLoad <- reactive({
    # Load Retaining Walls
    walls <- readOGR("http://wprdc.ogopendata.com/dataset/5e77546c-f1e1-432a-b556-9ccf29db9b2c/resource/b126d855-d283-4875-aa29-3180099090ec/download/retainingwallsimg.geojson")
    walls$image <- as.character(walls$image)
    
    return(walls)
  })
  wallsInput <- reactive({
    walls <- datWallsLoad()
    
    # Length Filter
    walls <- subset(walls, length >= input$ft_select[1] & length <= input$ft_select[2] | is.na(length))
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      walls <- walls[apply(walls@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(walls)
  })
  # Load Paving
  datStreetsLoad <- reactive({
    # Load Paving Schedule
    streets <- readOGR("http://wprdc.ogopendata.com/dataset/6d872b14-c9bb-4627-a475-de6a72050cb0/resource/c390f317-ee05-4d56-8450-6d00a1b02e39/download/pavingscheduleimg.geojson")
    
    return(streets)
  })
  # Street Paving with Filters
  streetsInput <- reactive({
    streets <- datStreetsLoad()

    # Year Filter
    if (length(input$year_select) > 0) {
      streets <- streets[streets@data$start_year %in% input$year_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      streets <- streets[apply(streets@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }

    return(streets)
  }) 
  # Load Waste Data
  datWasteLoad <- reactive({
    # Load Waste Recovery Sites
    waste <- ckan("51f0c4f3-0ddd-4073-8f39-ad19d7528575")
    #Build Address
    waste$address <- paste0(ifelse(is.na(waste$address_number), "", paste0(as.character(as.integer(waste$address_number)), " ")), ifelse(is.na(waste$street), "", as.character(waste$street)), paste0(ifelse(is.na(waste$city) | waste$city == "", "", paste0(" ", as.character(waste$city), ", PA"))))
    # Build URL Link
    waste$link <- ifelse(waste$website == "", as.character(waste$name), paste0('<a href=', waste$website,' target="_blank">', waste$name, '</a>'))
    
    waste$managed_by_city <- ifelse(waste$managed_by_city == 1, TRUE, FALSE)
    # Build Description
    waste$description <- ""
    for (i in levels(materials)) {
      col <- gsub(" ", "_", paste("accepts", tolower(i)))
      waste$description <- case_when(
        waste[,col] == 1 & waste$description == "" ~ i,
        waste[,col] == 1 & waste$description != "" ~ paste(waste$description, i, sep = ", "),
        TRUE ~ waste$description
      )
    }
    
    return(waste)
  })
  # Waste Recovery Sites with Filters
  wasteInput <- reactive({
    waste <- datWasteLoad()
  
    # Materials Select Filter for multiple
    if (length(input$materials_select) > 1) {
      # Put in Column Name
      cols <- gsub(" ", "_", paste("accepts", tolower(input$materials_select)))
      count <- 1
      for (i in cols) {
        temp <- waste[c(waste[i] == 1),]
        #Create DF
        if (count == 1) {
          count <- 2
          spdf <- temp
        } else {
          #Bind DF
          spdf <- rbind(spdf, temp)
        }
      }
      # Keep Unique Rows
      waste <- spdf[rownames(unique(spdf@data)),]
    # Materials Select Filter for single
    } else if (length(input$materials_select) == 1) {
      waste <- waste[c(waste[gsub(" ", "_", paste("accepts", tolower(input$materials_select)))] == 1),]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      waste <- waste[apply(waste, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(waste)
  })
  # Load Bridges
  datBridgesLoad <- reactive({
    # Load Bridges
    bridges <- readOGR("http://wprdc.ogopendata.com/dataset/d6e6c012-45f0-4e13-ab3b-9458fd56ad96/resource/c972b2cc-8396-4cd0-80d6-3051497da903/download/bridgesimg.geojson")
    
    return(bridges)
  })
  # Bridges data with Filters
  bridgesInput <- reactive({
    bridges <- datBridgesLoad()
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      bridges <- bridges[apply(bridges@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(bridges)
  })
  # Load Pools
  poolsLoad <- reactive({
    pools <- readOGR("http://wprdc.ogopendata.com/dataset/f7067c4e-0c1e-420c-8c31-f62769fcd29a/resource/77288f26-54a1-4c0c-bc59-7873b1109e76/download/poolsimg.geojson")
    
    return(pools)
  })
  # Pools Data with Filters
  poolsInput <- reactive({
    pools <- poolsLoad()
    
    # Usage Filter
    if (length(input$water_select) > 0) {
      pools <- pools[pools@data$type %in% input$water_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      pools <- pools[apply(pools@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(pools)
  })
  # Load Parks
  datLoadParks <- reactive({
    parks <- readOGR("https://opendata.arcgis.com/datasets/c39ca624271a4fe0afe7087a9ea805f9_0.geojson")

    parks@data <- mutate(parks@data,
                         image = paste0("https://tools.wprdc.org/images/pittsburgh/parks/", gsub(" ", "_", parks$updatepknm), ".jpg"),
                         exist = sapply(image, function(x) url.exists(as.character(x))),
                         image = ifelse(exist, image, NA))
    
    return(parks)
  })
  # Greenways Filters
  parksInput <- reactive({
    parks <- datLoadParks()
    
    # Usage Filter
    if (length(input$recreation_select) > 0) {
      parks <- parks[parks@data$final_cat %in% input$recreation_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      parks <- parks[apply(parks@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(parks)
  })
  # Load Greenways
  datGreenwaysLoad <- reactive({
    greenways <- readOGR("https://opendata.arcgis.com/datasets/d7a239c35b964485a24126bc9e0e4306_0.geojson")
    greenways@data$layer <- "Greenway"
    greenways@data$name <- toTitleCase(tolower(greenways@data$name))
    greenways@data <- subset(greenways@data, select = c(name, layer))
    
    return(greenways)
  })
  # Greenways Filter
  greenwaysInput <- reactive({
    greenways <- datGreenwaysLoad()
    
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
  # Courts Load
  datCourtsLoad <- reactive({
    # Load Court & Rinks
    courts <- readOGR("http://wprdc.ogopendata.com/dataset/8da92664-22a4-42b8-adae-1950048d70aa/resource/96d327a8-fb12-4174-a30d-7ec9a9920237/download/courtsimg.geojson")
    courts$grandstand <- ifelse(courts$grandstand == 1, TRUE, FALSE)
    
    return(courts)
  })
  # Courts Filters
  courtsInput <- reactive({
    courts <- datCourtsLoad()
    
    # Usage Filter
    if (length(input$recreation_select) > 0) {
      courts <- courts[courts@data$type %in% input$recreation_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      courts <- courts[apply(courts@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(courts)
  })
  # Load Fields
  datFieldsLoad <- reactive({
    # Load Playing Fields
    fields <- readOGR("https://data.wprdc.org/dataset/87c77ec3-db98-4b2a-8891-d9b577b4c44d/resource/d569b513-44c0-4b65-9241-cc3d5c506760/download/fields_img.geojson")
    fields$has_lights <- ifelse(fields$has_lights == 0, FALSE, TRUE)
    
    return(fields)
  })
  # Fields Filters
  fieldsInput <- reactive({
    fields <- datFieldsLoad()
    
    # Usage Filter
    if (length(input$recreation_select) > 0) {
      fields <- fields[fields$field_usage %in% input$recreation_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      fields <- fields[apply(fields@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(fields)
  })
  # Load Playgrounds Data
  datPlaygroundsLoad <- reactive({
    playgrounds <- readOGR("http://wprdc.ogopendata.com/dataset/37e7a776-c98b-4e08-ad61-a8c8e23ec9ab/resource/12d59d62-e86d-4f37-af19-463050496ed6/download/playgroundsimg.geojson")
    playgrounds@data$layer <- "Playground"
    
    return(playgrounds)
  })
  # Playgrounds Filter
  playgroundsInput <- reactive({
    playgrounds <- datPlaygroundsLoad()
    
    # Usage Filter
    if (length(input$recreation_select) > 0) {
      playgrounds <- playgrounds[playgrounds@data$layer %in% input$recreation_select,]
    }
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      playgrounds <- playgrounds[apply(playgrounds@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(playgrounds)
  })
  # Load Historic Data
  datHistoricLoad <- reactive({
    # Load Historic Districts
    histdist <- readOGR("https://opendata.arcgis.com/datasets/d6373af0b9e349b38a40ea6c99224730_0.geojson")
    histdist@data <- histdist@data %>%
      rename(name = historic_name) %>%
      mutate(layer = "Historic District") %>%
      select(name, layer)
    
    return(histdist)
  })
  historicInput <- reactive({
    histdist <- datHistoricLoad()

    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      histdist <- histdist[apply(histdist@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(histdist)
  })
  datParkingLoad <- reactive({
    ppa <- readOGR("https://pghgis-pittsburghpa.opendata.arcgis.com/datasets/d5bd5650c83e43b898850756218352b6_0.geojson")
    
    return(ppa)
  })
  parkingInput <- reactive({
    ppa <- datParkingLoad()
    
    # Search Filter
    if (!is.null(input$search) & input$search != "") {
      ppa <- ppa[apply(ppa@data, 1, function(row){any(grepl(input$search, row, ignore.case = TRUE))}), ]
    }
    
    return(ppa)
  })
  datEnviornmentalLoad <- reactive({
    # Load Flood Zones
    floodzones <- readOGR("https://opendata.arcgis.com/datasets/4ab4c0b4021547c79a9a6a875c1ae1be_0.geojson")
    floodzones$name <- NA
    floodzones$layer <- "Flood Zone"
    floodzones@data <- subset(floodzones@data, select = c(name, layer))
    
    # Load Landslide Prone
    landslide <- readOGR("https://opendata.arcgis.com/datasets/194cdce70d084b7e893653dece2de0bd_0.geojson")
    landslide$name <- NA
    landslide$layer <- "Landslide Prone"
    landslide@data <- subset(landslide@data, select = c(name, layer))
    
    # Load Undermined Areas
    undermined <- readOGR("https://opendata.arcgis.com/datasets/428f48cd3ba540339ab3d2afc94d65a9_0.geojson")
    undermined$layer <- "Undermined Area"
    undermined$name <- NA
    undermined@data <- subset(undermined@data, select = c(name, layer))
    
    # Merge & Clean Environmental 
    environmental <- rbind(floodzones, landslide, undermined, makeUniqueIDs = TRUE)  
    environmental@data$layer <- as.factor(environmental@data$layer)
    
    return(environmental)
  })
  
  environmentalInput <- reactive({
    environmental <- datEnviornmentalLoad()
    
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
  facilitiesLoad <- reactive({
    # Load facilities
    facilities <- readOGR("http://wprdc.ogopendata.com/dataset/e33e12d9-1268-45ed-ae47-ae3a76dcc0aa/resource/fd532423-b0ec-4028-98ff-5d414c47e01a/download/facilitiesimg.geojson")
    # Create Adress Column (checks to see if Address No. is valid, to add number and add space between street name)
    facilities@data$address <- paste0(ifelse(is.na(facilities@data$address_number), "", paste0(as.character(as.integer(facilities@data$address_number)), " ")), ifelse(is.na(facilities@data$street), "", as.character(facilities@data$street)))
    # Create Tooltip
    facilities@data$usage <- as.factor(toTitleCase(tolower(facilities@data$type)))
    facilities@data$rentable <- as.factor(facilities@data$rentable)
    facilities@data$url <- ifelse(facilities@data$rentable == 1, '<br><center><a href="https://registerparks.pittsburghpa.gov/" target="_blank">Rent this facility</a></center>', "")
    facilities@data$rentable <- ifelse(facilities@data$rentable == 1, "Yes", "No")
    
    return(facilities)
  })
  # City Facilities data with filters
  facilitiesInput <- reactive({
    facilities <- facilitiesLoad ()
    
    # Usage Filter
    if (length(input$usage_select) > 0) {
      facilities <- facilities[facilities$usage %in% input$usage_select,]
    }
    # Rentable Filter
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
  reportInput <- reactive({
    if (input$report_select == "City Facilities") {
      facilities <- facilitiesInput()
      
      facilities <- subset(facilities@data, select = c(usage, name, primary_user, address))
      colnames(facilities) <- c("Usage", "Description", "Dept", "Location")
      
      report <- facilities
    } else if (input$report_select == "Carnegie Library of Pittsburgh Locations") {
      libs <- libsInput()
      
      libs <- subset(libs, select = c(Name, full_address, Phone, MoOpen_tt, MoClose_tt, TuOpen_tt, TuClose_tt, WeOpen_tt, WeClose_tt, ThOpen_tt, ThClose_tt,  FrOpen_tt, FrClose_tt, SaOpen_tt, SaClose_tt, SuOpen_tt, SuClose_tt))
      colnames(libs) <- c("Name", "Address", "Phone", "Mon Open", "Mon Close", "Tue Open", "Tue Close", "Wed Open", "Wed Close", "Thu Open", "Thu Close", "Fri Open", "Fri Close", "Sat Open", "Sat Close", "Sun Open", "Sun Close")
      
      report <- libs
    } else if (input$report_select == "Water Features") {
      wf <- wfInput()
      
      wf <- select(wf, c(feature_type, name, make , control_type))
      colnames(wf) <- c("Feature Type", "Name", "Make", "Control Type")
      
      report <- wf
    } else if (input$report_select == "City Bridges") {
      bridges <- bridgesInput()
      
      bridges <- select(bridges@data, c(name, start_neighborhood, end_neighborhood))
      colnames(bridges) <- c("Name", "Start Neighborhood", "End Neighborhood")
      
      report <- bridges
    } else if (input$report_select == "City Steps") {
      steps <- stepsInput()
      
      steps <- select(steps@data, c(name, installed, material, length, number_of_steps, total_population, transit_rider_count, schools_count, overall_score, transit_score, school_score, detour_score))
      colnames(steps) <- c("Name", "Year Installed", "Material", "Length (ft)", "# of Steps", "Total Pop.", "Transit Rider Count", "School Count", "Overall Score", "Transit Score", "School Score", "Detour Score")
      
      report <- steps
    } else if (input$report_select == "City Parks") {
      parks <- parksInput()
      
      parks <- select(parks@data, c(updatepknm, final_cat, maintenanceresponsibility))
      colnames(parks) <- c("Name", "Type", "Maintenance")
      
      report <- parks
    } else if (input$report_select == "Playgrounds") {
      playgrounds <- playgroundsInput()
      
      playgrounds <- select(playgrounds@data, c(name, street, park))
      colnames(playgrounds) <- c("Name", "Street", "Park")
      
      report <- playgrounds
    } else if (input$report_select == "Courts & Rinks") {
      courts <- courtsInput()
      
      courts <- select(courts@data, c(name, type, surface_material, grandstand, park, location))
      colnames(courts) <- c("Name", "Type", "Surface Material", "Grandstands", "Park", "Location")
      
      report <- courts
    } else if (input$report_select == "Playing Fields") {
      fields <- fieldsInput()
      
      fields <- select(fields@data, c(name, park, field_usage, goal_post, infield_type, has_lights, left_field_distance, center_field_distance, right_field_distance))
      colnames(fields) <- c("Name", "Park", "Field Type", "Goal Posts", "Infield", "Lights", "Left Field", "Right Field", "Center Field")
      
      report <- fields
    } else if (input$report_select == "Pools & Spray Parks") {
      pools <- poolsInput()
      
      pools <- select(pools@data, c(name, type, water_source, capacity))
      colnames(pools) <- c("Name", "Type", "Water Source", "Capacity (gal)")
      
      report <- pools
    } else if (input$report_select == "Traffic Signals") {
      si <- siInput()

      si <- select(si@data, c(description, operation_type, flash_time, flash_yellow))
      colnames(si) <- c("Location", "Operation Type", "Flash Time", "Flash Yellow")
      
      report <- si
    } else if (input$report_select == "Pavement Markings"){
      mark <- markInput()
      
      mark <- select(mark@data, c(type, street))
      colnames(mark) <- c("Type", "Street")
    
      report <- mark
    } else if (input$report_select == "Street Signs") {
      signs <- signsInput()
      
      signs <- select(signs, c(description, mutcd_code, address_number, street, mounting_fixture, date_installed))
      colnames(signs) <- c("Sign Type", "MUTCD Code", "Address No.", "Street", "Mounting Fixture", "Installed")
      
      report <- signs
    } else if (input$report_select == "Paving Schedule") {
      streets <- streetsInput()
      
      streets <- select(streets@data, c(street, route_ahead, route_back, start_year, status))
      colnames(streets) <- c("Name", "Route Ahead", "Route Back", "Start Year", "Status")
      
      report <- streets
    } else if (input$report_select == "Waste Recovery Sites") {
      waste <- wasteInput()
      
      waste <- select(waste, c(link, managed_by_city, address, hours_of_operation, phone_number, description, notes))
      colnames(waste) <- c("Name","City Location" ,"Location", "Hours", "Phone #", "Materials", "Notes")
      
      report <- waste
    } else if (input$report_select == "City Retaining Walls") {
      walls <- wallsInput()
      
      walls <- select(walls@data, c(name, street, year_constructed, to_street, length, height))
      colnames(walls) <- c("Name", "Street", "Year Constructed", "To Street", "Length (ft)", "Height (ft)")
      
      report <- walls
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
  easterEgg <- reactive({
    if (Sys.Date() == eDay | Sys.Date() == pDay | input$search == "Vote!") {
      month <- as.numeric(format(Sys.Date(), "%m")) 
      
      if (month >= 10) {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*november*"
      } else if (month > 3 ) {
        yearQ <- as.character(as.numeric(format(Sys.Date(), "*%Y")) - 1)
        monthQ <- "*november*"
      } else {
        yearQ <- format(Sys.Date(), "*%Y")
        monthQ <- "*may"
      }
      
      ids <- getIds("Allegheny County Polling Place Locations") %>%
        filter(grepl(yearQ, name.x, ignore.case = T) & grepl(monthQ, name.x, ignore.case = T))
      
      id <- ids$id.y[1]
      
      load.egg <- ckanSQL(paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22", id, "%22%20WHERE%22MuniName%22%20=%20%27PITTSBURGH%27")) %>%
        mutate(icon = "election",
               X = as.numeric(X),
               Y = as.numeric(Y),
               tt = paste0("<font color='black'>No matter who you Vote for, make sure you Vote!
                           <br><br><b>Location: </b>", LocName,
                           "<br><b>Ward: </b>", Ward,
                           "<br><b>District: </b>", District,
                           "<br><b>Address: </b>", NewAddress,
                           '<br><center><a href="https://alleghenycounty.civicengine.com/" target="_blank">Find your polling place!</a></center>'
               ))
    } else if(Sys.Date() <= as.Date(paste0(this_year,"-10-31")) & Sys.Date() >= as.Date(paste0(this_year,"-10-01"))) {
      # Egg
      X <- c(-79.9573738, -79.9796721, -79.9892566, -79.9814719, -79.9517155, -79.9128181, -79.9272001, -79.983961, -79.9948964, -79.9933058, -80.0217265, -80.0215099, -79.9851465)
      Y <- c(40.4611634, 40.4671619, 40.4667157, 40.472155, 40.4684005, 40.4401088, 40.4161835, 40.4186422, 40.4066441, 40.4012173, 40.4737751, 40.4636383, 40.4289496)
      title <- c("Allegheny", "Voegtly", "Ridgelawn", "St. Pauls", "St. Mary", "Smithfield East", "Calvary Catholic", "St Michaels", "St John Vianney", "South Side", "Highwood", "Union Dale", "Prince of Peace")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "halloween"
      load.egg$tt <- "Yarr! There be nuttin' to be found with that search term matey."
    } else if (Sys.Date() <= as.Date(paste0(this_year,"-11-30")) & Sys.Date() >= as.Date(paste0(this_year,"-11-01"))) {
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
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-07-01")) & Sys.Date() <= as.Date(paste0(this_year,"-07-07"))) {
      load.egg <- read.csv("boundaries/Parks/parks.csv")
      load.egg$icon <- "july_4"
      load.egg$tt <- "<i>Happy Independence Day! Looks like you need to try another search term.</i>"
    } else if (Sys.Date() >= as.Date(paste0(this_year,"-05-01")) & Sys.Date() <= as.Date(paste0(this_year,"-08-31"))) {
      load.pools <- ckan("5cc254fe-2cbd-4912-9f44-2f95f0beea9a") %>%
        rename(X = latitude,
               Y = longitude) %>%
        mutate(icon = "summer",
               tt = "<i>Ah... Summer! Chill out, relax and grab some rays with me. Or if you'd like try another search term.</i>")
    } else {
      X <- c(-79.9968604, -80.004055)
      Y <- c(40.4381098, 40.440631)
      title <- c("City County Building", "Market Square")
      load.egg <- data.frame(X,Y,title)
      load.egg$icon <- "snow"
      load.egg$tt <- "Burrr!! The app's not frozen, there's just nothing that fits that description here!"
    }
    return(load.egg)
  })
  # Build City Map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(-79.9959, 40.4406, zoom = 12) %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  observe({
    # Code for Pittsburgh Basemap
    if (input$basemap_select == "mapStack") {
      leafletProxy("map", session = session) %>%
        clearTiles() %>%
        addTiles(urlTemplate = "http://{s}.sm.mapstack.stamen.com/((terrain-background,$000[@30],$fff[hsl-saturation@80],$1b334b[hsl-color],mapbox-water[destination-in]),(watercolor,$fff[difference],$000000[hsl-color],mapbox-water[destination-out]),(terrain-background,$000[@40],$000000[hsl-color],mapbox-water[destination-out])[screen@60],(streets-and-labels,$fedd9a[hsl-color])[@50])/{z}/{x}/{y}.png", attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="https://cartodb.com/attributions">CARTO</a>', options = providerTileOptions(noWrap = TRUE))
    } else {
      leafletProxy("map", session = session) %>%
        clearTiles() %>%
        addProviderTiles(input$basemap_select, options = providerTileOptions(noWrap = TRUE))
    }
  })
  # City Places Layer
  observe({
      if (input$toggleFacilities) {
        showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading City Facilities<center></div></font>')), type = "message", id = "cityMessage", duration = NULL, closeButton = FALSE)
        leafletProxy("map", session = session) %>%
          clearGroup("facilities")
        facilities <- facilitiesInput()
        if (nrow(facilities) > 0) {
          leafletProxy("map", session = session) %>%
            addPolygons(data = facilities, color = "#ff7f00", fillColor = "#ff7f00", fillOpacity = .5, group = "facilities",
                        popup = ~(paste(ifelse(facilities$image == "", "", paste0('<center><img id="imgPicture" src="', facilities$image,'" style="width:250px;"></center>')),
                                        "<font color='black'><b>Name:</b>", facilities$name,
                                        "<br><b>Location:</b>", facilities$address,
                                        "<br><b>Usage:</b>", facilities$usage,
                                        "<br><b>Dept:</b>", facilities$primary_user,
                                        facilities$url, "</font>"))
          )
        }
      } else {
        leafletProxy("map", session = session) %>%
          clearGroup("facilities")
      }
    removeNotification("cityMessage")
  })
  # Paving Schedule
  observe({
    if (input$toggleStreets) {
      leafletProxy("map", session = session) %>%
        clearGroup("streets")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Paving Schedule<center></div></font>')), type = "message", id = "streetsMessage", duration = NULL, closeButton = FALSE)
      
      streets <- streetsInput()
      if (nrow(streets) > 0) {
        leafletProxy("map", session = session) %>%
          addPolylines(data = streets, color = "#999999", opacity = 0.75, group = "streets",
                       popup = ~(paste("<font color='black'><b>Street:</b>", streets$street,
                                       "<br><b>Activity:</b>", streets$activity,
                                       ifelse(streets$task_description == "", "", paste("<br><b>Description:</b>", streets$task_description)),
                                       ifelse(is.na(streets$stop_date_actual), "", paste("<br><b>Stop Date:</b>", streets$stop_date_actual)),
                                       "<br><b>Start Year:</b>", streets$start_year,
                                       "<br><b>Route Ahead:</b>", streets$route_ahead,
                                       "<br><b>Route Back:</b>", streets$route_back,
                                       "<br><b>Status:</b>", streets$status,
                                       '<br><center><a href="http://pittsburghpa.gov/domi/street-resurfacing/paving-schedule.html" target="_blank">View the Paving Schedule!</a></center></font>'))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("streets")
    }
    removeNotification("streetsMessage")
  })
  # City Bridges
  observe({
    if (input$toggleBridges) {
      leafletProxy("map", session = session) %>%
        clearGroup("bridges")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading City Bridges<center></div></font>')), type = "message", id = "bridgesMessage", duration = NULL, closeButton = FALSE)
      
      bridges <- bridgesInput()
      if (nrow(bridges) > 0) {
        leafletProxy("map", session = session) %>%
          addPolylines(data = bridges, color = "#D4AF37", opacity = 0.75, group = "bridges",
                       popup = ~(paste(ifelse(bridges$image == "", "", paste0('<center><img id="imgPicture" src="', bridges$image,'" style="width:250px;"></center>')),
                                       "<font color='black'><b>Name:</b>", bridges$name,
                                       ifelse(is.na(bridges$year_built), "", paste("<br><b>Year Built:</b>", bridges$year_built)),
                                       ifelse(is.na(bridges$year_rehab), "", paste("<br><b>Year last rehab:</b>", bridges$year_rehab)),
                                       "<br><b>Start Neighborhood:</b>", bridges$start_neighborhood,
                                       ifelse(is.na(bridges$end_neighborhood), "", paste( "<br><b>End Neighborhood:</b>", bridges$end_neighborhood)), "</font>"))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("bridges")
    }
    removeNotification("bridgesMessage")
  })
  # Recreation Layers
  observe({
    if (input$toggleRecreation) {
      leafletProxy("map", session = session) %>%
        clearGroup("recreation")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Recreational Layers<center></div></font>')), type = "message", id = "recMessage", duration = NULL, closeButton = FALSE)
      
      parks <- parksInput()
      if (nrow(parks) > 0) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = parks, color = "#4daf4a", group = "recreation",
                      popup = ~(paste(ifelse(is.na(parks$image), "", paste0('<center><img id="imgPicture" src="', parks$image,'" style="width:250px;"></center>')),
                                      "<font color='black'><b>Name:</b>", parks$updatepknm,
                                      "<br><b>Type:</b>", parks$final_cat,
                                      ifelse(parks$maintenanceresponsibility == "", "", paste0("<br><b>Maintenance: </b>", parks$maintenanceresponsibility))))
        )
      }
      greenways <- greenwaysInput()
      if (nrow(greenways) > 0) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = greenways, color = "#4daf4a", group = "recreation",
                      popup = ~(paste("<font color='black'><b>Type:</b>", greenways$layer,
                                      ifelse(is.na(greenways$name), "", paste("<br><b>Name:</b>", greenways$name)),
                                      '</font>'))
        )
      }
      # Playgrounds
      playgrounds <- playgroundsInput()
      if (nrow(playgrounds) > 0) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = playgrounds, color = "#4daf4a", fillColor = "#4daf4a", fillOpacity = .5, group = "recreation",
                      popup = ~(paste(ifelse(playgrounds$image == "", "" ,paste0('<center><img id="imgPicture" src="', playgrounds$image, '" style="width:250px;"></center>')),
                                      "<font color='black'><b>Name:</b>", playgrounds$name,
                                      "<br><b>Location:</b>", playgrounds$street,
                                      "<br><b>Park:</b>", playgrounds$park, "</font>"))
        )
      }
      # Court & Rinks
      courts <- courtsInput()
      if(nrow(courts) > 0) {
        print("courts")
        leafletProxy("map", session = session) %>%
          addPolygons(data = courts, color = "#4daf4a", fillColor = "#4daf4a", fillOpacity = .5, group = "recreation",
                      popup = ~(paste("<font color='black'><b>Name:</b>", courts$name,
                                      "<br><b>Location:</b>", courts$park,
                                      "<br><b>Type:</b>", courts$type,
                                      "<br><b>Surface Material:</b>", courts$surface_material,
                                      "<br><b>Grandstands:</b>", courts$grandstand, "</font>"))
        )
      }
      # Playing Fields
      fields <- fieldsInput()
      if (nrow(fields) > 0) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = fields, color = "#4daf4a", fillColor = "#4daf4a", fillOpacity = .5, group = "recreation",
                      popup = ~(paste("<font color='black'><b>Name:</b>", fields$name,
                                      "<br><b>Location:</b>", fields$park,
                                      "<br><b>Type:</b>", fields$field_usage,
                                      ifelse(fields$infield_type == "N/A", "", paste("<br><b>Infield Type:</b>", fields$infield_type)),
                                      "<br><b>Lights:</b>", fields$has_lights,
                                      ifelse(fields$goal_post == 0, "", paste("<br><b>Goal Posts:</b>", fields$goal_post)),
                                      ifelse(fields$left_field_distance == "N/A", "", paste("<br><b>Left Field:</b>", fields$left_field_distance, "ft")),
                                      ifelse(fields$center_field_distance == "N/A", "", paste("<br><b>Center Field:</b>", fields$center_field_distance, "ft")),
                                      ifelse(fields$right_field_distance == "N/A", "", paste("<br><b>Right Field:</b>", fields$right_field_distance, "ft")), "</font>"))
          )
        }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("recreation")
    }
    removeNotification("recMessage")
  })
  # Carnegie Library of Pittsburgh Locations
  observe({
    if (input$toggleLibs) {
      leafletProxy("map", session = session) %>%
        clearGroup("libs")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Carnegie Libraries<center></div></font>')), type = "message", id = "libsMessage", duration = NULL, closeButton = FALSE)
      
      libs <- libsInput()
      if (nrow(libs) > 0) {
        leafletProxy("map", session = session) %>%
          addCircleMarkers(data = libs, color = "#b10dc9", fillColor = "#b10dc9", fillOpacity = .5, radius = 8,  ~Lon, ~Lat, group = "libs",
                           popup = ~(paste("<font color='black'><b>Name:</b> ", '<a href="', libs$url_name,'" target="_blank">', libs$Name, '</a>',
                                           "<br><b>Address:</b> ", libs$full_address,
                                           "<br><b>Phone:</b> ", libs$Phone,
                                           "<br><b>Hours:</b><br><ul>",
                                           ifelse(is.na(libs$MoOpen_tt), "<li><b>Mon:</b> Closed", paste0("<li><b>Mon:</b> ", libs$MoOpen_tt, " - ", libs$MoClose_tt)),
                                           ifelse(is.na(libs$TuOpen_tt), "<li><b>Tue:</b> Closed", paste0("<li><b>Tue:</b> ", libs$TuOpen_tt, " - ", libs$TuClose_tt)),
                                           ifelse(is.na(libs$WeOpen_tt), "<li><b>Wed:</b> Closed", paste0("<li><b>Wed:</b> ", libs$WeOpen_tt, " - ", libs$WeClose_tt)),
                                           ifelse(is.na(libs$ThOpen_tt), "<li><b>Thu:</b> Closed", paste0("<li><b>Thu:</b> ", libs$ThOpen_tt, " - ", libs$ThClose_tt)),
                                           ifelse(is.na(libs$FrOpen_tt), "<li><b>Fri:</b> Closed", paste0("<li><b>Fri:</b> ", libs$FrOpen_tt, " - ", libs$FrClose_tt)),
                                           ifelse(is.na(libs$SaOpen_tt), "<li><b>Sat:</b> Closed", paste0("<li><b>Sat:</b> ", libs$SaOpen_tt,  " - ", libs$SaClose_tt)),
                                           ifelse(is.na(libs$SuClose_tt), "<li><b>Sun:</b> Closed", paste0("<li><b>Sun:</b> ",  libs$SuOpen_tt, " - ", libs$SuClose_tt)), "</ul>"))
          )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("libs")
    }
    removeNotification("libsMessage")
  })
  # City Steps
  observe({
    if (input$toggleSteps) {
      leafletProxy("map", session = session) %>%
        clearGroup("steps")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading City Steps<center></div></font>')), type = "message", id = "stepsMessage", duration = NULL, closeButton = FALSE)
      
      steps <- stepsInput()
      if (nrow(steps) > 0) {
        leafletProxy("map", session = session) %>%
          addPolylines(data = steps, color = "#f781bf", opacity = 0.75, group = "steps",
                       popup = ~(paste(ifelse(steps$image == "", "", paste0('<center><img id="imgPicture" src="', steps$image,'" style="width:250px;"></center>')),
                                       "<font color='black'><b>Location:</b>", steps$name,
                                       ifelse(is.na(steps$length) | steps$length == 0, "", paste("<br><b>Length:</b>", steps$length, "ft")),
                                       ifelse(is.na(steps$number_of_steps), "",  paste("<br><b># of Steps:</b>", steps$number_of_steps)),
                                       ifelse(is.na(steps$material), "",  paste("<br><b>Material:</b>", steps$material)),
                                       ifelse(is.na(steps$installed) | steps$installed == 0, "<br><b>Year Constructed:</b> Unknown", paste("<br><b>Year Constructed:</b>", steps$installed)),
                                       ifelse(is.na(steps$total_population), "",  paste("<br><b>Population nearby:</b>", steps$total_population)),
                                       ifelse(is.na(steps$schools_count), "",  paste("<br><b>Schools nearby:</b>", steps$schools_count)),
                                       ifelse(is.na(steps$transit_rider_count), "", paste("<br><b>Transit Riders nearby:</b>", steps$transit_rider_count)),
                                       ifelse(is.na(steps$overall_score), "", "<br><b>Network Scores</b> <ul>"),
                                       ifelse(is.na(steps$overall_score), "",  paste0("<li><b>Overall Score:</b> ", steps$overall_score, "/10 </li>")),
                                       ifelse(is.na(steps$transit_score), "",  paste0("<li><b>Transit Score:</b> ", steps$transit_score, "/10</li>")),
                                       ifelse(is.na(steps$school_score), "",  paste0("<li><b>School Score:</b> ", ifelse(steps$school_score == 100, "No Schools", paste0(steps$school_score, "/10")), "</li>")),
                                       ifelse(is.na(steps$detour_score), "",  paste0("<li><b>Detour Score:</b> ", steps$detour_score, "/10 </li>")),
                                       ifelse(is.na(steps$overall_score), "", "</ul>"),
                                       '</font>'))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("steps")
    }
    removeNotification("stepsMessage")
  })
  observe({
    # Retaining Walls
    if (input$toggleWalls) {
      leafletProxy("map", session = session) %>%
        clearGroup("walls")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading City Retaining Walls<center></div></font>')), type = "message", id = "wallsMessage", duration = NULL, closeButton = FALSE)
      
      walls <- wallsInput()
      if (nrow(walls) > 0) {
        leafletProxy("map", session = session) %>%
          addPolylines(data = walls, color = "#43a1a1", opacity = 0.75,
                       popup = ~(paste(ifelse(walls$image == "", "", paste0('<center><img id="imgPicture" src="', walls$image,'" style="width:250px;"></center>')),
                                       "<font color='black'><b>Location:</b>", walls$name,
                                       ifelse(is.na(walls$length) | walls$length == 0, "", paste("<br><b>Length:</b>", walls$length, "ft")),
                                       ifelse(is.na(walls$height) | walls$height == 0, "", paste("<br><b>Height:</b>", walls$height, "ft")),
                                       ifelse(is.na(walls$year_constructed) | walls$year_constructed == 0, "<br><b>Year Constructed:</b> Unknown", paste("<br><b>Year Constructed:</b>", walls$year_constructed)), '</font>'))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("walls")
    }
    removeNotification("wallsMessage")
  })
  # Pool Layers
  observe({
    if (input$togglePools) {
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Pools & Spray Parks<center></div></font>')), type = "message", id = "poolsMessage", duration = NULL, closeButton = FALSE)
      leafletProxy("map", session = session) %>%
        clearGroup("pools")
      
      pools <- poolsInput()
      if (nrow(pools) > 0) {
        leafletProxy("map", session = session) %>%
          addPolygons(data = pools, color = "#377eb8", fillColor = "#377eb8", fillOpacity = .5, group = "pools",
                      popup = ~(paste(ifelse(pools$image == "", "", paste0('<center><img id="imgPicture" src="', pools$image, '" style="width:250px;"></center>')),
                                      "<font color='black'><b>Name:</b>", pools$name,
                                      "<br><b>Usage:</b>", pools$type,
                                      "<br><b>Water Source:</b>", pools$water_source,
                                      ifelse(is.na(pools$capacity), "", paste("<br><b>Capacity:</b>", prettyNum(pools$capacity, big.mark = ","),"gal")), "</font>"))
                      )
      }
      wf <- wfInput()
      if (nrow(wf) > 0) {
        leafletProxy("map", session = session) %>%
          addCircleMarkers(data = wf, color = "#377eb8", fillColor = "#377eb8", fillOpacity = .5, radius = 2, group = "pools", lat = ~latitude, lng = ~longitude,
                           popup = ~(paste(ifelse(wf$image == "", "", paste0('<center><img id="imgPicture" src="', wf$image,'" style="width:250px;"></center>')),
                                           "<font color='black'><b>Location:</b>", wf$name,
                                           "<br><b>Feature Type:</b>", wf$feature_type,
                                           ifelse(is.na(wf$make), "", paste("<br><b>Make:</b>", wf$make)),
                                           ifelse(is.na(wf$control_type), "", paste("<br><b>Control:</b>", wf$control_type)),"</font>"))
          )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("pools")
    }
    removeNotification("poolsMessage")
  })
  # Waste Recovery Sites
  observe({
    if (input$toggleWaste) {
      leafletProxy("map", session = session) %>%
        clearGroup("waste")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Waste Recovery Sites<center></div></font>')), type = "message", id = "wasteMessage", duration = NULL, closeButton = FALSE)
      
      waste <- wasteInput()
      if (nrow(waste) > 0) {
        leafletProxy("map", session = session) %>%
          addCircleMarkers(data = waste, color = "#099fff", fillColor = "#099fff", fillOpacity = .5, radius = 8, group = "waste",
                           lng = ~longitude, lat = ~latitude,
                           popup = ~(paste("<font color='black'><b>Name:</b>", waste$link,
                                           "<br><b>City Location:</b>", waste$managed_by_city,
                                           "<br><b>Location:</b>", waste$address,
                                           "<br><b>Phone:</b>", waste$phone_number,
                                           "<br><b>Hours:</b>", waste$hours_of_operation,
                                           ifelse(waste$description == "", "", paste("<br><b>Materials:</b>", waste$description)),
                                           ifelse(waste$notes == "", "", paste("<br><b>Notes:</b>", waste$notes, "</font>"))))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("waste")
    }
    removeNotification("wasteMessage")
  })
  # Historic Regions
  observe({
    if (input$toggleDesignations) {
      
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Designations<center></div></font>')), type = "message", id = "designationMessage", duration = NULL, closeButton = FALSE)
      leafletProxy("map", session = session) %>%
        clearGroup("hist") %>%
        clearGroup("ppa")
      if (is.null(input$designation_select)) {
        histdist <- historicInput()
        if (nrow(histdist) > 0) {
          leafletProxy("map", session = session) %>%
            addPolygons(data = histdist, color = "#85144b", fillColor = "#85144b", fillOpacity = .5, group = "hist",
                        popup = ~(paste("<font color='black'><b>Historic District:</b>", histdist$name,
                                        '</font>'))
            )
        }
        ppa <- parkingInput()
        if (nrow(ppa) > 0) {
          leafletProxy("map", session = session) %>%
            addPolygons(data = ppa, color = "#85144b", fillColor = "#85144b", fillOpacity = .5, group = "ppa",
                        popup = ~(paste("<font color='black'><b>Parking Zone:</b>", ppa$code, '</font>'))
            )
        }
      } else {
        if ("Historic Districts" %in% input$designation_select) {
          histdist <- historicInput()
          if (nrow(histdist) > 0) {
            leafletProxy("map", session = session) %>%
              addPolygons(data = histdist, color = "#85144b", fillColor = "#85144b", fillOpacity = .5, group = "hist",
                          popup = ~(paste("<font color='black'><b>Historic District:</b>", histdist$name,
                                          '</font>'))
              )
          }
        }
        if ("Residential Parking Areas" %in% input$designation_select) {
          ppa <- parkingInput()
          if (nrow(ppa) > 0) {
            leafletProxy("map", session = session) %>%
              addPolygons(data = ppa, color = "#85144b", fillColor = "#85144b", fillOpacity = .5, group = "ppa",
                          popup = ~(paste("<font color='black'><b>Parking Zone:</b>", ppa$code, '</font>'))
              )
          }
        }
      } 
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("hist") %>%
        clearGroup("ppa")
    }
    removeNotification("designationMessage")
  })
  # Environmental Regions
  observe({
    if (input$toggleEnvironmental) {
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Environmnetal Regions<center></div></font>')), type = "message", id = "environMessage", duration = NULL, closeButton = FALSE)
      
      environmental <- environmentalInput()
      if (nrow(environmental) > 0) {
        leafletProxy("map", session = session) %>%
          clearGroup("environ") %>%
          addPolygons(data = environmental, color = "#a65628", fillColor = "#a65628", fillOpacity = .5, group = "environ",
                      popup = ~(paste("<font color='black'><b>Region:</b>", environmental$layer,
                                      ifelse(is.na(environmental$name), "", paste("<br><b>Name:</b>", environmental$name)),
                                      '</font>'))
          )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("environ")
    }
    removeNotification("environMessage")
  })
  # Traffic Signals
  observe({
    if (input$toggleIntersections) {
      leafletProxy("map", session = session) %>%
        clearGroup("intersections")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Intersection Data<center></div></font>')), type = "message", id = "interMessage", duration = NULL, closeButton = FALSE)

      si <- siInput()
      if (nrow(si) > 0) {
        leafletProxy("map", session = session) %>%
          addCircleMarkers(data = si, color = "#e41a1c", fillColor = "#e41a1c", fillOpacity = .5, radius = 2, group = "intersections",
                           popup = ~(paste("<font color='black'><b>Location:</b>", si$description,
                                           ifelse(is.na(si$operation_type), "", paste("<br><b>Operation Type:</b>", si$operation_type)),
                                           ifelse(is.na(si$flash_time), "", paste("<br><b>Flash Time:</b>", si$flash_time)),
                                           ifelse(is.na(si$flash_yellow), "", paste("<br><b>Flash Yellow:</b>", si$flash_yellow)),"</font>"))
        )
      }
      # Markings
      mark <- markInput()
      if (nrow(mark) > 0) {
        leafletProxy("map", session = session) %>%
          addPolylines(data = mark, color = "#e41a1c", fillColor = "#e41a1c", fillOpacity = .5, group = "intersections",
                       popup = ~(paste("<font color='black'><b>Type:</b>", mark$type,
                                       "<br><b>Location:</b>", mark$street, "</font>"))
        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("intersections")
    }
    removeNotification("interMessage")
  })
  # Traffic Signs
  observe({
    if (input$toggleSigns) {
      leafletProxy("map", session = session) %>%
        clearGroup("intersections")
      showNotification(HTML(paste0('<center><font color = "white"><div class="loading">Loading Street Sign Data<center></div></font>')), type = "message", id = "signMessage", duration = NULL, closeButton = FALSE)
      
      signs <- signsInput()
      if (nrow(signs) > 0) {
        leafletProxy("map", session = session) %>%
          addCircleMarkers(data = signs, color = "#000000", fillColor = "#000000", fillOpacity = .5, radius = 2, group = "signs",
                           popup = ~(paste0("<font color='black'><b>Sign Type:</b> ", signs$description, " (", signs$mutcd_code, ")",
                                            "<br><b>Location:</b> ", trimws(paste(ifelse(is.na(signs$address_number), "", signs$address_number), signs$street), "both"),
                                            "<br><b>Mounting Fixture:</b> ", signs$mounting_fixture,
                                            ifelse(is.na(signs$date_installed),"" , paste0("<br><b>Installed Date:</b> ", signs$date_installed))))

        )
      }
    } else {
      leafletProxy("map", session = session) %>%
        clearGroup("signs")
    }
    removeNotification("signMessage")
  })
  # Easter Egg check to see what layers are active and if they have any data
  observe({
    layersCount <- 0
    # City Facilities
    if (input$toggleFacilities) {
      facilities <- facilitiesInput()
      if (nrow(facilities) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Paving Schedule
    if (input$toggleStreets) {
      streets <- streetsInput()
      if (nrow(streets) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Bridges
    if (input$toggleBridges) {
      bridges <- bridgesInput()
      if (nrow(bridges) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Recreation
    if (input$toggleRecreation) {
      # Parks
      parks <- parksInput()
      if (nrow(parks) > 0) {
        layersCount <- layersCount + 1
      }
      # Greenways
      greenways <- greenwaysInput()
      if (nrow(greenways) > 0) {
        layersCount <- layersCount + 1
      }
      # Playgrounds
      playgrounds <- playgroundsInput()
      if (nrow(playgrounds) > 0) {
        layersCount <- layersCount + 1
      }
      # Court & Rinks
      courts <- courtsInput()
      if(nrow(courts) > 0) {
        layersCount <- layersCount + 1
      }
      # Playing Fields
      fields <- fieldsInput()
      if (nrow(fields) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # CLP
    if (input$toggleLibs) {
      libs <- libsInput()
      if (nrow(libs) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Steps
    if (input$toggleSteps) {
      steps <- stepsInput()
      if (nrow(steps) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Walls
    if (input$toggleWalls) {
      walls <- wallsInput()
      if (nrow(walls) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Pools
    if (input$togglePools) {
      pools <- poolsInput()
      if (nrow(pools) > 0) {
        layersCount <- layersCount + 1
      }
      wf <- wfInput()
      if (nrow(wf) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Waste Disposal Sites
    if (input$toggleWaste) {
      waste <- wasteInput()
      if (nrow(waste) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Historic Districts
    if (input$toggleDesignations) {
      if (is.null(input$designation_select)) {
        histdist <- historicInput()
        if (nrow(histdist) > 0) {
          layersCount <- layersCount + 1
        }
        ppa <- parkingInput()
        if (nrow(ppa) > 0) {
          layersCount <- layersCount + 1
        }
      } else {
        if ("Historic Districts" %in% input$designation_select) {
          histdist <- historicInput()
          if (nrow(histdist) > 0) {
            layersCount <- layersCount + 1
          }
          if ("Residential Parking Areas" %in% input$designation_select) {
            ppa <- parkingInput()
            if (nrow(ppa) > 0) {
              layersCount <- layersCount + 1
            }
          }
        }
      }
    }
    # Enviornmental Layers
    if (input$toggleEnvironmental) {
      environmental <- environmentalInput()
      if (nrow(environmental) > 0) {
        layersCount <- layersCount + 1
      }
    }
    # Intersection Data
    if (input$toggleIntersections) {
      # Signals
      si <- siInput()
      if (nrow(si) > 0) {
        layersCount <- layersCount + 1
      }
      # Markings
      mark <- markInput()
      if (nrow(mark) > 0) {
        layersCount <- layersCount + 1
      }
    }
    if (input$toggleSigns) {
      # Signs
      signs <- signsInput()
      if (nrow(signs) > 0) {
        layersCount <- layersCount + 1
      }
    }
  
    # If no active layers, build egg dataframe
    if (layersCount == 0) {
      if (input$search == "Vote!") {
        egg <- easterEgg()
      } else {
        egg <- easterEgg()
        egg <- egg[sample(1:nrow(egg),1),]
      }

      leafletProxy("map", session = session) %>%
        clearGroup("egg") %>%
        addMarkers(data = egg, ~X, ~Y, icon = ~icons_egg[icon], popup = ~tt, group = "egg") %>%
        setView(-79.9959, 40.4406, zoom = 12)
    } else {
      # Clear old easter egg
      leafletProxy("map", session =session) %>%
        clearGroup("egg")
    }
    #Write inputs to Couch
    if (url.exists(paste0(couchdb_url, ":5984/_utils/"))){
      dateTime <- Sys.time()
      names(dateTime) <- "dateTime"
      inputs <- isolate(reactiveValuesToList(input))
      couchDB$dataList <- c(inputs, sessionID, dateTime, sessionStart)
      cdbAddDoc(couchDB)
    }
  })
})

enableBookmarking("url")

# Run the application 
shinyApp(ui = ui, server = server)