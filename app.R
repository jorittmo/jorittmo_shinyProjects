library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(plotly)
library(broom)

###############################################
#
# Covid Exploration
#
###############################################

cov_data_full <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# load("data/cov.RData")
# cov_data_full <- x
load("data/var_desc.Rdata")


latest_date <- cov_data_full %>% select(date) %>% arrange(desc(date)) %>% slice(1)

cont <- c("Africa", "Asia","Europe", "North America", "Oceania", "South America")
country_names <- list()
for (i in 1:length(cont)) {
  country_names[[paste(cont[i])]] <- na.omit(unique(cov_data_full$location[cov_data_full$continent == cont[i]]))
}

var_names <- cov_data_full %>% select(-starts_with("excess"), -where(is.character), -date) %>%  colnames() %>% sort()

get_scatter <- function(date_chosen, show_trend, xvar, yvar, logx, logy){
  
  xvar <- str_replace_all(tolower(xvar), " ", "_")
  yvar <- tolower(str_replace_all(yvar, " ", "_"))
  
  cov_scat <- cov_data_full %>% filter(date == date_chosen & location != "World")
  
  ggplot(cov_scat, aes_string(x = paste(xvar), y= paste(yvar)))+
    geom_point(aes(name = location), alpha = 0.5)+
    geom_smooth(method = "lm", color = "#f0ead6", se = FALSE, alpha = 0.1) +
    theme_dark() +
    theme(plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
          axis.text = element_text(colour="gray"), 
          axis.title = element_text(colour="Gray", size = 15)) -> p1
  
  p1 + labs(x = str_to_title(str_replace_all(p1[["labels"]][["x"]], "_", " ")), 
            y = str_to_title(str_replace_all(p1[["labels"]][["y"]], "_", " "))) -> p1
  
  if(logx) p1 <- p1 + scale_x_log10()
  if(logy) p1 <- p1 + scale_y_log10()
  
  if(!show_trend) p1$layers[[2]] <- NULL
  ggplotly(p1)
  
}


get_deaths <- function(date_chosen, country, show_world, yvar){
  
  yvar <- str_replace_all(tolower(yvar), " ", "_")
  
  if (show_world) country <- c("World", country)
  
  cov_deaths <- cov_data_full %>% filter(date < date_chosen & location %in% country)
  
  ggplot(cov_deaths, aes_string(x = "date", y=paste(yvar), color = "location"))+
    #geom_point(shape = 1, size = 1)+
    geom_line() +
    theme_dark() +
    xlab("")+
    theme(plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
          axis.text = element_text(colour="gray"), 
          axis.title = element_text(colour="Gray", size = 15), 
          legend.background = element_rect(color = "#272b30", fill = "#272b30"), 
          legend.text = element_text(color = "Gray"), 
          legend.title = element_text(color = "Gray"),
          legend.position = "top") -> p2
  p2 + labs(y = str_to_title(str_replace_all(p2[["labels"]][["y"]], "_", " "))) -> p2
  ggplotly(p2) %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
}


get_map <- function(date_chosen, color_var) {
  
  
  countries <- read.csv("data/countries.csv") %>% 
    select(country, latitude, longitude, alpha3)%>% 
    mutate(location = recode(country, 'United States Minor Outlying Islands' = 'United States'))
  
  
  cov_data <- cov_data_full %>% inner_join(countries, by = c("iso_code" = "alpha3")) %>%
    drop_na(total_deaths_per_million) %>% 
    group_by(location.x) %>%
    fill(people_fully_vaccinated_per_hundred) %>% 
    fill(reproduction_rate) %>% 
    dplyr::ungroup() %>% 
    filter(iso_code != "GIB" & date == date_chosen) %>%  
    mutate(iso_code = recode(iso_code, 'PSE' = 'PSX', 'SSD' = 'SDS')) %>% 
    distinct(iso_code, .keep_all = TRUE) %>%
    arrange(iso_code)
  
  
  
  worldcountry = geojson_read("data/50m.geojson", what = "sp")
  
  if (color_var == "dpm"){
    bins = c(0,50,500,1000,2000,3000,Inf)
    cov_pal <- colorBin("Reds", domain = cov_data$total_deaths_per_million, bins = bins)
    title_map <-  "<small>Deaths per million</small>"
    map_var <- cov_data$total_deaths_per_million
  } else if (color_var == "rrate"){
    bins = c(0, 0.5, 1, 2, 3, 5, Inf)
    cov_pal <- colorBin("Reds", domain = cov_data$reproduction_rate, bins = bins)
    title_map = "<small>Reproduction rate</small>"
    map_var <- cov_data$reproduction_rate
  } else if (color_var == "strind"){
    bins = c(0, 10, 30, 50, 70, 80, 90)
    cov_pal <- colorBin("Reds", domain = cov_data$stringency_index, bins = bins)
    title_map = "<small>Stringency index</small>"
    map_var <- cov_data$stringency_index
  } else if (color_var == "fullvax"){
    bins = c(0, 10, 30, 50, 70, 80, 90)
    cov_pal <- colorBin("Greens", domain = cov_data$people_fully_vaccinated_per_hundred, bins = bins)
    title_map = "<small>Fully vaccinated people per hundred</small>"
    map_var <- cov_data$people_fully_vaccinated_per_hundred
  } else if (color_var == "slope") {
    cov_data_full %>% 
      select(location, date, new_cases_smoothed_per_million) %>% 
      filter(date > as.Date(date_chosen) - 10 & date <= as.Date(date_chosen) & !is.na(new_cases_smoothed_per_million)) %>% 
      nest(data = !location) %>% 
      mutate(model = map(data, ~lm(new_cases_smoothed_per_million ~ date, data = .))) %>% 
      mutate(tidied = map(model, tidy)) %>% 
      unnest(tidied) %>% 
      filter(term == "date") %>% 
      select(location, estimate) %>% 
      inner_join(cov_data, by = c("location"="location.x"))%>%
      arrange(iso_code)-> model_data
    
    bins = c(-Inf, -10, -5, -3, -1, 0, 10, 50, 100, 200, Inf)
    cov_pal <- colorBin("RdYlGn", domain = model_data$estimate, bins = bins, reverse = TRUE)
    title_map = "<small>Average increase or decrease <br/> of new cases per million <br/> per day over last 10 days </small>"
    map_var <- model_data$estimate
    
  } 
  
  plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cov_data$iso_code, ]
  
  m <- leaflet(plot_map) %>%
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.DarkMatter)%>% 
    addLegend("bottomright", pal = cov_pal, values = ~map_var,
              title = title_map) %>% 
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
                fillColor = ~cov_pal(map_var)) %>%   
    addLayersControl(
      position = "topright",
      overlayGroups = c("Total deaths", "R rate"),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("R rate", "Fully vacced people"))  %>%
    addCircleMarkers(data = cov_data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(total_deaths_per_million)^(1/4),
                     fillOpacity = 0.2, color = "red", group = "Total deaths",
                     label = sprintf("<strong>%s </strong><br/>Cases per mil: %g<br/>Deaths per mil: %g",
                                     cov_data$location.x,
                                     cov_data$total_cases_per_million,
                                     cov_data$total_deaths_per_million) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px", direction = "auto")) %>% 
    addCircleMarkers(data = cov_data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cov_data$reproduction_rate)*2,
                     fillOpacity = 0.2, color = "blue", group = "R rate",
                     label = sprintf("<strong>%s </strong><br/>Cases per mil: %g<br/>Deaths per mil: %g",
                                     cov_data$location.x,
                                     cov_data$total_cases_per_million,
                                     cov_data$total_deaths_per_million) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px", direction = "auto")) 
  m
}


# Stylize the dropdown meny

onInitialize <- '
$(function() {
  $("body").on("mousedown", ".selectize-dropdown-content", function(e){
    e.preventDefault();
    return false;
  });
  $("body").on("click", ".optgroup-header", function(){
    $(this).siblings().toggle();
  });
});'

onDropdownOpen <- '
function(el){
  setTimeout(function(){
    $(el).find(".optgroup .option").hide();
  }, 0);
}'



###############################################
#
# UN Voting
#
###############################################











###############################################
#
# Shiny UI
#
###############################################


ui = navbarPage("Projects", theme = shinytheme("slate"), 
                
                # define the tabs to be used in the app ----------------------------------------
                # introduction splash
                # visualisation of visits mapped on to interactive map
                tabPanel("Covid-19", 
                         sidebarLayout(
                           sidebarPanel(h3("Covid Explorer"),
                                        h4(paste0("Updated: ", format(latest_date$date, "%b, %d, %Y"))),
                                        sliderInput("date",
                                                    "Date:",
                                                    min = as.Date("2020-01-24"),
                                                    max = as.Date(max(cov_data_full$date)),
                                                    value = as.Date(latest_date$date[1]), 
                                                    ticks = FALSE),
                                        radioButtons("map_coloring", "Color map according to:",
                                                     c("Deaths per million" = "dpm",
                                                       "Reproduction rate" = "rrate",
                                                       "Stringency index" = "strind",
                                                       "Fully vaccinated people per hundred" = "fullvax",
                                                       "Average increase or decrease of new cases per million/day over last 10 days" = "slope")),
                                        tags$script(HTML(onInitialize)),
                                        selectizeInput("countries", "Countries to compare:",
                                                       country_names, multiple = TRUE,
                                                       selected = c("Sweden", "Norway", "Denmark", "Finland", "Iceland"),
                                                       options = list(
                                                         onDropdownOpen = I(onDropdownOpen)
                                                       )),
                                        selectInput("yvar", "Variable for Y-axis:",
                                                    str_to_title(str_replace_all(var_names, "_", " ")), multiple = FALSE,
                                                    selected = c("Total Deaths Per Million")), 
                                        selectInput("xvar", "Variable for X-axis:",
                                                    str_to_title(str_replace_all(var_names, "_", " ")),
                                                    multiple = FALSE,
                                                    selected = c("Median Age")),
                                        p("If no observations show for the chosen date and variable, please choose another date."),
                                        
                                        width = 2
                           ),
                           
                           mainPanel(
                             fluidPage(
                               column(width = 10,
                                      leafletOutput("map", height = 500),
                                      box(
                                        title = "Variable vs variable at chosen date",
                                        fluidRow(
                                          column(width = 3,
                                                 checkboxInput("trendline", "Show trend", value = FALSE)
                                          ),
                                          column(width = 4,
                                                 checkboxInput("logx", "Logscale X axis", value = FALSE)
                                          ),                             
                                          column(width = 4,
                                                 checkboxInput("logy", "Logscale Y axis", value = FALSE)
                                          )
                                        ),
                                        #plotOutput("deathmil"),
                                        plotlyOutput("scat")
                                      ),
                                      box(
                                        title = "Country vs country, over time",
                                        fluidRow(
                                          column(width = 3,
                                                 checkboxInput("show_global", "Show global", value = FALSE)
                                          )
                                        ),
                                        #plotOutput("totdeathmil"),
                                        plotlyOutput("totdeathmil")
                                      )
                                      
                               ),
                               sidebarPanel(h4("Variable descriptions"),
                                            strong(textOutput("ydescvar")),
                                            textOutput("ydesc"),
                                            h3(),
                                            strong(textOutput("xdescvar")),
                                            textOutput("xdesc"),
                                            h3(),
                                            strong(p("About the data")),
                                            tagList("Data accessed from", a("here", href="https://ourworldindata.org/coronavirus")),
                                            width = 2)), 
                             width = 10
                           )
                           
                         )
                ),
                
                # journey time histogram(s)
                tabPanel("UN Voting",
                         fluidRow(column(12,
                                         h1("UN Voting"),
                                         p("Project page for analysis of UN votes"),
                                         br(),
                                         h4("Instructions"),
                                         p("Use the radio buttons"))),
                         hr(),
                         fluidRow(sidebarPanel(width = 3,
                                               h4("Weekends or weekdays?"),
                                               helpText("Chose whether you would like to see"),
                                               radioButtons("day", NULL,
                                                            c("Weekday" = "Weekday",
                                                              "Weekend" = "Weekend",
                                                              "Both" = "Both"))),
                                  mainPanel(plotOutput("hist", height = 500)))
                )
                # close the UI definition
)








server <- function(input, output) {
  
  # Output for Covid explorer
  
  output$map <- renderLeaflet({
    get_map(input$date, input$map_coloring) %>%  setView(30, 15, 2)
  })
  
  output$scat <- renderPlotly({
    get_scatter(input$date, input$trendline, input$xvar, input$yvar, input$logx, input$logy)
  })
  
  output$totdeathmil <- renderPlotly({
    get_deaths(input$date, input$countries, input$show_global, input$yvar)
  })
  
  output$ydescvar <- renderText({
    input$yvar
  })
  output$ydesc <- renderText({
    var_desc %>% filter(Variable == str_replace_all(tolower(input$yvar), " ", "_")) %>% select(Description) %>%  magrittr::extract2(1)
  })
  
  output$xdescvar <- renderText({
    input$xvar
  })
  output$xdesc <- renderText({
    var_desc %>% filter(Variable == str_replace_all(tolower(input$xvar), " ", "_")) %>% select(Description) %>%  magrittr::extract2(1)
  })
  
  
  # Output for UN voting
  
  # output$map2 <- renderLeaflet({
  #   get_map(input$date, input$map_coloring) %>%  setView(30, 15, 2)
  # })
  
  
  
}

shinyApp(ui, server)