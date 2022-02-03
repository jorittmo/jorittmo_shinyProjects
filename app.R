library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(plotly)
library(broom)
library(janitor)
library(countrycode)
library(cluster)
library(dendextend)
library(fpc)
library(shinyjs)
library(paletteer)



###############################################
#
# Covid Exploration
#
###############################################

#cov_data_full <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
load("data/cov.RData")
cov_data_full <- x
load("data/var_desc.Rdata")

countries <- read.csv("data/countries.csv") %>% 
  select(country, latitude, longitude, alpha3)%>% 
  mutate(location = recode(country, 'United States Minor Outlying Islands' = 'United States'))

worldcountry = geojson_read("data/50m.geojson", what = "sp")

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
    geom_smooth(method = "lm", color = "#967bb6", se = FALSE, alpha = 0.1) +
    theme_dark() +
    theme(plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
          axis.text = element_text(colour="gray"), 
          axis.title = element_text(colour="Gray", size = 15), 
          panel.background = element_rect(fill = "#FFFDE4",
                                          colour = "#FFFDE4",
                                          size = 0.5, linetype = "solid")) -> p1
  
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
    labs(color = "", x = "") +
    theme(plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
          axis.text = element_text(colour="gray"), 
          axis.title = element_text(colour="Gray", size = 15), 
          legend.background = element_rect(color = "#272b30", fill = "#272b30"), 
          legend.text = element_text(color = "Gray"), 
          legend.title = element_text(color = "Gray"),
          legend.position = "top", 
          panel.background = element_rect(fill = "#FFFDE4",
                                          colour = "#FFFDE4",
                                          size = 0.5, linetype = "solid")) -> p2
  p2 + labs(y = str_to_title(str_replace_all(p2[["labels"]][["y"]], "_", " "))) -> p2
  ggplotly(p2) %>% layout(legend = list(orientation = "h", x = 0, y = -0.1))
}


get_map <- function(date_chosen, color_var) {
  

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

load("data/votes.Rdata")
load("data/gdp_long.Rdata")

votes <- votes %>%  filter(vote <= 3) %>% 
  mutate(country = countrycode(ccode, "cown", "country.name", custom_match = c('260' = 'German Federal Republic')),
         other = ifelse(me + nu + di + hr + co + ec == 0, TRUE, FALSE),
         across(me:ec, as.logical)) 


##########
#
# Clustering
#
##########

votes_joined <- votes %>% left_join(gdp_long, by = c("country_code" = "country_code", "year" = "year")) %>%
  group_by(country_code) %>% tidyr::fill(gdp, .direction = "downup") %>% ungroup()

gdp_filter <- votes_joined %>%
  filter(year >= 2010 & year <= 2020) %>%
  group_by(country_code) %>%
  summarise(
    max_gdp = max(gdp)
  ) %>% slice_max(max_gdp, prop = 0.55)


filt_vote <- votes %>%  filter(year >= 2010 & year <= 2020) %>% 
  filter(country_code %in% gdp_filter$country_code) %>%
  select(country, vote, year, rcid) %>% 
  arrange(country) %>% 
  pivot_wider(names_from = country, values_from = vote) %>% 
  select(-year, -rcid)

filt_vote <- filt_vote %>% relocate(sort(colnames(filt_vote)))

filt_vote = filt_vote[ ,!sapply(filt_vote, function(x) mean(is.na(x)))> 0.5]

votes_tree <- as.data.frame(t(filt_vote)) %>% mutate(across(everything(), as_factor))



removed_countries <-  votes %>% filter(country %in% rownames(votes_tree)) %>%
  select(country_code, country) %>% distinct() %>% filter(country_code %in% c("CSK", "YAR", "EAZ", "DDR", "TUV", "SSD"))

included_countries <- votes %>% filter(country %in% rownames(votes_tree)) %>%
  select(country_code, country) %>% distinct() %>% filter(!country_code %in% c("CSK", "YAR", "EAZ", "DDR", "TUV", "SSD"))


clust_method = "complete"

gower_df_part <- daisy(votes_tree,
                       metric = "gower")


tree_part <- hclust(gower_df_part, method = clust_method)


clust_metrics <- data.frame(type = c(), value = c(), k = c())
for (k in 2:10) {
  clusters <- cutree(tree_part, k = k)
  clust_stats <- cluster.stats(gower_df_part, clusters)
  stats_df <- data.frame(type = c("withinss", "silwidth"),
                         value = c(clust_stats$within.cluster.ss, clust_stats$avg.silwidth),
                         k = c(k, k))
  clust_metrics <- rbind(clust_metrics, stats_df)
}

k = clust_metrics %>% filter(type == "silwidth") %>% 
  slice_max(value) %>% select(k) %>% magrittr::extract2(1)


dend <- as.dendrogram(tree_part)

gdend <- dendextend::as.ggdend(dend %>%
                                 set('branches_k_color', k = k) %>%
                                 set('branches_lwd', 0.6) %>%
                                 set('labels_colors', k = k) %>%
                                 set('labels_cex', 0.6))

clust_dat <- gdend$labels %>% 
  mutate(country = label) %>% 
  select(country, col) %>% 
  filter(!country %in% removed_countries$country) %>% 
  left_join(included_countries, by = "country") %>% 
  mutate(clust = paste("Cluster", as.numeric(as_factor(col)))) %>% 
  arrange(country_code)


map_UN <- worldcountry[worldcountry$ADM0_A3 %in% clust_dat$country_code, ]

leaflet(map_UN) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
              fillColor = ~clust_dat$col) -> basemap_UN

rm(votes_joined, gdp_filter, filt_vote, votes_tree, dend, gdend, map_UN)

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
                         sidebarLayout(
                           sidebarPanel(h3("UN Voting"),
                                        p("Use hierarchical clustering and explore voting patterns for the General Assembly of the United Nations. Choose time range and/or specific topics to explore."),
                                        useShinyjs(),
                                        sliderInput("year2",
                                                    "Year:",
                                                    min = as.Date(paste(min(votes$year), 1, 1, sep = "-")),
                                                    max = as.Date(paste(max(votes$year), 1, 1, sep = "-")),
                                                    value = c(as.Date(paste(2010, 1, 1, sep = "-")), 
                                                              as.Date(paste(2020, 1, 1, sep = "-"))),
                                                    ticks = FALSE, 
                                                    step = 365,
                                                    timeFormat = "%Y"),
                                        checkboxGroupInput(
                                          "topic",
                                          "Select topic(s):",
                                          choices = list(
                                            "Palestinian conflict" = "me",
                                            "Nuclear weapons and nuclear material" = "nu",
                                            "Arms control and disarmament" = "di",
                                            "Human rights" = "hr",
                                            "Colonialism" = "co",
                                            "Economic development" = "ec",
                                            "Other" = "other"
                                          ),
                                          selected = NULL, #c("me", "nu", "di", "hr", "co", "ec", "other"),
                                          inline = FALSE,
                                          width = NULL,
                                          choiceNames = NULL,
                                          choiceValues = NULL
                                        ),
                                        sliderInput("gdp_filt",
                                                    "Choose proportion of countries to include based on max GDP of chosen time period:",
                                                    min = 10,
                                                    max = 100,
                                                    value = 55, 
                                                    step = 1, 
                                                    post  = " %"),
                                        sliderInput("na_prop",
                                                    "Maximum percentage of missing votes allowed (per country):",
                                                    min = 1,
                                                    max = 50,
                                                    value = 50, 
                                                    step = 1, 
                                                    post  = " %"),
                                        h3("Cluster settings"),
                                        radioButtons("cluster", 
                                                     "Select automatic or manual clustering:",
                                                     c("Automatic" = "aut",
                                                       "Manual" = "man")
                                                     ),
                                        p("Automatic clustering base the number of clusters on the maximum sillhoutte width of various cluster sizes."),
                                        sliderInput(inputId = "clust", label = "Number of clusters",
                                                    min = 2, max = 10, value = k, ticks = FALSE),
                                        radioButtons("linkage", "Linkage method:",
                                                     c("Complete" = "complete", "Single"="single", "Average" = "average"),
                                                     inline = TRUE, selected = "complete"),
                                        h4("About the data"),
                                        tagList('UN voting data', 
                                                em('(Erik Voeten "Data and Analyses of Voting in the UN General Assembly" Routledge Handbook of International Organization)'),
                                                'accessed',
                                                a("here.", href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ"),
                                                "Source for GDP per year and country:",
                                                a("Worldbank.", href="https://data.worldbank.org/indicator/NY.GDP.MKTP.CD")),
                                        
                                        width = 2
                           ),
                           
                           mainPanel(
                             fluidPage(
                               column(width = 8,
                                      leafletOutput("map_UN", height = 400),
                                      column(width = 6,
                                             plotOutput("bar", height = 400)  
                                      ),
                                      column(width = 6,
                                             plotOutput("overtime", height = 400)  
                                      )
                                      
                                      
                               ),
                               column(width = 4,
                                      plotOutput("dendro", height = 800)
                                      
                               )), 
                             width = 10
                           )
                           
                         )
                )
                # close the UI definition
)




server <- function(input, output, session) {
  
  # INPUT FOR UN PROJECT
  
  type_clust <- eventReactive(c(input$clust, input$cluster), {
    input$cluster
  })
  
  
  observeEvent(c(input$cluster, cluster_data()$k), {
    # Change the following line for more examples
    if (input$cluster == "man"){
      enable("clust")
    }
    if (input$cluster == "aut"){
      disable("clust")
      updateSliderInput(session, "clust", value = cluster_data()$k)
    }
    
  })
  
  
  
  gdp_filter <- reactive({
    # Decide what to do with North Korea
    votes_joined <- votes %>% left_join(gdp_long, by = c("country_code" = "country_code", "year" = "year")) %>% 
      group_by(country_code) %>% tidyr::fill(gdp, .direction = "downup") %>% ungroup()
    
    gdp_filter <- votes_joined %>% 
      filter(year >= as.numeric(format(input$year2[1], "%Y")) & year <= as.numeric(format(input$year2[2], "%Y"))) %>%
      group_by(country_code) %>% 
      summarise(
        max_gdp = max(gdp)
      ) %>% slice_max(max_gdp, prop = input$gdp_filt/100)
    
    gdp_filter$country_code
  })
  
  cluster_data <- reactive({
    
    
    if (length(input$topic) > 0){
      topic_mask <- rowSums(votes[ , c(input$topic)]) > 0  
    } else {
      topic_mask <- !logical(nrow(votes))
    }
    
    
    filt_vote <- votes %>%  filter(topic_mask) %>% 
      filter(year >= as.numeric(format(input$year2[1], "%Y")) & year <= as.numeric(format(input$year2[2], "%Y"))) %>% 
      filter(country_code %in% gdp_filter()) %>% 
      select(country, vote, year, rcid) %>% 
      pivot_wider(names_from = country, values_from = vote) %>% 
      select(-year, -rcid)
    
    filt_vote <- filt_vote %>% relocate(sort(colnames(filt_vote)))
    
    filt_vote = filt_vote[ ,!sapply(filt_vote, function(x) mean(is.na(x)))> input$na_prop/100]
    
    votes_tree <- as.data.frame(t(filt_vote)) %>% mutate(across(everything(), as_factor))
    
    
    removed_countries <-  votes %>% filter(country %in% rownames(votes_tree)) %>%
      select(country_code, country) %>% distinct() %>% filter(country_code %in% c("CSK", "YAR", "EAZ", "DDR", "TUV", "SSD"))
    
    included_countries <- votes %>% filter(country %in% rownames(votes_tree)) %>%
      select(country_code, country) %>% distinct() %>% filter(!country_code %in% c("CSK", "YAR", "EAZ", "DDR", "TUV", "SSD"))
    
    
    map_UN <- worldcountry[worldcountry$ADM0_A3 %in% included_countries$country_code, ]
    
    
    clust_method = input$linkage
    
    gower_df_part <- daisy(votes_tree,
                           metric = "gower")
    
    
    tree_part <- hclust(gower_df_part, method = clust_method)
    
    
    if (input$cluster == "aut") {
      clust_metrics <- data.frame(type = c(), value = c(), k = c())
      for (k in 2:10) {
        clusters <- cutree(tree_part, k = k)
        clust_stats <- cluster.stats(gower_df_part, clusters)
        stats_df <- data.frame(type = c("withinss", "silwidth"),
                               value = c(clust_stats$within.cluster.ss, clust_stats$avg.silwidth),
                               k = c(k, k))
        clust_metrics <- rbind(clust_metrics, stats_df)
      }
      
      k = clust_metrics %>% filter(type == "silwidth") %>% 
        slice_max(value) %>% select(k) %>% magrittr::extract2(1)
    } else {
      k = input$clust
    }

    
    dend <- as.dendrogram(tree_part)
    
    gdend <- dendextend::as.ggdend(dend %>%
                                     set('branches_k_color', k = k) %>%
                                     set('branches_lwd', 0.6) %>%
                                     set('labels_colors', k = k) %>%
                                     set('labels_cex', 0.6))
    
    clust_dat <- gdend$labels %>% 
      mutate(country = label) %>% 
      select(country, col) %>% 
      filter(!country %in% removed_countries$country) %>% 
      left_join(included_countries, by = "country") %>% 
      mutate(clust = paste("Cluster", as.numeric(as_factor(col)))) %>% 
      arrange(country_code)
    
    
    what_vote <- votes_tree %>% rownames_to_column("country") %>% inner_join(clust_dat, by ="country") %>% 
      pivot_longer(colnames(votes_tree), values_to = "vote") %>% 
      select(-name) %>% 
      mutate(vote = case_when(
        vote == 1 ~ "Yes",
        vote == 2 ~ "Abstain",
        vote == 3 ~ "No"
      ))
    
    clust_col <- clust_dat %>% select(clust, col) %>% distinct() %>% arrange(clust)
    
    
    over_time <- votes %>% 
      filter(year >= as.numeric(format(input$year2[1], "%Y")) & year <= as.numeric(format(input$year2[2], "%Y"))) %>% 
      inner_join(clust_dat, by = "country_code") %>% 
      group_by(clust, year) %>% 
      summarise(percent_yes = mean(vote == 1))
    
    
    list(votes_tree = votes_tree, gdend = gdend, polygons = map_UN,
         map_clust = clust_dat$col, k = k, what_vote = what_vote, clust_col = clust_col, over_time = over_time)
  })

  
  # OUTPUT FOR COVID EXPLORER
  
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
  

  
  # UN output

  
  output$map_UN <- renderLeaflet({

      basemap_UN  %>%  setView(15, 15, 1)

  })

  observeEvent(c(input$year2, input$na_prop, input$topic, input$clust, input$gdp_filt, input$linkage), {
    
      leafletProxy("map_UN") %>%
        clearMarkers() %>%
        clearShapes() %>%
        addPolygons(data = cluster_data()$polygons, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
                    fillColor = ~cluster_data()$map_clust) %>%  setView(15, 15, 1)
  })
  
  output$dendro <- renderPlot({
    
    ggplot(cluster_data()$gdend, horiz = T, offset_labels = -0.04) +
      theme(plot.background = element_rect(color = "#272b30", fill = "#272b30")) +
      scale_y_reverse(expand = c(0.2, 0.2)) + scale_x_continuous(expand = c(.02, .02)) 
  })
  
  output$bar <- renderPlot({
    
    ggplot(cluster_data()$what_vote, aes(clust, fill = vote)) +
      scale_fill_paletteer_d("colorBlindness::paletteMartin")+
      geom_bar(position = "fill") +
      theme_dark() +
      labs(y = "Proportion of Votes", x = "", fill = "") +
      theme(
        plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
        axis.text.y = element_text(colour="gray"),
        axis.text.x = element_text(colour=cluster_data()$clust_col$col),
        axis.title = element_text(colour="Gray", size = 15), 
        legend.background = element_rect(color = "#272b30", fill = "#272b30"), 
        legend.text = element_text(color = "Gray"), 
        legend.title = element_text(color = "Gray"),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFDE4",
                                        colour = "#FFFDE4",
                                        size = 0.5, linetype = "solid")) -> pbar
    pbar
  })
  
  output$overtime <- renderPlot({
    ggplot(cluster_data()$over_time, aes(x = lubridate::ymd(year, truncated = 2L), y = percent_yes, color = clust)) +
      geom_point(size = 3) +
      geom_line(lwd = 1) +
      labs(x = "", y = "Yes votes", col = "") +
      scale_color_manual(values=cluster_data()$clust_col$col) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme_dark() +
      theme(
        plot.background = element_rect(color = "#272b30", fill = "#272b30"), 
        axis.text.y = element_text(colour="gray"),
        axis.text.x = element_text(colour="gray"),
        axis.title = element_text(colour="Gray", size = 15), 
        legend.background = element_rect(color = "#272b30", fill = "#272b30"), 
        legend.text = element_text(color = "Gray"), 
        legend.title = element_text(color = "Gray"),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFDE4",
                                        colour = "#FFFDE4",
                                        size = 0.1, linetype = "solid")) -> ptime
    ptime
    #ggplotly(p) %>% layout(legend = list(orientation = 'h', x = 0, y = 1.1))
    
  })
  
  
}

shinyApp(ui, server)

