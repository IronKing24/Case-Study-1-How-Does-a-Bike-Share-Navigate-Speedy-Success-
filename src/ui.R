library(shiny)
library(DT)
library(leaflet)

fluidPage(
  titlePanel("How Does a Bike-Share Navigate Speedy Success?"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date", label = "Date range",start = "2019-12-01", end = "2019-12-31", min = "2013-01-01", max = "2019-12-31", format = "yyyy/mm/dd", startview = "year", separator = "-"),
      downloadButton("report", "Generate report")
    ),
    mainPanel(
      h2("Ask"),
      h3("Business task"),
      p("Find the matching patterns between members and none members to influence the new marketing strategy that is aimed to increase members count."),
      h2("Prepare"),
      p("The trips and stations data was located at ", 
        a("Divvy's historical data", href = "https://www.google.com/"), 
        " released under the",
        a("data license agreement", href = "https://divvybikes.com/data-license-agreement")),
      h2("Process"),
      tags$details(
        tags$summary("Data change log"),
        tags$ol(
          tags$li("The trips data files from the year 2013 up to the year 2019 where merged under a single file."),
          tags$li("The columns of the station names where removed."),
          tags$li("The column headers' names were unified to (id, start_time, stop_time, from_station_id, to_station_id, user_type, gender, birth_year)."),
          tags$li("The genders where changed for 0 for null, 1 for male, and 2 for female."),
          tags$li("The the customers value was changed to 0, the subscribers value was changed to 1, and the dependent value was changed to 2."),
          tags$li("Removed trips with the same id."),
          tags$li("Removed trips with negative duration.")
        )
      ),
      h2("Analyze"),
      p("The mean ride duration is ", textOutput("avg_duration", inline = T), "."),
      p("The maximum trip duration is ", textOutput("max_duration", inline = T), "."),
      p("The mode of the week day is ", textOutput("mode_duration", inline = T), "."),
      DTOutput("weekdays_pivottable"),
      h2("Share"),
      plotOutput("weekdays_barplot"),
      plotOutput("trips_timeseries"),
      plotOutput("gender_piechart"),
      plotOutput("age_piechart"),
      leafletOutput("map_plot"),
      h2("Act"),
      tags$ol(
        tags$li("None subscribers tend to take trips around the holiday building a subscription plan tailored to their needs might convince them to buy that plan."),
        tags$li("None subscribers tend to have an average trip length than subscribers having a subscription reward program might incentive's them to subscribe to accumulate points."),
        tags$li("Users between 13-17 don't tend to use bikes compared to other age groups, doing a special subscription for under aged users would tend to attract more subscribes of the a age group and they might keep being users when they grow up."),
      )
    )
  )
)