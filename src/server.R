library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(DT)
library(sf)
library(leaflet)

function(input, output, session)
{
  path <- getwd()
  db <- try(dbConnect(SQLite(), dbname = file.path(path, "../data/Divvy.sqlite")))

  session$onSessionEnded(function() {
    dbDisconnect(db)
    stopApp()
  })

  output$avg_duration <- reactive({
    db %>%
      tbl("trips") %>%
      filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
      select(duration) %>%
      summarize(avg = round(mean(duration, na.rm = TRUE), 2)) %>%
      as_tibble() %>%
      seconds_to_period()
  })

  output$max_duration <- reactive({
    db %>%
      tbl("trips") %>%
      filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
      select(duration) %>%
      summarize(max = max(duration, na.rm = TRUE)) %>%
      as_tibble() %>%
      seconds_to_period() %>%
      toString()
  })

  output$mode_duration <- reactive({
    db %>%
      tbl("trips") %>%
      filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
      select(week_day) %>%
      as_tibble() %>%
      mutate(week_day = wday(week_day, label = TRUE, abbr=FALSE)) %>%
      group_by(week_day) %>%
      summarize(n = n()) %>%
      slice_max(n) %>%
      .$week_day %>%
      toString()
  })

  output$weekdays_pivottable <- db %>%
    tbl("trips") %>%
    filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
    select(week_day, duration, user_type) %>%
    mutate(user_type = case_match(user_type , 0 ~ "Customer", 1 ~ 'Subscriber', 2 ~ "Dependent")) %>%
    as_tibble() %>%
    mutate(week_day =  fct_relevel(wday(week_day, label = TRUE, abbr = FALSE))) %>%
    group_by(week_day, user_type) %>%
    summarize(.groups = "keep", "Number of trips" =  n(), "Average trip duration" = mean(duration, na.rm = TRUE)) %>%
    rename("Day of the week" = week_day, "User type" = user_type) %>%
    renderDT(caption = "User type and week day pivot table", filter = 'top', options = list(pageLength = 21))

  output$weekdays_barplot <- renderPlot({
    ggplot(
      db %>%
        tbl("trips") %>%
        filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
        select(week_day, user_type, gender) %>%
        as_tibble() %>%
        mutate(week_day = fct_relevel(wday(week_day, label = TRUE, abbr = FALSE)),
               user_type=case_match(user_type , 0 ~ "Customer", 1 ~ 'Subscriber', 2 ~ "Dependent"),
               gender=case_match(gender , 0 ~ "Unknown", 1 ~ 'Male', 2 ~ "Female", 9 ~ "Not applicable")) %>%
        group_by(gender, week_day, user_type) %>%
        summarise(n = n(), .groups = "keep"),
      aes(x = week_day, y = n ,fill = user_type)) +
      geom_bar(stat = 'identity', position = "stack") +
      facet_wrap(~gender) +
      labs(fill = 'User type') +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
      xlab("Day of the week") +
      ylab("Number of trips")
  })

  output$trips_timeseries <- renderPlot({
    ggplot(
      db %>%
        tbl("trips") %>%
        filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
        select(user_type, start_time) %>%
        as_tibble() %>%
        mutate(
          start_time = date(as.POSIXct((start_time))),
          user_type = case_match(user_type , 0 ~ "Customer", 1 ~ 'Subscriber', 2 ~ "Dependent")),
      aes(x = start_time, group = user_type, fill = user_type)) +
      geom_density(alpha = 0.5) +
      labs(fill = 'User type') +
      xlab("Date") +
      ylab("Number of trips") +
      scale_y_continuous()
  })

  output$gender_piechart <- renderPlot({
    ggplot(
      db %>%
        tbl("trips") %>%
        filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
        select(user_type, gender) %>%
        group_by(user_type, gender) %>%
        summarise(Number = n(), .groups = "keep") %>%
        as_tibble() %>%
        mutate(
          user_type=case_match(user_type , 0 ~ "Customer", 1 ~ 'Subscriber', 2 ~ "Dependent"),
          gender=case_match(gender , 0 ~ "Unknown", 1 ~ 'Male', 2 ~ "Female", 9 ~ "Not applicable")),
      aes(x = "", y = user_type, fill = user_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      geom_text(aes(x = "", y = user_type, label = gender), position = position_stack(vjust = 0.5)) +
      labs(fill = 'User type')
  })

  output$age_piechart <- renderPlot({
    ggplot(
      db %>%
        tbl("trips") %>%
        filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2])) & birth_year > 0) %>%
        select(birth_year, start_time, user_type) %>%
        as_tibble() %>%
        mutate(
          user_type=case_match(user_type , 0 ~ "Customer", 1 ~ 'Subscriber', 2 ~ "Dependent"),
          age = year(as.POSIXct(start_time)) - birth_year,
          cat = case_when(
            age < 1 ~ "Infant (less than 1)",
            age >= 1 & age <= 12 ~ "Child (1-12)",
            age >= 13 & age <= 17 ~ "Adolescent (13-17)",
            age >= 18 & age <= 65 ~ "Adult (18-65)",
            age > 65 ~ "Elderly (65+)")) %>%
        group_by(user_type, cat) %>%
        summarise(Number = n(), .groups = "keep"),
      aes(x = "", y = cat, fill = cat)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      geom_text(aes(x = "", y = cat, label = user_type), position = position_stack(vjust = 0.5)) +
      labs(fill = 'Age category')
  })

  stations <- reactive({
    bind_rows(
      db %>%
        tbl("trips") %>%
        filter(between(start_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
        select(from_station_id, start_time) %>%
        rename(id = from_station_id, time=start_time) %>%
        as_tibble(),
      db %>%
        tbl("trips") %>%
        filter(between(end_time, unixepoch(!!input$date[1]), unixepoch(!!input$date[2]))) %>%
        select(to_station_id, end_time)%>%
        rename(id = to_station_id, time=end_time) %>%
        as_tibble()
    ) %>%
      group_by(id) %>%
      summarise(freq = n(), .groups = "keep") %>%
      inner_join(
        db %>%
          tbl("stations") %>%
          select(id, name, status, longitude, latitude) %>%
          as_tibble(),
        by = "id"
      ) %>%
      as_tibble() %>%
      st_as_sf(coords = c("longitude", "latitude"), remove = TRUE)
  })

  col <- colorNumeric(palette = "YlOrRd", domain = NULL)

  output$map_plot <- renderLeaflet({
    leaflet(stations()) %>%
      addTiles() %>%
      addCircleMarkers(
        label = ~stations()$name,
        color = ~col(stations()$freq),
        stroke = ~ifelse(stations()$status,TRUE, FALSE),
        fillOpacity = 0.8) %>%
      addLegend("bottomright",
                pal = col,
                values = ~stations()$freq,
                title = 'Chicago Divvy station usage')
  })

  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = list(date = input$date,
                                      path = path),
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
