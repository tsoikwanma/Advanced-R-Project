# Advanced Programming in R [2400-DS1APR] Final Project by:
# I Putu Agastya Harta Pratama - 472876 - i.pratama@student.uw.edu.pl
# Tsoi Kwan Ma - 476914 - t.ma@student.uw.edu.pl



server <- function(input, output, session) {
  
  
  # --------------------Data Validation-----------------------------
  data_applied <- reactiveVal(FALSE)
  upload_error <- reactiveVal(NULL)
  
  observeEvent(input$uploadFile, {
    data_applied(FALSE)
    upload_error(NULL)
  })
  
  emission_data_active <- reactive({
    if (data_applied()) {
      # user has clicked “Apply Data” and validation passed
      uploaded_data()
    } else {
      # fall back to the original CSV
      emission_data
    }
  })

  # --------------------Input React from User-----------------------------
  
  # 1) Populate overviewCountry dropdown
  observe({
    df0 <- emission_data_active()
    countries <- sort(unique(df0$country))
    updateSelectInput(session, "overviewCountry",
                      choices  = c("Total", countries),
                      selected = "Total")
  })
  
  # 2) Populate sector‐tab inputs
  observe({
    df0 <- emission_data_active()
    updateSelectInput(session, "country_sector_tab",
                      choices  = c("Total", sort(unique(df0$country))),
                      selected = "Total")
    updateSelectInput(session, "sector_select",
                      choices  = c("All sectors", sort(unique(df0$sector))),
                      selected = "All sectors")
    
    available_years <- sort(unique(df0$year))
    min_year <- min(available_years)
    max_year <- max(available_years)
    updateSliderInput(session, "yearRangeSector",
                      min = min_year,
                      max = max_year,
                      value = c(min_year, max_year))
  })
  
  # 3) Populate second sector‐tab country
  observe({
    df0 <- emission_data_active()
    updateSelectInput(session, "country_sector_tab_2",
                      choices  = c("Total", sort(unique(df0$country))),
                      selected = "Total")
    
    available_years <- sort(unique(df0$year))
    min_year <- min(available_years)
    max_year <- max(available_years)
    updateSliderInput(session, "yearRangeSector_2",
                      min = min_year,
                      max = max_year,
                      value = c(min_year, max_year))
  })
  
  # 3) Raw Data Filters
  observe({
    df0 <- emission_data_active()
    updateSelectInput(session, "filter_sector",
                      choices  = sort(unique(df0$sector)),
                      selected = sort(unique(df0$sector))[1])
    updateSelectInput(session, "filter_country",
                      choices  = sort(unique(df0$country)),
                      selected = sort(unique(df0$country))[1])
    updateSelectInput(session, "filter_year",
                      choices  = sort(unique(df0$year)),
                      selected = sort(unique(df0$year))[1])
  })
  
  # 4) Year Selector Filtering for Pie Chart
  observe({
    df0 <- emission_data_active()
    available_years <- sort(unique(df0$year))
    min_year <- min(available_years)
    max_year <- max(available_years)
    
    updateSliderInput(session, "filter_year_pie",
                      min   = min_year,
                      max   = max_year,
                      value = max_year,
                      step  = 1)
   })
  
  # --------------------New CSV File Upload & Validation-----------------------------
  uploaded_data <- eventReactive(input$validateUpload, {
    req(input$uploadFile)
    upload_error(NULL)  
    
    # 1) Safe read
    df <- tryCatch(
      read.csv(input$uploadFile$datapath, stringsAsFactors = FALSE),
      error = function(e) {
        upload_error(paste("Could not read CSV:", e$message))
        return(NULL)
      }
    )
    if (is.null(df)) return(NULL)
    
    # 2) Column checks
    required_cols <- c("National.accounts.indicator..ESA.2010.","Statistical.classification.of.economic.activities.in.the.European.Community..NACE.Rev..2.","Country.of.origin","TIME_PERIOD", "OBS_VALUE")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols)) {
      upload_error(paste("Missing required column(s):", paste(missing_cols, collapse=", ")))
      return(NULL)
    }
    
    df <- df %>%
      filter(National.accounts.indicator..ESA.2010. == "Total") %>%
      rename(
        indicator = National.accounts.indicator..ESA.2010.,
        sector = Statistical.classification.of.economic.activities.in.the.European.Community..NACE.Rev..2.,
        country = Country.of.origin,
        year = TIME_PERIOD,
        emission = OBS_VALUE
      ) %>%
      select(sector, country, year, emission)
    
    
    # 3) Year Check
    if (!is.numeric(df$year)) {
      upload_error("Column 'year' must be numeric.")
      return(NULL)
    }
    if (any(df$year < 2010 | df$year > 2022, na.rm = TRUE)) {
      upload_error("All 'year' values must be between 2010 and 2022.")
      return(NULL)
    }
    
    # 4) Country 
    invalid_ctry <- setdiff(unique(df$country), eu_members)
    if (length(invalid_ctry)) {
      upload_error(paste("These countries are not in the allowed list:", 
                         paste(invalid_ctry, collapse=", ")))
      return(NULL)
    }
    
    # 5) Passed all checks!
    df_clean <- df
    colnames(df_clean) <- c("sector", "country", "year", "emission")
    df_clean
  }, ignoreNULL = FALSE)
  
  # Only show Apply-Data button when validation succeeded
  output$applyDataUI <- renderUI({
    req(uploaded_data())   # only if uploaded_data() is non-NULL
    actionButton("applyData", "Apply Data", icon = icon("play"),
                 class = "btn btn-success", style="margin-left:1em;")
  })
  
  observeEvent(input$applyData, {
    req(uploaded_data())           
    data_applied(TRUE)
    showNotification("Uploaded data is now available.", type = "message")
  })
  
  # Metadata preview (unchanged)
  output$uploadMeta <- renderUI({
    req(uploaded_data(), !data_applied())
    df <- uploaded_data()
    tags$div(
      style="margin-bottom:1em;",
      tags$p(
        strong("Rows:"), nrow(df), br(),
        strong("Sectors:"), paste(unique(df$sector), collapse=", "), br(),
        strong("Countries:"), paste(unique(df$country), collapse=", "), br(),
        strong("Years:"), paste0(min(df$year), "–", max(df$year))
      ), hr()
    )
  })
  
  # Error/info/success messages
  output$uploadMessage <- renderUI({
    req(input$validateUpload > 0)
    
    if (!is.null(upload_error())) {
      tags$div(class="alert alert-danger",
               strong("Error: "), upload_error())
      
    } else if (!data_applied()) {
      tags$div(class="alert alert-info",
               "Upload validated — you can now ", strong("Apply Data"),
               " to switch the dashboard to your file.")
      
    } else {
      tags$div(class="alert alert-success",
               "Using your uploaded dataset for all views.")
    }
  })

  # --------------------Overview Plot-----------------------------
  output$overviewPlot <- renderPlot({
    df0 <- emission_data_active()
    req(input$overviewCountry)

    if (input$overviewCountry == "Total") {
      yearly <- df0 %>%
        group_by(year) %>%
        summarise(total_emission = total_emission(emission))  # C++ function

      ggplot(yearly, aes(x = year, y = total_emission)) +
        geom_area(fill = "#78c2ad", alpha = 0.3) +        
        geom_line(color = "#78c2ad", size = 2) +         
        scale_x_continuous(breaks = 2010:2022) +
        scale_y_continuous(labels = scales::comma) +      
        labs(title = "Development of Total Carbon Emissions",
             x = "Year", y = "Emissions (CO2e by thousand tonnes)") +
        theme_minimal() +
        theme(
          panel.grid     = element_blank(),
          plot.title     = element_text(face = "bold", size = 16),
          axis.text.x    = element_text(size = 10),
          axis.text.y    = element_text(
            size   = 10,
            margin = margin(r = 10)       
          ),
          axis.title.y   = element_text(margin = margin(r = 12)),
          plot.margin    = margin(t = 5, r = 5, b = 5, l = 15)  
        )

    } else {
      country_data <- df0 %>%
        filter(country == input$overviewCountry) %>%
        group_by(year) %>%
        summarise(total_emission = total_emission(emission))  # C++

      ggplot(country_data, aes(x = year, y = total_emission)) +
              geom_area(fill = "#78c2ad", alpha = 0.3) +      
              geom_line(color = "#78c2ad", size = 2) +        
              scale_x_continuous(breaks = 2010:2022) +
              scale_y_continuous(labels = scales::comma) +    
        labs(title = paste("Development of Carbon Emissions for", input$overviewCountry),
             x = "Year", y = "Emissions (CO2e by thousand tonnes)") +
        theme_minimal() +
        theme(
          panel.grid     = element_blank(),
          plot.title     = element_text(face = "bold", size = 16),
          axis.text.x    = element_text(size = 10),
          axis.text.y    = element_text(
            size   = 10,
            margin = margin(r = 10)
          ),
          axis.title.y   = element_text(margin = margin(r = 12)),
          plot.margin    = margin(t = 5, r = 5, b = 5, l = 15)
        )
    }
  })


  # --------------------Europe Map-----------------------------
  map_data_active <- reactive({
    df0 <- emission_data_active()
    total_by_country <- df0 %>%
      group_by(country) %>%
      summarise(
        total_emission = total_emission(emission),
        .groups        = "drop"
      )
    
    mismatch <- setdiff(total_by_country$country, eu_sf$admin)
    if (length(mismatch)) {
      warning("Shapefile mismatch: ", paste(mismatch, collapse = ", "))
    }
    
    eu_sf %>%
      left_join(total_by_country, by = c("admin" = "country"))
  })
  
  output$overviewMap <- renderLeaflet({
    md  <- map_data_active()   # reactive sf with total_emission
    pal <- colorNumeric(
      palette = brewer.pal(4, "Blues"),
      domain  = md$total_emission,
      na.color= "gray90"
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = world_sf, fillColor = "grey90", fillOpacity = 0.5, weight = 1, color = "white") %>%
      addPolygons(
        data        = md,
        fillColor   = ~pal(total_emission),
        fillOpacity = 0.8,
        weight      = 1,
        color       = "white",
        dashArray   = "3",
        label       = ~paste0(admin, ": ", scales::comma(total_emission)),
        highlightOptions = highlightOptions(
          weight       = 3, color = "#666",
          dashArray    = "", fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = md$total_emission,
        position = "bottomright",
        title    = "Total CO₂e"
      ) %>%
      setView(lng = 10, lat = 52, zoom = 4)
  })


  # --------------------Line Plot by Sector or Total-----------------------------
  output$linePlotSector <- renderPlot({
    df0 <- emission_data_active()
    req(input$country_sector_tab, input$yearRangeSector, input$sector_select)

    data_filtered <- df0 %>%
      filter(
        (input$country_sector_tab == "Total" | country == input$country_sector_tab),
        year >= input$yearRangeSector[1],
        year <= input$yearRangeSector[2]
      )

    y_range <- input$yearRangeSector

    if (input$sector_select == "All sectors") {
      df <- data_filtered %>%
        group_by(year, sector) %>%
        summarise(total = sum(emission, na.rm = TRUE), .groups = "drop")

      label_vector <- setNames(
        sub(";.*", "", unique(df$sector)),   
        unique(df$sector)                    
      )

      ggplot(df, aes(x = year, y = total, color = sector)) +
        geom_line(size = 0.8) +
        scale_x_continuous(breaks = y_range[1]:y_range[2]) +
        scale_y_continuous(labels = scales::comma) + 
        labs(
          title = paste("Carbon Emissions by Sector for", input$country_sector_tab),
          x = "Year", y = "Emissions (CO2e by thousand tonnes)", color = NULL  
        ) +
        guides(color = guide_legend(ncol = 4)) +  
        scale_color_discrete(labels = label_vector) + 
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.text = element_text(size = 9)
        )
      
    } else {
      
      df <- data_filtered %>%
        filter(sector == input$sector_select) %>%
        group_by(year) %>%
        summarise(total = sum(emission, na.rm = TRUE), .groups = "drop")

      short_sector <- sub(";.*", "", input$sector_select)

      ggplot(df, aes(x = year, y = total)) +
        geom_line(color = "darkblue", size = 0.8) +
        scale_x_continuous(breaks = y_range[1]:y_range[2]) +
        scale_y_continuous(labels = scales::comma) +
        labs(
          title = paste("Trend Emissions for", short_sector, "in", input$country_sector_tab),
          x = "Year", y = "Emissions (CO2e by thousand tonnes)"
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 16),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)
        )
    }
  })

  # --------------------Bar Plot by Sector-----------------------------
  output$barPlotSector <- renderPlot({
    df0 <- emission_data_active()
    req(input$country_sector_tab_2, input$yearRangeSector_2)
    
    year_start <- input$yearRangeSector_2[1]
    year_end   <- input$yearRangeSector_2[2]
    
    data_filtered <- df0 %>%
      filter(
        (input$country_sector_tab_2 == "Total" | country == input$country_sector_tab_2),
        year %in% c(year_start, year_end)
      )
    
    df_wide <- data_filtered %>%
      group_by(sector, year) %>%
      summarise(total = sum(emission, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from  = year,
        values_from = total,
        values_fill = list(total = 0)
      )

    df_wide <- df_wide %>%
      rename(
        start = as.character(year_start),
        end   = as.character(year_end)
      )
    
    df_wide$change <- emission_change(df_wide$start, df_wide$end) # C++ function
    
    df_wide$label <- sub(";.*", "", df_wide$sector)
    
    ggplot(df_wide, aes(x = change, y = label, fill = change > 0)) +
      geom_col(show.legend = FALSE) +
      geom_vline(xintercept = 0, color = "gray40", linetype = "dashed") +
      scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
      labs(
        title = paste(
          "Change in Emissions by Sector between",
          year_start, "and", year_end, "for", input$country_sector_tab_2
        ),
        x = "Change in Emissions (CO2e by thousand tonnes)",
        y = "Sector"
      ) +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(
        panel.grid  = element_blank(),
        plot.title  = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
  })
  


  # --------------------Data Table for Exporting-----------------------------
  filtered_data <- reactive({
    df0 <- emission_data_active()
    req(input$filter_sector, input$filter_country, input$filter_year)

    df <- df0 %>%
      filter(
        sector %in% input$filter_sector,
        country %in% input$filter_country,
        year %in% input$filter_year
      )

    validate(need(nrow(df) > 0, "No matching data. Try different filters."))

    df
  })






  # --------------------Pie Chart-----------------------------
  pie_data <- reactive({
    df0 <- emission_data_active()
    req(input$filter_year_pie)
    df <- df0 %>%
      filter(year == input$filter_year_pie) %>%
      group_by(country) %>%
      summarise(total_emission = total_emission(emission), .groups = "drop")
    validate(
      need(nrow(df) > 0, "No data for this year.")
    )
    
    top_n <- df %>% arrange(desc(total_emission)) %>% slice(1:10)
    others <- df %>% arrange(desc(total_emission)) %>% slice(-1:-10)
    df <- bind_rows(top_n, tibble(country="Other", total_emission=sum(others$total_emission)))
    df
  })
  
  output$pieYearUI <- renderUI({
    yrs <- sort(unique(emission_data_active()$year))
    sliderInput(
      "filter_year_pie",         
      "Select Year:",
      min   = yrs[1],
      max   = yrs[length(yrs)],
      value = yrs[length(yrs)],
      step  = 1,
      sep   = "",         
      width = "100%"
    )
  })
  
  output$piePlotly <- renderPlotly({
    df <- pie_data()

    # Compute percentages
    df <- df %>%
      mutate(pct = total_emission / sum(total_emission) * 100)

    plot_ly(
      data = df,
      labels = ~country,
      values = ~total_emission,
      type   = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      hoverinfo = 'label+percent+value',
      marker = list(line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        title = paste0('Emissions Composition in ', input$filter_year_pie),
        legend = list(orientation = 'v', x = 1.0, y = 0.5)
      )
  })

  
  # --------------------Data Table for Exporting-----------------------------
  output$tab <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 15))
  })


  observeEvent(input$save, {
    req(input$file_name)
    filename <- paste0(input$file_name, ".csv")
    write.csv(filtered_data(), filename, row.names = FALSE)
    showNotification(paste("Saved:", filename), type = "message")
  })
}
