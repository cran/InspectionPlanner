
ui <- shiny::fluidPage(
  #' @importFrom shiny tags HTML
  #' @importFrom shiny observe showNotification updateNumericInput div h3 h4

    shiny::tags$style(HTML("

      #download_report_html {
        background-color: #d0e7ff;
        color: #000;
        border: 1px solid #ccc;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 14px;
        cursor: pointer;
        border-radius: 4px;
        transition-duration: 0.4s;
      }

      #download_report_html:hover {
        background-color: #a6c8ff;
        border: 1px solid #000;
      }

      .gray-background {
        background-color: #f2f2f2;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .table-title {
        font-size: 13px;
        font-family: Arial, sans-serif;
      }
      .pallet-title {
        font-size: 15px;
        font-weight: bold;
      }
      .body-title, .height-title {
        font-size: 18px;
        font-weight: bold;
      }
      .main-title {
        font-size: 26px;
        font-family: Arial, sans-serif;
         font-weight: bold;
      }
      .highlight {
        background-color: yellow !important;
      }
      .pallets_ui {
  page-break-inside: avoid;
  margin-bottom: 5mm;
}
 @media print {
    body, html {
      margin: 0;
      padding: 0;
      width: 100%;
      background: #FFF;
    }
    .shiny-output-error {
      display: none;
    }
    .shiny-output-error:before {
      display: none;
    }
    .content {
      margin: 10mm 15mm 10mm 15mm;
    }
    #download_report_html, .sidebar, .header, .footer {
      display: none; /* Oculta elementos que no necesitas imprimir */
    }
  }

    ")
  ),

  shiny::titlePanel(tags$h1("Inspection Planner", class="main-title")),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("sampling_method", "Sampling Method:",
                         choices = c("simple random sampling",
                                     "systematic sampling with random start")),
      shiny::numericInput("total_units", "Lot Size:", value = 216, min = 1, step = 1),
      shiny::numericInput("sample_size", "Sample size:", value = 15, min = 1, step = 1),
      shiny::numericInput("num_rows_per_pallet", "Number of rows per Pallet:", value = 6, min = 1, step = 1),
      shiny::numericInput("num_columns_per_pallet", "Number of columns per Pallet:", value = 6, min = 1, step = 1),
      shiny::numericInput("pallet_height", "Pallet height:", value = 6, min = 1, step = 1),
      shiny::downloadButton("download_report_html", "Generate HTML Report")
    ),
    shiny::mainPanel(
      shiny::titlePanel(tags$h1("Sampling Plan", class="body-title")),#
      shiny::textOutput("total_units_text"),
      shiny::textOutput("sample_size_text"),
      shiny::textOutput("num_pallets_text"),
      shiny::textOutput("num_rows_per_pallet_text"),
      shiny::textOutput("num_columns_per_pallet_text"),
      shiny::textOutput("pallet_height_text"),
      shiny::textOutput("selected_method"),
      shiny::uiOutput("pallets_ui")
    )
  )
)

server <- function(input, output, session) {
  # Function to check if a number is an integer
  is_integer <- function(x) {
    return(x %% 1 == 0)
  }
   # Observe and validate the inputs, ensuring certain inputs are integers
  observe({
    if (!is_integer(input$total_units)) {
      showNotification("Total units must be an integer.", type = "error")
      updateNumericInput(session, "total_units", value = floor(input$total_units))
    }
    if (!is_integer(input$sample_size)) {
      showNotification("sample size must be an integer.", type = "error")
      updateNumericInput(session, "sample_size", value = floor(input$sample_size))
    }

    if (!is_integer(input$num_rows_per_pallet)) {
      showNotification("Number of rows per Pallet must be an integer.", type = "error")
      updateNumericInput(session, "num_rows_per_pallet", value = floor(input$num_rows_per_pallet))
    }

    if (!is_integer(input$num_columns_per_pallet)) {
      showNotification("Number of columns per Pallet must be an integer.", type = "error")
      updateNumericInput(session, "num_columns_per_pallet", value = floor(input$num_columns_per_pallet))
    }

    if (!is_integer(input$pallet_height)) {
      showNotification("Height of the Pallet must be an integer.", type = "error")
      updateNumericInput(session, "pallet_height", value = floor(input$pallet_height))
    }
  })


  output$total_units_text <- shiny::renderText({
    paste("Total number of units:", input$total_units)
  })
  output$total_units_text <- shiny::renderText({
    paste("Total number of units:", input$total_units)
  })

  output$sample_size_text <- shiny::renderText({
    paste("Sample size:", input$sample_size)
  })

  output$num_pallets_text <- shiny::renderText({
    num_units_per_pallet <- input$num_rows_per_pallet * input$num_columns_per_pallet * input$pallet_height
    num_pallets <- ceiling(input$total_units / num_units_per_pallet)
    paste("Calculated number of Pallets:", num_pallets)
  })

  output$num_rows_per_pallet_text <- shiny::renderText({
    paste("Number of rows per Pallet:", input$num_rows_per_pallet)
  })

  output$num_columns_per_pallet_text <- shiny::renderText({
    paste("Number of columns per Pallet:", input$num_columns_per_pallet)
  })

  output$pallet_height_text <- shiny::renderText({
    paste("Pallet height:", input$pallet_height)
  })

  output$selected_method <- shiny::renderText({
    paste("Selected sampling method:", input$sampling_method)
  })

  pallets_html <- shiny::reactive({
    num_units_per_pallet <- input$num_rows_per_pallet * input$num_columns_per_pallet * input$pallet_height
    num_pallets <- ceiling(input$total_units / num_units_per_pallet)
    num_rows <- input$num_rows_per_pallet
    num_columns <- input$num_columns_per_pallet
    height <- input$pallet_height
    total_units <- input$total_units

    sampled_units <- NULL
    if (input$sampling_method == "simple random sampling") {
      sampled_units <- sample(1:total_units, input$sample_size)
    }
    else if (input$sampling_method == "systematic sampling with random start") {
      k <- floor(input$total_units / input$sample_size)
      start <- sample(1:k, 1)
      sampled_units <- seq(start, by = k, length.out =  input$sample_size)
      sampled_units <- sampled_units[sampled_units <= input$total_units]
    }

    # Function to create zigzag numbering for pallet design
    create_zigzag_numbering <- function(rows, cols) {
      mat <- matrix(1:(rows * cols), nrow = rows, byrow = TRUE)
      for (i in seq(2, nrow(mat), by = 2)) {
        mat[i, ] <- rev(mat[i, ])
      }
      return(as.vector(t(mat)))
    }


    htmlOutput <- lapply(1:num_pallets, function(pallet_num) {
      div(
        h3(class = "pallet-title", paste("Pallet", pallet_num)),
        div(
          lapply(1:height, function(h) {
            div(
              h4(class = "height-title", paste("Height", h)),
              div(
                style = paste("display: grid; grid-template-columns: repeat(", num_columns, ", 40px); grid-template-rows: repeat(", num_rows, ", 40px); gap: 10px; margin-bottom: 10px;"),
                lapply(1:(num_rows * num_columns), function(i) {
                  zigzag_num <- create_zigzag_numbering(num_rows, num_columns)
                  unit_num <- (pallet_num - 1) * num_units_per_pallet + (h - 1) * (num_rows * num_columns) + zigzag_num[i]
                  if (unit_num <= total_units) {
                    if (!is.null(sampled_units) && unit_num %in% sampled_units) {
                      div(class = "highlight", style = "border: 2px solid black; width: 40px; height: 40px; display: flex; justify-content: center; align-items: center; background-color: yellow;", unit_num)
                    } else {
                      div(style = "border: 2px solid gray; width: 40px; height: 40px; display: flex; justify-content: center; align-items: center;", unit_num)
                    }
                  } else {
                    div(style = "border: 2px solid gray; width: 40px; height: 40px;")
                  }
                })
              ),
              style = "margin-right: 20px; display: inline-block; vertical-align: top;"
            )
          }),
          style = "display: inline-block; vertical-align: top; margin-right: 40px;"
        ),
        style = "margin-bottom: 40px;"
      )
    })

    do.call(shiny::tagList, htmlOutput)
  })

  output$pallets_ui <- shiny::renderUI({
    pallets_html()
  })

  generate_report <- function(file) {
    num_units_per_pallet <- input$num_rows_per_pallet * input$num_columns_per_pallet * input$pallet_height
    num_pallets <- ceiling(input$total_units / num_units_per_pallet)

    report_data <- data.frame(
      `Field` = c("Total number of units", "Sample size", "Number of Pallets", "Number of rows per Pallet", "Number of columns per Pallet", "Pallet height", "Sampling method"),
      `Value` = c(input$total_units, input$sample_size, num_pallets, input$num_rows_per_pallet, input$num_columns_per_pallet, input$pallet_height, input$sampling_method)
    )

    temp_report <- tempfile(fileext = ".Rmd")
    pallets_html_file <- tempfile(fileext = ".html")
    htmltools::save_html(pallets_html(), pallets_html_file)
    pallets_html_file <- normalizePath(pallets_html_file, winslash = "/")

    report_content <- paste0(
      "---\n",
      "title: 'Sampling Plan Report'\n",
      "output: rmarkdown::html_document\n",
      "---\n\n",
      "<div style='font-size: 16px;'>\n",
      "Date: ", Sys.Date(), "          Time: ", format(Sys.time(), "%H:%M:%S"), "\n",
      "</div>\n\n",
      "<button onclick=\"window.print()\">Print Report</button>\n\n",
      "<style>\n",
      "@media print {\n",
      "  .highlight { background-color: yellow !important; }\n",
      "  * {-webkit-print-color-adjust: exact;}\n",
      "}\n",
      "</style>\n",
      "```{r echo=FALSE, results='asis', warning=FALSE}\n",
      "knitr::kable(report_data)\n",
      "```\n",
      "```{r echo=FALSE, warning=FALSE}\n",
      "htmltools::includeHTML('", pallets_html_file, "')\n",
      "```\n"
    )


    writeLines(report_content, temp_report)
    rmarkdown::render(temp_report, output_file = file)
  }

  output$download_report_html <- shiny::downloadHandler(
    filename = function() {
      paste("report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      generate_report(file)
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
