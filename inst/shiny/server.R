shinyServer(function(session, input, output) {
    values <- reactiveValues()
    observeEvent(req(input$loadedTable), {
        file <- input$loadedTable$datapath
        ext <- tolower(tools::file_ext(file))
        if(ext %in% c("csv", "txt")){
            tab <- import_delim(file)
        } else if (grepl("xls", ext)){
            tab <- readxl::read_excel(file, sheet = 1, guess_max = 10000, .name_repair = "minimal")
        }
        values$tab <- tab

    })

    output$initialTable <- DT::renderDataTable({
        values$tab
    },options = list(searching = FALSE))

    output$showTable <- renderUI({
        req(values$tab)
        tagList(box(
            title = "Initial Table",
            width = 12,
            DT::dataTableOutput("initialTable")
        ))
    })

    output$config <- renderUI({
        tagList(
            box(title = "Bootstrap Configuration",
                numericInput("seed", "Seed", value = 1),
                numericInput("nrepli", "Number of replications", value = 1000),
                textInput("costUnit", "Cost unit", "€"),
                textInput("effUnit", "Effectiveness Unit", "QALY"),
                radioButtons("sep1000", "Thousands Separator", choices = c("Comma" = ",", "Space" = " ", "None" = "")),
                uiOutput("startBoot")
            )
        )
    })

    output$startBoot <- renderUI({
        req(values$tab)
        tagList(
            selectInput("reference", "Reference Strategy", unique(values$tab$strategy)),
            span(actionButton("start", "Start Bootstrap"), uiOutput("loading", inline = TRUE))
        )
    })

    observeEvent(values$tab, {
        if (!all(c("strategy", "effect", "cost") %in% names(values$tab))){
            create_alert("Your column names must include at least 'cost', 'effect' and 'strategy' (lower case)")
            values$tab <- NULL
            return(NULL)
        }
        if(any(is.na(values$tab$cost))){
            create_alert("Missing values in 'cost' column")
            values$tab <- NULL
        }
        if(any(is.na(values$tab$cost))){
            create_alert("Missing values in 'effect' column")
            values$tab <- NULL
        }
        if(any(is.na(values$tab$strategy))){
            create_alert("Missing values in 'strategy' column")
            values$tab <- NULL
        }
    })

    observeEvent(input$start, {
        updateTabItems(session, "menu", "results")
        shinyjs::show("spinner")
        values$means <- values$tab %>%
            get_resamples(n = input$nrepli, seed = input$seed) %>%
            get_means()
        shinyjs::hide("spinner")
    })

    output$loading <- renderUI({
        hidden(span(id = "spinner", icon(name = "spinner", class = "fa-pulse fa-2x")))
    })

    observe({
        req(values$means)
        values$differences <- get_differences(values$means, reference = input$reference)
    })

    output$plotCE <- renderPlot({
        req(values$means)
        (values$plotCE <- plot_ce(values$differences,
                unit_x = input$effUnit, unit_y = input$costUnit, sep1000 = input$sep1000))
    })

    output$plotAC <- renderPlot({
        req(values$means)
        (values$plotAC <- plot_ac(values$means, unit = paste(input$costUnit, input$effUnit, sep = "/"),
                sep1000 = input$sep1000))
    })

    output$options <- renderUI({
        tagList(
            selectInput("formatImg", "Figure extension", choices = c("jpg", "png", "pdf", "tiff", "eps" , "svg"), ifelse(!is.null(input$formatImg), input$formatImg, "png")),#,
            numericInput("dpi", gettext("Résolution (DPI)"), value = ifelse(!is.null(input$dpi), input$dpi, 300)),
            numericInput("height", gettext("Height (cm)"), value = ifelse(!is.null(input$height), input$height, 15))
        )
    })

    output$allResults <- renderUI({
        req(values$means, values$differences)
        tagList(
            fluidRow(
                box(plotOutput("plotCE")),
                box(plotOutput("plotAC"))
           ),
           fluidRow(
               box(
                uiOutput("options"),
                downloadButton("download", "Download Figures")
               ),
               box(
                tableOutput("quadrants"),
                title = "Proportion in each quadrant"
               )
           )
        )
    })

    output$quadrants <- renderTable({
            get_quadrant_prop(values$differences,
                              strategy = setdiff(unique(values$tab$strategy),  input$reference))
    })

    output$download <- downloadHandler(
        filename = function() {
            gettextf('figures %s.zip', Sys.Date())
        },
        content = function(file) {
            path <- file.path(tempdir(), "export")
            dir.create(path)
            plotCE <- values$plotCE
            plotAC <- values$plotAC
            saveCE <- export_image(path, input$formatImg, force_range(input$dpi, 72, 1200),
                                    force_range(input$height, 5, 20), plotCE)
            saveAC <- export_image(path, input$formatImg, force_range(input$dpi, 72, 1200),
                                   force_range(input$height, 5, 20), plotAC)
            zip(file, path, flags = "-jrm")
        }
    )

})
