library(shiny)
library(booticer)

shinyServer(function(session, input, output) {
    values <- reactiveValues()
    observeEvent(req(input$loadedTable), {
        file <- input$loadedTable$datapath
        ext <- tolower(tools::file_ext(file))
        if(ext %in% c("csv", "txt")){
            tab <- import_delim(file)
        } else if (substr(ext, 3) == "xls"){
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
            if (!is.null(values$tab)){
                tagList(
                    selectInput("reference", "Reference Strategy", unique(values$tab$strategy)),
                    span(actionButton("start", "Start Bootstrap"), uiOutput("loading", inline = TRUE))
                )
            }
            )
        )

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

    output$plotCE <- renderPlot({
        req(values$means)
        plot_ce(values$means %>% get_differences(reference = input$reference),
                unit_x = input$effUnit, unit_y = input$costUnit, sep1000 = input$sep1000)
    })

    output$plotAC <- renderPlot({
        req(values$means)
        plot_ac(values$means, unit = paste(input$costUnit, input$effUnit, sep = "/"),
                sep1000 = input$sep1000)
    })

    output$allResults <- renderUI({
        req(values$means)
        tagList(
            fluidRow(
                box(plotOutput("plotCE")),
                box(plotOutput("plotAC"))
               # box(plot_ce(values$means %>% get_differences(reference = input$reference)))
           ),
           fluidRow(
               actionButton("download", "DownloadFigures")
           )
        )
    })

    output$download <- downloadHandler(
        filename = function() {
            gettextf('figures %s.zip', Sys.Date())
        },
        content = function(file) {
            saveImg <- export_image(path, input$formatImg, borner(input$dpi, 72, 1200), borner(input$hauteur, 5, 100),
                                    valeurs$savePlotDesc, plot_desc, valeurs$savePlotBivar, plot_bivar)
            zip(file, c(paste0(path, "/analyses.Rmd"), sprintf("%s/fichier.%s", path, tools::file_ext(valeurs$fichier$name))), flags = "-j")
            unlink(path, recursive = TRUE)
        }
    )

})
