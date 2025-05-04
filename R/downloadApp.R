#' Download App
#'
#' @param id identifier for shiny reactive
#' @param prefix static prefix for filename
#' @param main_par input parameters from calling routine
#' @param download_list reactiveValues with  postfix,plot,table
#' @return nothing 
#'
#' @importFrom shiny column downloadButton downloadHandler fluidRow
#'             moduleServer NS radioButtons reactive renderUI req
#'             textAreaInput uiOutput
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf
#' @export
downloadApp <- function(id) {
  source("R/plot_null.R")
  ui <- bslib::page_sidebar(
    title = "Test Download",
    sidebar = bslib::sidebar("side_panel",
      downloadInput("download"), # plot_table, preset
      downloadOutput("download") # downloadButton, filename
    ),
    bslib::card(
      bslib::card_header("Download Preview"),
      downloadUI("download")
    )
  )
  server <- function(input, output, session) { 
    # Test sets
    prefix <- "Download"
    download_list <- shiny::reactiveValues(
      filename = "panelID_instanceID",
      plots = shiny::reactiveValues(
        none = shiny::reactive(plot_null("nothing")),
        some = shiny::reactive(plot_null("something"))),
      tables = shiny::reactiveValues(
        twelve = shiny::reactive(matrix(1:12,nrow=3)),
        twenty = shiny::reactive(matrix(1:20,nrow=4)))
    )
    downloadServer("download", download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, download_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$download_list <- shiny::renderUI({
      shiny::tagList(
        "filename",
        download_list$filename,
        shiny::br(),
        "plots",
        paste(names(download_list$plots), collapse = ", "),
        shiny::br(),
        "tables", 
        paste(names(download_list$tables), collapse = ", "),
        switch(shiny::req(input$plot_table),
         # ** this only shows last plot **
          Plots  = shiny::plotOutput(ns("preview_plots")),
          Tables = DT::dataTableOutput(ns("preview_table")))
      )
    })
    chosen_plot <- shiny::reactive({
      plot <- shiny::req(input$plot_choice)
      shiny::req(download_list$plots[[plot]]())
    })
    output$preview_plots <- shiny::renderPlot({
      print(shiny::req(chosen_plot()))
    })
    chosen_table <- shiny::reactive({
      table <- shiny::req(input$table_choice)
      shiny::req(download_list$tables[[table]]())
    })
    output$preview_table <- DT::renderDataTable({
      shiny::req(chosen_table())
    })
    output$choices <- shiny::renderUI({
      switch(shiny::req(input$plot_table),
        Plots = {
          list(
            shiny::selectInput(ns("plot_choice"), "Plots:", NULL),
            shiny::selectInput(ns("png_pdf"), "",
              choices = c("PNG","PDF")),
            shiny::selectInput(ns("preset"), "Plot preset ratio:",
              choices = c("1to1","3to2","16to9"))
          )
        },
        Tables = {
          shiny::selectInput(ns("table_choice"), "Table:", NULL)
        })
      
    })
    shiny::observeEvent(shiny::req(input$plot_table, download_list$plots), {
      choices = names(download_list$plots)
      shiny::updateSelectInput(session, "plot_choice", 
        choices = choices, selected = NULL)
    })
    shiny::observeEvent(shiny::req(input$plot_table, download_list$tables), {
      choices = names(download_list$tables)
      shiny::updateSelectInput(session, "table_choice",
        choices = choices, selected = NULL)
    })
    output$downloads <- shiny::renderUI({
      plot_table <- shiny::req(input$plot_table)
      shiny::downloadButton(ns(plot_table), plot_table)
    })
    output$filename <- renderUI({
      filename <- paste0(shiny::req(download_list$filename))
      if(shiny::req(input$plot_table) == "Tables") {
        table <- shiny::req(input$table_choice)
        filename <- paste(table, filename, sep = "_")
      }
      shiny::textAreaInput(ns("filename"), "File Base Name:", filename)
    })
    output$Plots <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename),
                                   ".", tolower(input$png_pdf)),
      content = function(file) {
        shiny::req(input$plot_choice, input$preset, input$png_pdf)
        switch(input$preset,
          "1to1" = {
            width = 800
            height = 800
          },
          "3to2" = {
            width = 900
            height = 600
          },
          "16to9" = {
            width = 1600
            height = 900
          })
        switch(input$png_pdf,
          "PNG" = {
            grDevices::png(file, width = width, height = height)
          },
          "PDF" = {
            grDevices::pdf(file, width = width / 72,
                           height = height / 72)
          })
        print(chosen_plot())
        grDevices::dev.off()
      },
      contentType = "application/pdf")
    output$Tables <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".csv"),
      content = function(file) {
        chosen_table()
        utils::write.csv(table, file, row.names = FALSE)
      })
  })
}
#' @rdname downloadApp
#' @export
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  list(
    shiny::radioButtons(ns("plot_table"), "", c("Plots","Tables"), "Plots",
                        inline = TRUE),
    shiny::uiOutput(ns("choices"))
  )
}
#' @rdname downloadApp
#' @export
downloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_list"))
  # ** add png/pdf **
}
#' @rdname downloadApp
#' @export
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Download:"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("downloads"))),
      shiny::column(9, shiny::uiOutput(ns("filename")))))
}
