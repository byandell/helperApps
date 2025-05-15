#' Download App
#'
#' @param id identifier for shiny reactive
#' @param download_list reactiveValues object
#'
#' @importFrom shiny br downloadButton downloadHandler h5 moduleServer NS
#'             observeEvent radioButtons reactive reactiveValues renderPlot
#'             renderUI req shinyApp textAreaInput uiOutput updateRadioButtons
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf png
#' @importFrom bslib card card_header layout_columns page_sidebar sidebar
#' @export
downloadApp <- function(id) {
  ui <- bslib::page_sidebar(
    title = "Test Kalynn Download",
    sidebar = bslib::sidebar("side_panel", width = 400,
      downloadInput("download"), # plot_table, inputs for Plots or Tables
      downloadOutput("download") # downloadButton, filename
    ),
    bslib::card(
      downloadUI("download")
    ),
    bslib::card(
      bslib::card_header("Download Preview"),
      downloadShow("download")    # Only for Preview of downloadApp().
    )
  )
  server <- function(input, output, session) { 
    # Test sets
    prefix <- "Download"
    download_list <- shiny::reactiveValues(
      filename = "panelID_instanceID",
      no_download = "plots_skip",
      plots = shiny::reactiveValues(
        skip = shiny::reactive(plot_null("skip")),
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
    
    plot_width_rv <- shiny::reactiveVal(1200)
    plot_height_rv <- shiny::reactiveVal(600)
    use_alternating_colors_rv <- shiny::reactiveVal(TRUE)
    clicked_plotly_point_details_rv <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$plot_width, { plot_width_rv(input$plot_width) }, ignoreNULL = TRUE)
    shiny::observeEvent(input$plot_height, { plot_height_rv(input$plot_height) }, ignoreNULL = TRUE)
    
    shiny::observeEvent(input$preset_1to1, {
      shiny::updateNumericInput(session, "plot_width", value = 800)
      shiny::updateNumericInput(session, "plot_height", value = 800)
    })
    shiny::observeEvent(input$preset_3to2, {
      shiny::updateNumericInput(session, "plot_width", value = 900)
      shiny::updateNumericInput(session, "plot_height", value = 600)
    })
    shiny::observeEvent(input$preset_16to9, {
      shiny::updateNumericInput(session, "plot_width", value = 1280)
      shiny::updateNumericInput(session, "plot_height", value = 720)
    })
    
    shiny::observeEvent(input$color_toggle, {
      if(is.logical(input$color_toggle)){
        use_alternating_colors_rv(input$color_toggle)
      } else {
        use_alternating_colors_rv(!use_alternating_colors_rv())
      }
    })
    
    output$download_list <- shiny::renderUI({
      list(
        "filename",
        download_list$filename,
        shiny::br(),
        "no download",
        paste(download_list$no_download, collapse = ", "),
        shiny::br(),
        "plots",
        paste(names(download_list$plots), collapse = ", "),
        shiny::br(),
        "tables", 
        paste(names(download_list$tables), collapse = ", "),
        switch(shiny::req(input$plot_table),
          # ** this only shows last plot **
          Plots  = shiny::uiOutput(ns("preview_plots")),
          Tables = DT::dataTableOutput(ns("preview_table")))
      )
    })
    selected_plot <- shiny::reactive({
      plot <- shiny::req(input$Plots_choice)
      shiny::req(download_list$plots[[plot]]())
    })
    output$preview_plots <- shiny::renderUI({
      width <- shiny::reactive(plot_width_rv() / 2)
      height <- shiny::reactive(plot_height_rv() / 2)
      shiny::renderPlot({
        shiny::req(selected_plot())
      }, width = width, height = height)
    })
    selected_table <- shiny::reactive({
      table <- shiny::req(input$Tables_choice)
      shiny::req(download_list$tables[[table]]())
    })
    output$preview_table <- DT::renderDataTable({
      shiny::req(selected_table())
    })
    output$choices <- shiny::renderUI({
      switch(shiny::req(input$plot_table),
        Plots = {
          bslib::layout_columns(
            shiny::radioButtons(ns("Plots_choice"), "Plot:",
              "", inline = TRUE),
            shiny::uiOutput(ns("choices_Plots"))
          )
        },
        Tables = {
          shiny::radioButtons(ns("Tables_choice"), "Table:", "", inline = TRUE)
        })
    })
    output$choices_Plots <- shiny::renderUI({
      # Use supplied `create_` functions or standard `shiny`.
      if (!exists("create_button", mode = "function")) {
        create_button <- shiny::actionButton
      }
      if (!exists("create_download_button", mode = "function")) {
        create_download_button <- shiny::downloadButton
        button_class <- "btn-sm"
      } else {
        button_class <- "btn-sm btn-light"
      }
      if (!exists("create_numeric_input", mode = "function")) {
        create_numeric_input <- shiny::numericInput
      }
      if (!exists("create_lever_switch", mode = "function")) {
        create_lever_switch <- shiny::checkboxInput
      }
      list(
        # Row for plot title, download buttons, and preset buttons
        shiny::div(
          style = paste("display: flex; justify-content: space-between;",
                        "align-items: center; margin-bottom: 10px;",
                        "flex-wrap: wrap;"),
          shiny::h4(
            "LOD Score Plot",
            style = "margin: 0 15px 0 0; color: #2c3e50; font-weight: 600;"),
          shiny::div(
            style = paste("display: flex; align-items: center; gap: 10px;",
                          "flex-grow: 1; justify-content: flex-end;"),
            # Preset Aspect Ratio Buttons
            shiny::div(
              style = "display: flex; gap: 5px; margin-right: 15px;",
              shiny::tagList(
                create_button(ns("preset_1to1"), "1:1", class = button_class),
                create_button(ns("preset_3to2"), "3:2", class = button_class),
                create_button(ns("preset_16to9"), "16:9", class = button_class)
              )
            ),
            # Download Buttons
            shiny::tagList(
              create_download_button(ns("download_plot_png"), "PNG",
                                     class = "btn-sm"),
              create_download_button(ns("download_plot_pdf"), "PDF",
                                     class = "btn-sm")
            )
          )
        ),
        # Row for plot dimension controls and color toggle
        shiny::div(
          style = "display: flex; gap: 10px; align-items: center; margin-bottom: 5px; flex-wrap: wrap;",
          shiny::div(
            style = "display: flex; align-items: center; gap: 10px;",
            create_numeric_input(ns("plot_width"), "Width:",
              value = 1200, min = 400, max = 2000, step = 50, width = "100px"),
            create_numeric_input(ns("plot_height"), "Height:",
              value = 600, min = 300, max = 1200, step = 50, width = "100px")
          ),
          create_lever_switch(ns("color_toggle"), "Alt Colors", value = TRUE)
        )
      )
    }) 
    choices_download <- function(type) {
      # Choices excluding any in `no_download` vector.
      choices = names(download_list[[type]])
      if(shiny::isTruthy(download_list$no_download)) {
        choices <- choices[
          !(paste0(type, "_", choices) %in% download_list$no_download)]
      }
      choices
    }
    shiny::observeEvent(shiny::req(input$plot_table, download_list$plots), {
      choices = choices_download("plots")
      shiny::updateRadioButtons(session, "Plots_choice", 
        choices = choices, selected = NULL)
    })
    shiny::observeEvent(shiny::req(input$plot_table, download_list$tables), {
      choices = choices_download("tables")
      shiny::updateRadioButtons(session, "Tables_choice",
        choices = choices, selected = NULL)
    })
    output$filename <- renderUI({
      filename <- paste0(shiny::req(download_list$filename))
      # Prepend filename with Plots or Tables choice.
      filename <- paste(
        shiny::req(input[[
          paste(shiny::req(input$plot_table), "choice", sep = "_")]]),
        filename, sep = "_")
      shiny::textAreaInput(ns("filename"), "File Basename:", filename)
    })
    # Download handlers for QTL plot
    download_filename <- function(mime = "png") {
      function() {
        paste0(shiny::req(input$filename), "_",
               format(Sys.time(), "%Y%m%d"), ".", mime)
      }
    }
    output$download_plot_png <- shiny::downloadHandler(
      filename = download_filename("png"),
      content = function(file) {
        shiny::req(selected_plot())
        # Use dynamic width/height for saving
        ggplot2::ggsave(file, plot = selected_plot(), 
          width = plot_width_rv()/96, # Assuming 96 DPI for conversion from px
          height = plot_height_rv()/96, 
          dpi = 300, units = "in")
      }
    )
    output$download_plot_pdf <- shiny::downloadHandler(
      filename = download_filename("pdf"),
      content = function(file) {
        shiny::req(selected_plot())
        ggplot2::ggsave(file, plot = selected_plot(), 
          width = plot_width_rv()/96, 
          height = plot_height_rv()/96, 
          device = cairo_pdf, units = "in") # Use cairo_pdf for better PDF quality
      }
    )
    output$Tables <- shiny::downloadHandler(
      filename = download_filename("csv"),
      content = function(file) {
        selected_table()
        utils::write.csv(table, file, row.names = FALSE)
      })
  })
}
#' @rdname downloadApp
#' @export
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("plot_table"), "", c("Plots","Tables"), "Plots",
                        inline = TRUE)
}
#' @rdname downloadApp
#' @export
downloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("choices"))
}
#' @rdname downloadApp
#' @export
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  list(
    shiny::h5("Download:"),
    bslib::layout_columns(
      shiny::uiOutput(ns("downloads")),
      shiny::uiOutput(ns("filename")),
      col_widths = c(3,9)))
}
downloadShow <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("download_list"))
}
  