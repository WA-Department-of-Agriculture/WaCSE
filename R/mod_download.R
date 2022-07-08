#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 8, status = "primary",
      textInput(ns("name"), "Organization or Farm Name"),
      textInput(ns("project"), "Project Name"),
      downloadButton(ns("report")),
      bookmarkButton())
  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$report <- downloadHandler(

      filename = function() {
        paste0(Sys.Date(), "_WaCSE_Report.pdf")
        },

      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
          tempReport <- file.path(tempdir(), "WaCSE_Report.Rmd")
          file.copy(normalizePath("inst/app/www/WaCSE_Report.Rmd"),
                    tempReport, overwrite = TRUE)
        })

        # Set up parameters to pass to Rmd document
        params <- list(
          name = input$name,
          project = input$project
          )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")
