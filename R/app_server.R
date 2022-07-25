#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic

  # intro modal -------------------------------------------------------------
  observeEvent("", {
    fct_helpModal(md = "intro")
  })

  # render explore tab ------------------------------------------------------

  mod_explore_server("explore_tab")

  # render estimate tab ------------------------------------------------------

  mod_estimate_server("estimate_tab")

  # render understand tab ----------------------------------------------------

  mod_impact_server("impact_tab")

  # stop session when browser is closed -------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })

  # warn user after 7 minutes of inactivity that they have 3 minutes left -----

  shinyjs::runjs(
    "(function() {
      var timeoutWarningMsecs = 7 * 60 * 1000;
      var idleTimer;

      function onTimeout() {
        alert('Warning: Your session will timeout in 3 minutes.');
      }

      function startIdleTimer() {
        if (idleTimer) clearTimeout(idleTimer);
        idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
      }

      $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

    })();"
  )
}
