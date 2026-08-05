library(shiny)
library(shinydashboardPlus)

# =============================================================================
# Feedback Module
# UI:     FeedbackUI(id)
# Server: FeedbackServer(id)   – currently static, no server logic needed
# =============================================================================

#' Feedback UI
#'
#' Executes FeedbackUI.
#'
#' @param id Argument for FeedbackUI.
#'
#' @return Return value produced by FeedbackUI.
FeedbackUI <- function(id) {
  fluidPage(
    fluidRow(
      userBox(
        title = userDescription(
          title    = "Hongjian Jin",
          subtitle = "Principal Bioinformatics Research Scientist",
          type     = 2,
          image    = "Hongjian.Jin128x128.jpg"
        ),
        status = "primary",
        paste0(
          "This App is developed and maintained by Dr. Hongjian Jin at the Center for ",
          "Applied Bioinformatics (CAB), St Jude Children's Research Hospital (SJCRH). ",
          "As a shared core facility, we keep expanding our available analyses, services ",
          "and opportunities for collaborations. If you have any questions or suggestions, ",
          "please feel free to contact CAB (CAB.HelpDesk@stjude.org) or the developer ",
          "(Hongjian.Jin@stjude.org)."
        ),
        footer = HTML("<i>Finding cures. Saving children.</i>")
      ),
      userBox(
        title = userDescription(
          title    = "Surbhi Sona",
          subtitle = "Bioinformatics Research Scientist",
          type     = 2,
          image    = "Surbhi.Sona128x128.jpg"
        ),
        status = "primary",
        paste0(
          "This App is maintained by Dr. Surbhi Sona at the Center for ",
          "Applied Bioinformatics (CAB), St Jude Children's Research Hospital (SJCRH). ",
          "As a shared core facility, we keep expanding our available analyses, services ",
          "and opportunities for collaborations. If you have any questions or suggestions, ",
          "please feel free to contact CAB (CAB.HelpDesk@stjude.org) or the current maintainer ",
          "(Surbhi.Sona@stjude.org)."
        ),
        footer = HTML("<i>Finding cures. Saving children.</i>")
      )
    )
  )
}

#' Feedback Server
#'
#' Executes FeedbackServer.
#'
#' @param id Argument for FeedbackServer.
#'
#' @return Return value produced by FeedbackServer.
FeedbackServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No reactive logic needed – purely static content
  })
}
