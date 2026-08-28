library(shiny)
library(shinydashboardPlus)

# =============================================================================
# Feedback Module
# UI:     FeedbackUI(id)
# Server: FeedbackServer(id)   - currently static, no server logic needed
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
        width = 6,
        paste0(
          "Dr. Hongjian Jin developed the original RNA-seq differential expression Shiny app, ",
          "RNAseqERCC report workflow, and EnrichR analysis module that formed the foundation of SPADE."
        )
      ),
      userBox(
        title = userDescription(
          title    = "Surbhi Sona",
          subtitle = "Bioinformatics Research Scientist",
          type     = 2,
          image    = "Surbhi.Sona128x128.jpg"
        ),
        status = "primary",
        width = 6,
        paste0(
          "Dr. Surbhi Sona integrated these components into SPADE, added fGSEA analysis, ",
          "and led the platform rebranding and continued development."
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        title = "Contact and Support",
        HTML(paste0(
          "SPADE is developed and maintained by the Center for Applied Bioinformatics ",
          "(CAB) at St. Jude Children's Research Hospital. CAB provides bioinformatics ",
          "analyses, services, and collaboration opportunities. For questions or ",
          "suggestions, contact <b>CAB.HelpDesk@stjude.org</b>.",
          "<br/><br/><i>Finding cures. Saving children.</i>"
        ))
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
    # No reactive logic needed - purely static content
  })
}