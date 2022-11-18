#' mod_post_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_post_card_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      style = "border: 2px solid #375a7f;padding:10px;margin:10px;border-radius:25px;width:100%;",
      shiny::uiOutput(outputId = shiny::NS(id, "card_ui"))
    )
  )
}

#' mod_post_card Server Functions
#'
#' @noRd
mod_post_card_server <- function(id,
                                 post) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$card_ui <- shiny::renderUI(
      shiny::tagList(
        shiny::strong(post$account$username),
        htmltools::hr(),
        htmltools::strong(post$spoiler_text),
        HTML(post$content),
        htmltools::tags$a(lubridate::as_datetime(post$created_at),
          href = post$uri,
          target = "_blank"
        )
      )
    )
  })
}

## To be copied in the UI
# mod_post_card_ui("mod_post_card_1")

## To be copied in the server
# mod_post_card_server("mod_post_card_1")

#  x <- purrr::transpose(.l = get_timeline_hashtag(hashtag = "rstats", instance = "fosstodon.org"))[[1]]
# mod_post_card_app(x)
mod_post_card_app <- function(post) {
  ui <- shiny::fluidPage(
    mod_post_card_ui("mod_post_card_1")
  )

  server <- function(input, output, session) {
    mod_post_card_server(
      id = "mod_post_card_1",
      post = post
    )
  }
  shiny::shinyApp(ui, server)
}
