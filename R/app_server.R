#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  shiny::observeEvent(
    eventExpr = input$update_button,
    handlerExpr = {
      purrr::walk(
        .x = purrr::transpose(.l = rtoot::get_timeline_hashtag(
          hashtag = input$hashtag_input,
          instance = input$instance_input
        ) %>%
          dplyr::mutate(row_id = dplyr::row_number())),
        .f = function(x) {
          mod_post_card_server(
            id = stringr::str_c("post_", x$row_id),
            post = x
          )
        }
      )

      output$hashtag_cards_ui <- shiny::renderUI({
        shiny::tagList(
          # purrr::map(
          #   .x = get_groups_names(),
          #   .f = function(current_group) {
          #     shiny::column(
          #       width = 3,
          #       shiny::h2(current_group),

          purrr::map(
            .x = stringr::str_c("post_", 1:20),
            .f = function(x) {
              mod_post_card_ui(x)
            }
          ) %>%
            shiny::tagList() %>%
            shiny::fluidRow()
          #    )
          #  }
          # ) %>%
          #   fluidRow()
        )
      })
    }, ignoreNULL = FALSE
  )
}
