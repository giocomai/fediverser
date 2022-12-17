#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  batch_r <- reactiveVal(
    value = 0,
    label = "Identifier of retrieval batch used to prevent id collision"
  )

  posts_df_r <- reactiveVal(value = fed_empty_posts)

  observeEvent(
    eventExpr = list(input$list_end_reached),
    handlerExpr = {
      current_batch <- sum(batch_r(), 1)
      batch_r(current_batch)

      if (nrow(posts_df_r()) == 0) {
        current_max_id <- NULL
      } else {
        current_max_id <- min(as.numeric(posts_df_r()[["id"]]))
      }

      new_posts_df <- rtoot::get_timeline_hashtag(
        hashtag = input$hashtag_input,
        instance = input$instance_input,
        max_id = current_max_id
      ) %>%
        dplyr::mutate(internal_id = stringr::str_c(
          current_batch,
          "_",
          dplyr::row_number()
        )) %>%
        dplyr::slice(-1)

      posts_df_r() %>%
        tibble::add_row(new_posts_df) %>%
        posts_df_r()
    }
  )


  shiny::observeEvent(
    eventExpr = input$update_button,
    handlerExpr = {
      current_batch <- sum(batch_r(), 1)
      batch_r(current_batch)

      posts_df <- rtoot::get_timeline_hashtag(
        hashtag = input$hashtag_input,
        instance = input$instance_input
      ) %>%
        dplyr::mutate(internal_id = stringr::str_c(
          current_batch,
          "_",
          dplyr::row_number()
        ))

      purrr::walk(
        .x = purrr::transpose(.l = posts_df),
        .f = function(x) {
          mod_post_card_server(
            id = stringr::str_c("post_", x$internal_id),
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
            .x = stringr::str_c("post_", posts_df$internal_id),
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

  observeEvent(
    eventExpr = input$list_end_reached,
    handlerExpr = {
      if (nrow(posts_df_r()) == 0) {
        return(NULL)
      }

      process_df <- posts_df_r() %>%
        dplyr::mutate(batch = stringr::str_extract(
          string = internal_id,
          pattern = "[[:digit:]]+"
        ) %>%
          as.numeric())

      previous_max_batch <- max(process_df$batch) - 1

      new_posts_df <- process_df %>%
        dplyr::filter(batch > previous_max_batch)

      purrr::walk(
        .x = purrr::transpose(.l = new_posts_df),
        .f = function(x) {
          mod_post_card_server(
            id = stringr::str_c("post_", x$internal_id),
            post = x
          )
        }
      )

      posts_to_add_ui <- shiny::tagList(
        purrr::map(
          .x = stringr::str_c("post_", new_posts_df$internal_id),
          .f = function(x) {
            mod_post_card_ui(x)
          }
        ) %>%
          shiny::tagList() %>%
          shiny::fluidRow()
      )

      insertUI(
        selector = "#end",
        where = "beforeBegin",
        ui = posts_to_add_ui
      )
    }
  )
}
