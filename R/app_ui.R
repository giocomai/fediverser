#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),
      theme = bslib::bs_theme(
        version = 4,
        bootswatch = "darkly"
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::textInput(
            inputId = "instance_input",
            label = "Instance",
            value = "fosstodon.org"
          ),
          shiny::textInput(
            inputId = "hashtag_input",
            label = "Hashtag",
            value = "rstats"
          ),
          shiny::actionButton(
            inputId = "update_button",
            label = "Update",
            icon = shiny::icon(name = "sync"),
            style = "margin-left:10px;margin-right:10px;"
          )
        ),
        style = "border: 2px solid #375a7f;padding:10px;margin:10px;border-radius:25px;width:100%;"
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(outputId = "hashtag_cards_ui")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fediverser"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
