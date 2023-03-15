#' Dummy DDL app
#'
#' Run it
#' @examples
#'
#' example <- tdata_ddl(
#'   code = "
#'     conn <- open_dummy_connection(username = {username}, password = {password})
#'     my_data <- get_dummy_data(conn)
#'     close_conn()
#'   ",
#'   tdata_function = function(env_list, code) {
#'     new_tdata(data = list(DATA = env_list$my_data), code = code)
#'   }
#' )
#' app <- dummy_teal_init(example)
#'
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
#'@export
dummy_teal_init <- function(object) {
  app <- shinyApp(
    ui = fluidPage(
      fluidRow(
        column(
          3,
          h1("User Inputs"),
          object$ui(id = "custom_ui")
        ),
        column(
          9,
          h1("R code"),
          verbatimTextOutput("rcode")
        )
      ),
      h1("Data"),
      verbatimTextOutput("data_ui")
    ),
    server = function(input, output, session) {
      loaded_data <- object$server(id = "custom_ui", object$offline_args, object$code, object$tdata_function)
      output$rcode <- renderText({
        req(loaded_data())
        get_code(loaded_data())
      })
      output$data_ui <- renderPrint({
        req(loaded_data())
        loaded_data()
      })
    }
  )
}
