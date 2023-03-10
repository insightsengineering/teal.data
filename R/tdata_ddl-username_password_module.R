#' @export
username_password_server <- function(id, offline_args, code, tdata_function) {
  moduleServer(id, function(input, output, session) {
    # todo:
    #  - disable submit button
    #  - validate password and username
    #  - think about hashing password
    #  - enable submit only when username and password are not empty


    # create a tdata object when submit is pressed
    tdata <- eventReactive(input$submit, {
      foo(offline_args = offline_args, code = code, tdata_function = tdata_function, input = input)
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive tdata...
    return(tdata)
  })
}

#' @export
username_password_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("username"), label = "Username"),
    passwordInput(ns("password"), label = "Password"),
    actionButton(ns("submit"), label = "Submit")
  )
}

#' @export
username_password_args <- function() {
  list(
    username = quote(askpass::askpass('Please enter username')),
    password = quote(askpass::askpass('Please enter password'))
  )
}
