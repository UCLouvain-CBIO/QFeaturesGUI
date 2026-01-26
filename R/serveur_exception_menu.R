#' The server logic behind the exception dropdown menu
#'
#' @param input input parameter that should be given by
#'  the higher level server builder
#' @param output output parameter that should be given by
#'  the higher level server builder
#' @param session session parameter that should be given by
#'  the higher level server builder
#'
#' @return The server logic behind the exception dropdown menu
#' @rdname INTERNAL_server_exception_menu
#' @keywords internal
#'
#' @importFrom shiny reactive observe observeEvent showModal modalDialog modalButton verbatimTextOutput renderText
#' @importFrom shinydashboard dropdownMenu renderMenu
#' @importFrom htmltools div h3 HTML span br
server_exception_menu <- function(input, output, session) {
  
  output$exception_menu <- renderMenu({
    dropdownMenu(
      type = "messages",
      icon = icon("warning"),
      badgeStatus = "danger",
      .list = lapply(seq_len(nrow(global_rv$exception_data)), function(i) {
        row <- global_rv$exception_data[i, ]
        clickableMessageItem(row$id, row$title, row$time)
      })
    )
  })
  
  observeEvent(input$exception_clicked, {
    req(input$exception_clicked)
    
    row <- global_rv$exception_data[
      global_rv$exception_data$id == input$exception_clicked, ]
    
    req(nrow(row) == 1)
    
    showModal(modalDialog(
      title = row$title,
      tags$div(
        class = "italic-text",
        "Occurred at ", format(row$time, "%H:%M:%S")
      ),
      h4("Function call"),
      verbatimTextOutput("func_call"),
      h4("Full message"),
      verbatimTextOutput("full_message"),
      easyClose = TRUE,
      size = "l"
    ))
    
    output$func_call <- renderText(row$func_call)
    output$full_message <- renderText(row$full_message)
  })}

