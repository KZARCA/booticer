library(shiny)
library(booticer)
create_alert <- function(text){
  modalDialog(
    div(
    class = "conteneur center",
    icon("times-circle-o", class = "fa-3x red")
    ),
    div(
      class = "conteneur center",
      h3(text)),
    footer = modalButton("OK")) %>%
    showModal()
}
