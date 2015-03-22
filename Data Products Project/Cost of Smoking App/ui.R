source('smoking_code.R', local=FALSE)

shinyUI(pageWithSidebar(
  headerPanel("Calculate the Cost of Smoking by U.S. State"),
  sidebarPanel(
    selectInput('user_state', "Choose a U.S. State", choices = paste(states[-9])),
    #selectInput('user_units', "Select the Units", choices = c("cigarettes", "packs")),
    radioButtons("user_units", 
                       label = ("Select the Units"), 
                       choices = list("cigarettes" = 1, "packs" = 2),
                       selected = 1),
    numericInput('user_number', "Specify a Quantity", value = ""),
    #selectInput('user_time_units', "Per", choices = c("day", "week")),
    radioButtons("user_time_units", 
                       label = ("Per"), 
                       choices = list("day" = 1, "week" = 2),
                       selected = 1),
    numericInput('user_duration', 'Specify Number of Years Smoking', value = ""),
    submitButton(text = "Calculate!", icon = NULL)
  ),
  mainPanel(
    div(h2(textOutput("response")), style = "color:blue"),
    br(), br(),br(),br(),br(),br(),br(),br(),br(),br(), br(),br(),br(),br(),br(),
    div(h4("This calculation is based on \"Retail Price Per Pack With All Taxes\" data from http://www.tobaccofreekids.org/research/factsheets/pdf/0099.pdf "), style = "color:black")
  )
))