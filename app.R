library(shiny)
library(tidyverse)
library(bslib)


cdc = read_csv("data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")
cdc = cdc %>%
  filter(!is.na(COVID19_Deaths)) %>%
  filter(COVID19_Deaths !=0)

deaths = c(
    "Covid" = "COVID19_Deaths",
    "Pneumonia" = "Pneumonia_Deaths",
    "Influenza" = "Influenza_Deaths",
    "Total" = "Total_Deaths"
)

# Define UI for application that draws a histogram
app=fluidPage(

    # Application title
    titlePanel("CDC Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "State", label = "State:", choices = unique(cdc$`State`)),
            selectInput(inputId = "Age_Group", label = "Age:", choices = unique(cdc$`Age_Group`)),
            selectInput(inputId = "Deaths", label = "Death:", choices = deaths),
            checkboxInput(inputId =  "Cost", label = "Find Cost of Deaths?")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)



ui <- navbarPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    title = "CDC Deaths Data",
    tabPanel("App", app),
    tabPanel("Table", dataTableOutput("table")),
    tabPanel("About", includeMarkdown("about.Rmd")))


# Define server logic required to draw a histogram
server=function(input, output) {
    
    cdc_state = reactive({
        cdc%>%
            filter(State == input$State) %>%
            filter(Group == 'By Month') %>%
            select(-State,-Year, -Month, -Footnote, -Pneumonia_Influenza_COVID_Deaths, 
                   -Pneumonia_COVID_Deaths, -Sex)
    })
    
    observeEvent(
        eventExpr = input$State,
        handlerExpr = {
            updateSelectInput(inputId = 'Age_Group', choices = unique(cdc_state()$Age_Group))
        }
    )
    
    
    cdc_state_age = reactive({
        cdc_state_age = cdc_state()%>%
            filter(Age_Group == input$Age_Group) %>%
            select(-Age_Group)
        
        if(input$Cost){
            cdc_state_age = cdc_state_age%>%
                mutate(across(COVID19_Deaths:Influenza_Deaths, deaths_to_dollars))
        }
        cdc_state_age
    })
    
    output$table = renderDataTable(cdc_state_age())
    output$distPlot=renderPlot({
        ggplot(data = cdc_state_age(), aes(Start_Date,!!as.symbol(input$Deaths))) + geom_point() + theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
