### Zadanie 4  ###
### Jacek Mańko ###

library(shiny)
library(plotly)
library(tidyverse)
library(DT)
library(rsconnect)
library(units)

### Data preprocessing ###

starwars_data = starwars %>%
  mutate(
    height = case_when(
      name == 'Finn' ~ as.integer(178),
      name == 'Rey' ~ as.integer(170),
      name == 'Poe Dameron' ~ as.integer(172),
      name == 'BB8' ~ as.integer(67),
      name == 'Captain Phasma' ~ as.integer(200),
      TRUE ~ height
    ),
    mass = case_when(
      name == 'Finn' ~ 73,
      name == 'Rey' ~ 54,
      name == 'Poe Dameron' ~ 80,
      name == 'BB8' ~ 18,
      name == 'Captain Phasma' ~ 76,
      TRUE ~ mass
    )
  )

install_symbolic_unit(name ="BBY") 

starwars_data = starwars_data %>% 
mutate_at(vars(homeworld, gender), ~replace(., is.na(.), "Unknown"))  %>%
replace_na(list(hair_color = "not applicable")) %>%
mutate(height = as.numeric(height)) %>%
mutate(mass = set_units(mass, kg)) %>%
mutate(height = set_units(height, cm))  %>%
mutate(birth_year = set_units(birth_year, BBY)) 


starwars_data = starwars_data %>% rename(Imię = name, Wzrost = height, Płeć = gender,
                         "Urodzony(a)" = birth_year, Planeta = homeworld,
                         Gatunek = species, Waga = mass, Włosy = hair_color,
                         Skóra = skin_color, Oczy = eye_color, Pojazdy = vehicles,
                         Statki = starships, Filmy = films)
                         

### Here it goes with the Shiny app ###


ui <- fluidPage(
  
  h1("Praca domowa nr 4", tags$br(), "Jacek Mańko", align = "center"),
  
  tags$br(),
  
  sidebarLayout(
  
    sidebarPanel(
               
    
     # selectInput("pickvalue2", label = "Kolor włosów", sort(starwars_data$Włosy),
                 # selected = NULL, multiple = T),
      #selectInput("pickvalue", label = "Gatuneczek", sort(starwars_data$Gatunek),
                #  selected = NULL, multiple = T),
        
      uiOutput("wlosy"),
      
      uiOutput("gatunek"),
      
      tags$br(),
      
      htmlOutput('x4')),    
    

      mainPanel(
      
        fluidRow(
        
        column(12, plotlyOutput('plot')),
       
        DT::dataTableOutput('tableOut')
       
      ) 
    )
  )
)

srv <- function(input, output, session) {
 
  
  starwarsy <- reactive({
    
    dat2 <- starwars_data 
    
    if (!is.null(input$pickvalue2)){dat2 <- dat2 %>% filter(Włosy %in% input$pickvalue2)} 
    
    if (!is.null(input$pickvalue)){dat2 <- dat2 %>% filter(Gatunek %in% input$pickvalue)} 
    
    return(dat2)
  
    })
  
  selected_data = reactive({
    
    sel_data = NULL

    ed = event_data("plotly_selected", source = "scatter") 
    
    if(!is.null(ed)){ 
      
      sel_data = starwarsy() %>% 
      filter(Imię %in% ed$key)   
      
    } else {
      sel_data = starwarsy()
    }
    
    sel_data
  })
  
  
  observeEvent(input$pickvalue,
               {
                 
                 updateSelectInput(
                  session = session,
                  inputId = "pickvalue2", 
                  choices = sort(selected_data()$Włosy),
                  selected = input$pickvalue2)},
               ignoreInit = FALSE,
               ignoreNULL = FALSE)
  
  
  output$wlosy <- renderUI({
    
    selectInput(
      inputId = "pickvalue2", 
      label = "Kolor włosów",
      choices = NULL,
      multiple = T)
    
  })
  
  
  observeEvent(input$pickvalue2, 
               {
                 updateSelectInput(
                   session = session,
                   inputId = "pickvalue",
                   choices = sort(selected_data()$Gatunek),
                   selected = input$pickvalue
                 )},
               ignoreInit = FALSE,
               ignoreNULL = FALSE)
  
  
  output$gatunek <- renderUI({
    
    
    selectInput(
      inputId = "pickvalue", 
      label = "Gatunek",
      choices = NULL,
      multiple = T)
    
  })
  
  
  output$tableOut<- renderDataTable({
    DT::datatable(selected_data() %>% 
                    mutate("L. filmów" = as.numeric(map(Filmy, length)),
                           "L. statków" = as.numeric(map(Statki, length)),
                           "L. pojazdów" = as.numeric(map(Pojazdy, length))) %>%  
                    select(Imię, Wzrost, Waga, Włosy, 
                           "Rok urodzenia" = "Urodzony(a)", 
                           "L. filmów",
                           "L. pojazdów", 
                           "L. statków"), 
                  selection = "single") 
  }) 
  
  
  output$plot <- renderPlotly({
    
    plot_ly(
      starwarsy(),
      source = 'scatter') %>%         
      add_markers(x = ~Wzrost, y = ~Planeta,
                  type = "scatter", 
                  mode = "markers",
                  color = ~Płeć,
                  key = ~Imię) %>% 
      layout(
            dragmode =  "select",
            xaxis = list(title = 'Wzrost [cm]'), 
            yaxis = list(title = 'Rodzima planeta')
      )
    
  })

  output$x4 = renderPrint({
    
    s = input$tableOut_rows_selected
    
    if (length(s)) {
     
      for (i in colnames(selected_data()[1:10])){
        
        if (!is.null(attributes(selected_data()[[i]])$units$numerator)) {
          
          cat(paste0(colnames(selected_data()[i]),":", " ", 
                     "<b>", paste0(selected_data()[s,i]), " ", attributes(selected_data()[[i]])$units$numerator), "</b>", sep="<br/>")
        }
         else {
          cat(paste0(colnames(selected_data()[i]),":", " ",
                     "<b>", selected_data()[s,i]), "</b>", sep="<br/>")
        }
        
      } 
      
      cat('',  collapse='\n', sep="<br/>")
      
      for (i in colnames(selected_data()[,c(12,13,11)])) {
        cat(paste0(colnames(selected_data()[i]), ":", collapse='\n',  sep="<br/>"))
        cat(paste0("<ul><li>", 
                   paste0(
                     paste0("<b>",unlist(selected_data()[s,i]), "</b>", collpase = ""), collapse = "</li><li>"),
                   "</li></ul>"))
      
      }
      #cat(paste0(colnames(selected_data()[12]), ":", collapse='\n',  sep="<br/>"))
      #cat(paste0("<ul><li>", 
       #          paste0(
        #           paste0("<b>",unlist(selected_data()[s,12]), "</b>", collpase = ""), collapse = "</li><li>"),
         #        "</li></ul>"))
      
        }
    })
  
}
    
shinyApp(ui = ui, server = srv)


