library(shiny)
library(shinythemes)
library(shinyjs)
shinyUI(fluidPage(
  
  titlePanel("Chipotle Restaurant Reviews"),
  
  sidebarLayout( 
  
    position = "left",
    
    sidebarPanel(
   
      
      actionButton("random", "Random Review", width = "100%"),
    
      p(),
    
      actionButton("analzye", "Analyze the Reviews", width = "100%")
    

     
   )
     
     
   ,
  
  
mainPanel(
  column(12,
         #verbatimTextOutput("msg"),
         textAreaInput(
           inputId="reviewbox", 
           label="Review Display:", 
           width = 600, height = 350, resize = "both"
         )
  ),
  
  column(12,
         textAreaInput(
           inputId = "response",
           label = "Review Response:",
           width = 600, height =  300, resize = "both"
           
           
         )
         
         )
  
  )
  
)))
