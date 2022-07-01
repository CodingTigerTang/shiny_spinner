

library(shiny)
library(tidyverse)
library(shinyjs)
library(reactable)
library(googlesheets4)


data_on_gs <- function(){
  
  gs4_deauth()
  gs4_auth(cache = ".secrets", email = read_rds("data/email.rds"))
  data <- read_sheet(read_rds("data/gs_path.rds"),sheet = "history")
  data 
  
}


save_record_gs <- function(data) {
  
  gs4_deauth()
  gs4_auth(cache = ".secrets", email = read_rds("data/email.rds"))
  write_sheet(data,read_rds("data/gs_path.rds"),sheet = "history")
}

ui <- fluidPage(
  
  useShinyjs(),
  extendShinyjs(script = "confetti.js",functions = c("startConfettiInner","removeConfettiInner","stopConfettiInner")),
  extendShinyjs(script = "sound.js",functions = c("congrats")),
  tagList(tags$head(tags$style(type = 'text/css','.navbar-nav{display:none;}')),
    

          navbarPage(
            theme = "sandstone.min.css",
            title = "Take me for a Spin!",windowTitle = "Shiny Spinner",
            tabPanel("",     
                     sidebarLayout(
                       sidebarPanel(textAreaInput("names","Entries",height = "250px",value = c("Tiger\nClaire\nDiego\nBanita\nEagle\nCoffee\n")),
                                    #  actionButton("history",label = "",icon = icon("book")), commen this out for reproducibility on git
                                    width = 3),
                       mainPanel = mainPanel(width = 6,plotOutput("spin",width = "550px",height = "550px",click = "spin_start"),align="center"),
                      
                     )
            )
          ))
)

server <- function(input,output,session) {
  
  names <- reactive({strsplit(input$names,split = "\n") %>% pluck(1)})  
  
  start <- reactiveVal(0)
  number <- reactiveVal(0)
  active <- reactiveVal(FALSE)  
  times <- reactiveVal(0)
  chosen <- reactiveVal(NULL)
  
  observeEvent(active(),{  
    if (length(names())>10){
      times(c(rep(c(1:length(names())),times = 2),c(1:sample(1:length(names()),1))))    } else {
        times(c(rep(c(1:length(names())),times = 3),c(1:sample(1:length(names()),1))))
      }
    timer <- reactiveVal(times())
    
    observe({
      invalidateLater(1000, session)
      isolate({
        if(active()) {
          
          if (start()<length(times())){
            start(start()+1)
            number(times()[start()])
          } else if (start() >= length(times())) {
            active(FALSE)
            js$startConfettiInner()
            js$congrats()
            
            showModal(
              
              modalDialog(
                title = sprintf("Congrats %s",names()[number()]),
                renderUI({
                  gif <- giphyr::gif_search("congrats",img_format = c("downsized_medium"))[,-24] %>% sample_n(1)
                  actionLink(gif$id, title = gif$slug,
                             label = NULL, class = "gifpreview", icon = NULL,
                             tags$img(src = gif$downsized_medium))
                }),
                
                easyClose = F,size = "m",
                footer = tagList(
                  actionButton("reset", "Spinner again"),
                  actionButton("remove", "Remove"),
                )
              ))
            
          }
        }
      })
    })
    
  })
  observeEvent(input$reset,{
    
    chosen <- chosen(c(chosen(),names()[number()]))
    
    start(0)
    number(0)
    active(FALSE)  
    removeModal()
    js$stopConfettiInner()
    
  })
  
  observeEvent(input$remove,{
    
    chosen <- chosen(c(chosen(),names()[number()]))
    
    updateTextInput(session, "names", label = "names",
                    value = strsplit(input$names,split = "\n") %>% pluck(1) %>% .[-number()] %>% paste0(collapse = "\n") )
    start(0)
    active(FALSE)  
    removeModal()
    js$stopConfettiInner()
    number(0)
    
  })
  
  
  observeEvent(input$spin_start,{
    active(T)
    
    data.frame(
      session = session$token,
      date = Sys.Date(),
      times = 1,
      stringsAsFactors = F
    ) %>% 
      bind_rows(data_on_gs()) %>% 
      save_record_gs()
    
  })
  
  observeEvent(input$history,{
    
    all_time <- data_on_gs() %>% 
      summarise(sum = sum(times,na.rm = T)) %>% 
      pull(sum)
    
    showModal(
      
      modalDialog(
        title = sprintf("Session spins (all time spins: %s)",all_time),
        renderUI({
          reactableOutput("session")
        }),
        
        easyClose = T,size = "m"
      ))  
    
  })

  output$session <- renderReactable({
    req(input$history)
    drawing_data <- data.frame(
      picks = chosen()
    ) %>% 
      mutate(order = row_number()) 
    
    reactable(drawing_data)
    
  })
  
  
  
  output$spin <- renderPlot({
    
    req(names())
    
    colors <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F',
                '#FF7F00','#CAB2D6','#6A3D9A','#8DD3C7','#BEBADA','#FB8072', #'#FFFFB3', too brigh
                '#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD') %>% 
      .[1:length(names())]
    
    update_color2 <- function(number, colors){
      colors[-number] <- "#F0F0F0"
      colors
    }

    
    if (length(names())>=7 & length(names())<=10) {
      nudge_x = 0.1
      size = 6
    } else if (length(names())>9){
      nudge_x = 0.2
      size = 5
    } else {
      nudge_x = 0
      size = 7
    }
    # new plot ----------------------------------------------------------------
    
    data <- data.frame(
      name = names(),
      value = rep(c(1),times = length(names()))
    )
    
    # Compute percentages
    data$fraction <- data$value / sum(data$value)
    data$ymax <- cumsum(data$fraction)
    data$ymin <- c(0, head(data$ymax, n=-1))
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Make the plot
    plot <- ggplot(data %>% mutate(name = as_factor(name)), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
      geom_rect() +
      geom_text( x=3.5, aes(y=labelPosition, label=name),nudge_x = nudge_x, size=size) + 
      coord_polar(theta="y") +
      xlim(c(2.7, 4)) +
      theme_void() +
      theme(legend.position = "none") + 
      annotate("label", x = 2.7, y=1,size = 6, label = "Click Me")
    
    # end of new plot ---------------------------------------------------------
    
    plot_function <- function(number) {
      p <- plot + scale_fill_manual(values=update_color2(number,colors))
      return(p)
    }
    
    plot_function(number())
  }) #%>% 
    #bindCache(number(),input$names)
  
  output$questions <- renderUI({
    req(input$question)
    text <- questions %>% sample_n(1) %>% pull(Questions)
    
    shinydashboard::box(title = text,width=12,solidHeader = T,background = "light-blue",
                        status = 'primary'
    )
  })  
  
  
}


shinyApp(ui, server)