library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plotly)


food <- read_csv("Data.csv", na = "")

# removing unnecessary columns
food <- food[,-c(1,3,4,8:11,20:22)]

#glimpse(food)

names(food)[3] <- "year"

supplychain <- c("Pre-Harvest","Harvest","Grading","Storage",
                 "Transport","Traders","Processing","Packaging",
                 "Distribution","Wholesale","Retail","Consumer",
                 "Export")

# fixing the order of levels
#supplychain <- factor(supplychain, levels = supplychain)

# typo correction in fsc_location1
food$fsc_location1 <- ifelse(food$fsc_location1 == "Havest","Harvest",food$fsc_location1) 


#food_new <- food %>% filter(!(is.na(fsc_location1)))
food_new <- food %>% filter(fsc_location1 %in% supplychain)

# fixing the order of levels
#food_new$fsc_location1 <-  food_new %>% factor(fsc_location1,levels=fsc_location1)




# p <- food_new %>% ggplot(aes(year,loss_per_clean,col=fsc_location1,
#                              text=row.names(food_new))) + geom_point()

# line breaks
linebreaks <- function(n){HTML(strrep(br(), n))}

#All <- list(unique(food_new$fsc_location1))

check_box <- c("All",unique(food_new$fsc_location1))

ui <- fluidPage(
  titlePanel("Global food losses and waste"),
  
  sidebarLayout(
    sidebarPanel(
           
           
           sliderInput("year","Select year range",value = c(2010,2015),
                       min=min(food_new$year),max=max(food_new$year)),
           checkboxGroupInput("valuechain","Select value chain",check_box, selected = "All"),
#            #radioButtons("valuechain","Select value chain",supplychain, inline = TRUE),
# #### CSS code for aligning the radioButtons in different columns
# #### https://www.debugcn.com/en/article/41369850.html
#            tags$script("$('.radio-inline').addClass('col-md-3');$('.shiny-options-group').addClass('row');"),
#            tags$head(
#              tags$style(
#                ".radio-inline{margin-left:10px;"
#              )
#            )),
),
    
          
          mainPanel(
            
              tabsetPanel(type="tabs",
                tabPanel("Box plot",linebreaks(3),
                         plotlyOutput("box_plot",width="100%"),linebreaks(1),
                         "Data source: http://www.fao.org/platform-food-loss-waste/flw-data/en/"),
                tabPanel("Data table",dataTableOutput("fooddata"))
              )
    
  )
)
)

server <- function(input, output, session) {
  
  plot2 <- reactive(if("All" %in% input$valuechain){
    food_new
  }else{food_new %>% filter(fsc_location1 %in% input$valuechain)
    }
    )
  
  plot1 <- reactive(plot2() %>% filter(year >= input$year[1] & year <= input$year[2]))
                           
  output$box_plot <- renderPlotly(
   ggplotly(plot1() %>% ggplot(aes(fsc_location1,loss_per_clean,
                               col=fsc_location1,fill=fsc_location1)) +
      geom_boxplot(alpha=0.5) +
        xlab("") + 
        ylab("Percentage loss (%)") +
        theme(axis.text.x=element_text(angle = 45)) +
        theme(legend.title = element_blank()) +
        labs(caption = "Data source: http://www.fao.org/platform-food-loss-waste/flw-data/en/"))
      )
  
  output$fooddata <- renderDataTable(plot1(), options = list(pageLength = 10))
}

shinyApp(ui, server)

