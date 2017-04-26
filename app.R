library(shiny)
library(ggplot2)


package_check <- require("plotly")
if (package_check == FALSE) {
  install.packages('plotly')
}
library("plotly")  

package_check <- require("GGally")
if (package_check == FALSE) {
  install.packages('GGally')
}
library("GGally")  



FB_df <- read.csv("dataset_Facebook.csv", sep = ';')

#Rename columns to usable format
colnames(FB_df) = c('Total_Likes', 'Type', 'Category', 'Month', 'Day',
                    'Hour', 'Paid', 'Reach', 'Impressions', 'Users',
                    'Consumers', 'Consumptions', 'Impressions_by_Likers',
                    'Reach_by_Likers', 'No_Liked_and_Engaged', 'Comments',
                    'Likes', 'Shares', 'Interactions')

FB_df <- na.omit(FB_df) #Removes 5 rows of data

FB_df$Month <- month.abb[FB_df$Month]
FB_df$Day <- factor(FB_df$Day, labels = c('Mon', 'Tue', 'Wed', 'Thu','Fri', 'Sat', 'Sun'), ordered = TRUE)
FB_df$Paid <- factor(FB_df$Paid, labels = c('Unpaid', 'Paid'))

#Remove outlier point which messes up scale
FB_df <- FB_df[FB_df$Likes < 4000, ]

ui <- fluidPage(headerPanel("Amazing Display of Facebook Information"),
                fluidRow(column(12, tabsetPanel(
                                             tabPanel("Bubble Plot", plotlyOutput("bubble")),
                                             tabPanel("Scatterplot", plotOutput("scatter")),
                                             tabPanel("Parallel Plot", plotlyOutput("parallel")),
                                             selectizeInput("month", "Select month to view:",
                                                            choices = c("All", "Jan", "Feb",
                                                                        "Mar", "Apr", "May",
                                                                        "Jun", "Jul", "Aug",
                                                                        "Sep", "Oct", "Nov",
                                                                        "Dec"))))
                                ))
                
                                                          
   
                                                          



server <- function(input, output) {
  
  Month_select <- reactive({
    
    switch(input$month,
           
           "All" = FB_df, "Jan" = FB_df[FB_df$Month == 'Jan', ],
           "Feb" = FB_df[FB_df$Month == 'Feb', ],
           "Mar" = FB_df[FB_df$Month == 'Mar', ],
           "Apr" = FB_df[FB_df$Month == 'Apr', ],
           "May" = FB_df[FB_df$Month == 'May', ],
           "Jun" = FB_df[FB_df$Month == 'Jun', ],
           "Jul" = FB_df[FB_df$Month == 'Jul', ],
           "Aug" = FB_df[FB_df$Month == 'Aug', ],
           "Sep" = FB_df[FB_df$Month == 'Sep', ],
           "Oct" = FB_df[FB_df$Month == 'Oct', ],
           "Nov" = FB_df[FB_df$Month == 'Nov', ],
           "Dec" = FB_df[FB_df$Month == 'Dec', ]
           
         
           )
    
  })
  
  
  
  output$bubble <- renderPlotly(
    
    { data_used <- Month_select()
    
    ggplot() + geom_point(data = data_used, aes(x = Likes, y = Shares, colour = Type, size = Impressions)) + 
      
      ggtitle("Facebook Likes vs Shares (Sized by Total Likes)") +
      
      theme_bw() + theme(legend.title=element_blank()) +
      
      theme(axis.title = element_blank()) + scale_size(guide = 'none')}
    
  )
  
  output$scatter <- renderPlot(
    { data_used <- Month_select()

    ggpairs(data_used, columns = c(16:18), mapping = aes(color = Paid),
            title = 'Correlation of Comments, Likes and Shares (Paid vs. Unpaid)')}
  )
  
  output$parallel <- renderPlotly(
    
    { data_used <- Month_select()
    
    par_plot = ggparcoord(data = data_used, columns = 16:19, groupColumn = 'Day',
                        scale = 'globalminmax', title = 'Parallel View of Likes, Shares and Comments') +
      
      theme_bw() + ylab(NULL) + theme(legend.title=element_blank(), 
            axis.title.x = element_blank(), 
            panel.background = element_rect(fill="white",color = "black", size = 0.5))

      ggplotly(par_plot, tooltip = c('colour', 'value'))
  }
    
  )
  
}




shinyApp(ui = ui, server = server)

