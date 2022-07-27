#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shiny)
library(ggplot2)
library(graphics)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(cluster)
library(pheatmap)
library(ComplexHeatmap)
library(circlize)
library(ggfortify)
library(datasets)
library(RColorBrewer)
library(psych)
s<-describe(mtcars)

write.csv(s, "C:/Users/y_du10/OneDrive - Fort Hays State University/Desktop/KU Data Science Program/Courses/Data 824/Week 7/mtcars_sta.csv", row.names=TRUE)

s1<-s%>%select(3:5,8:10)

st1<-t(s1)



coul <- brewer.pal(8, "Set2") 
#coul_1<-brewer.pal(32,"Set2")
mtcars



# Define UI for application that draws a histogram
ui <- 
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    theme = shinytheme("superhero"),
    # Application Title 1
    titlePanel("Data Visualization for Motor Trend Car Road Tests Data"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("xvar4", "Choose a variable for BarPlot of Descriptive Statistics", choices=names(mtcars)),
        selectInput("xvar0", "Choose a Continuous Variable for ScatterPlot:", choices=c("disp","hp","drat","wt","qsec")),
        selectInput("xvar2", "Choose a Categorical Variable for BoxPlot:", choices=c("cyl","vs","am","gear","carb")),
        selectInput("xvar3", "Choose a variable for BarPlot:", choices=names(mtcars)),
        
        selectInput("xvar5", "Choose a variabble for Histogram:", choices=names(mtcars)),
        sliderInput('num',label='Insert Number of Clusters',value = 3,min = 2,max = 10,step = 1),
        hr(),
        helpText("MTCARS Data: Motor Trend Car Road Tests Data in R."),
        helpText("Source: Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411.)")
        
        
        
      ),  # end of sidebarPanel
      
      # Create a spot 
      mainPanel(
        plotOutput("BarPlot0"),
        plotOutput("ggplot2"),
        plotOutput("BoxPlot"),
        plotOutput("BarPlot"),
        plotOutput("Histogram"),
        plotOutput("PCA"),
        plotOutput("PCA1"),
        plotOutput("Hc"),
        plotOutput("Pheatmap"),
        plotOutput("PCA2"),
        plotOutput("CA")
        
        
      ) # end of mainPanel
      
    ) # end of sidebarLayout
    
  ) # end of fluidPage for ui

# Define server logic required to draw a histogram
server <- function(input, output) {
  ################################################################################# 
  #Plot1 --# Render a BarPlot for Column Vars
  
  output$BarPlot0 <- renderPlot({
    
    barplot(st1[,input$xvar4],
            main=paste("Barplot of Descriptive Statistics for Variable", input$xvar4),
            names.arg = rownames(st1),
            col=coul,
            #xlab="Cars",
            ylab=input$xvar4,
            horiz=FALSE
    )# end of barplot function
    
    
  }) # end of renderPlot for Plot1
  
  
  
  
  ####################################################################################  
  #Plot2--- # Render a Scatterplot for Continous Vars
  output$ggplot2<- renderPlot({
    
    mtcars %>% 
      ggplot(aes(x = input$xvar0, y = mpg)) +
      geom_point(position="jitter",     # parameter for controling point position (jitter or nudge)
                 alpha = 100,           # paramater for presenting density of points - alpha
                 size=3,                # parameter for controlling point size
                 shape=21,              # parameter for controlling point shape
                 color = "red",       # parameter for controlling point color
                 fill = "white", 
                 stroke = 1.3 ) +       # Shapes with border
      # geom_smooth(method = "lm", formula = 'y ~ x', se = TRUE, size = 1, color = "red")+
      #  scale_x_continuous(breaks = seq(0,40,2)) +
      #  scale_y_continuous(breaks = seq(0,50,5)) +
      xlab(input$xvar0) +
      ylab("mpg") +
      ggtitle(paste("Scatterplot of MPG v.s.", input$xvar0)) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 25, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5))
    
    
    
    
  }) # end of renderPlot for ggplot2
  #################################################################################### 
  
  ##################################################################################  
  #Plot3 --  # Render a BoxPlot for Categorical Vars
  output$BoxPlot <- renderPlot({
    boxplot(mtcars$mpg~mtcars[,input$xvar2],
            main=paste("Boxplot of MPG v.s. Categorical Var ---",input$xvar2),
            xlab=input$xvar2,
            ylab="mpg"
            
            
    )# end of boxplot function
    
    
  })  #end of renderPlot for Plot3
  
  ################################################################################
  
  #Plot4 --# Render a BarPlot for Column Vars
  
  output$BarPlot <- renderPlot({
  
    barplot(mtcars[,input$xvar3],
            main=paste("Barplot of", input$xvar3, "across Cars" ),
            names.arg = rownames(mtcars),
            col=coul,
            #xlab="Cars",
            ylab=input$xvar3,
            horiz=FALSE,
            las=3
     )# end of barplot function
  
  
    }) # end of renderPlot for Plot4
  
  
  ################################################################################   
  
  ################################################################################
  #Plot5 -- # Render a Histogram for Column Vars
  
  output$Histogram <- renderPlot({
    
    hist(mtcars[,input$xvar5],
         prob=TRUE, 
         xlab=input$xvar5,
         main=paste("Historgram of", input$xvar5)
         
         )  # end of histogram function
    
    
  }) # end of renderPlot for Plot5
  
  
  ###############################################################################
  # Plot 6 -- #Render Kmeans partitioning with different number of clustering      
  
  clust_data <- reactive({
    kmeans(mtcars,input$num)
    
  })# end of renderactive
  
  
  output$PCA<-renderPlot({
    autoplot(clust_data(),
             data=mtcars,
             label=TRUE,
             label.size=3,
             main="Kmeans Partitioning for MTCARS Data"
             
             
             )# end of autoplot function
    
    
    
    
  })  # end of renderPlot
  
 
  
  ################################################################################## 
  ## Plot 7 -- #Render Pam partitioning with different number of clustering  
  clust_data_1 <- reactive({
    pam(mtcars,input$num)
    
  }) # end of renderactive
  
  
  output$PCA1<-renderPlot({
    autoplot(clust_data_1(),
             data=mtcars,
             label=TRUE,
             label.size=3,
             main="PAM Partitioning for MTCARS Data"
             )
    
    
  })  # end of renderPlot
  
  ##################################################################################  
  # Plot 8 -- #Render Hierarchical Clustering with different number of clustering
  
  output$Hc <- renderPlot({
    
    res.hc <-hclust(dist(mtcars), method="ward.D2")
    fviz_dend(res.hc,cex=0.5,k=input$num,palette="jco",
              main = "Hierarchical Cluster Dendrogram for MTCARS Data",
              show_labels = TRUE
              )
    
    
  })  # end of renderPlot
  ###################################################################################   
  # Plot 9 -- #Render Heatmap with different number of clustering
  
  output$Pheatmap<- renderPlot({
    
    pheatmap(t(mtcars),
             #label="var",
             k=input$num, 
             #cuttree_cols=3,
             main="Heatmap Clustering for MTCARS Data"
             )# end of pheatmap function
    
  })  # end of renderPlot
  
  ###################################################################################
  # Plot 10 -- #Render PCA Biplot
  
  
  output$PCA2<- renderPlot({
    
    res.pca <- prcomp(mtcars,  scale = TRUE)
    
    fviz_pca_biplot(res.pca, label ="var", col.ind="cos2") +  theme_minimal() 
    
  })  # end of renderPlot
  
  ##################################################################################
  # Plot 11 -- #Render CA Biplot
  output$CA<- renderPlot({
    
    res.ca<- CA(mtcars, graph=FALSE)
    
    fviz_ca_biplot(res.ca, repel =TRUE) 
    
  })  # end of renderPlot
  
  
  ##############################################################################
  
  
  
  
  
  
}  ##end of function for server




# Run the application 
shinyApp(ui = ui, server = server)
