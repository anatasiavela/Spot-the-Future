# load required libraries
library("shiny")
library("RColorBrewer")
library("wordcloud")
library("wordcloud2")

# read in csv files of songs
top_2012 <- read.csv("data/top_songs_2012.csv")
top_2013 <- read.csv("data/top_songs_2013.csv")
top_2014 <- read.csv("data/top_songs_2014.csv")
top_2015 <- read.csv("data/top_songs_2015.csv")
top_2016 <- read.csv("data/top_songs_2016.csv")

# remove id column
top_2012$id <- NULL
top_2013$id <- NULL
top_2014$id <- NULL
top_2015$id <- NULL
top_2016$id <- NULL

# create vector of file names
years <- c("2012", "2013", "2014", "2015", "2016")

# create vector of feature names
features <- colnames(top_2012)[3:15]

# create rescale() to rescale data
rescale <- function(x, xmin, xmax) {
  100 * (x - xmin) / (xmax - xmin)
}

# create get_data() to get data with chosen year
get_data <- function(x) {
  if (x == "2012") {
    top_2012
  } else if (x == "2013") {
    top_2013
  } else if (x == "2014") {
    top_2014
  } else if (x == "2015") {
    top_2015
  } else if (x == "2016") {
    top_2016
  }
}

# define UI for application
ui <- fluidPage(
  
  # application title
  titlePanel("Song Visualizer"),
  
  # sidebar with conditional panels for multiple tabs
  sidebarLayout(
    sidebarPanel(
      
      # first tab
      conditionalPanel(condition = "input.tabselected==1",
                       selectInput("year1", "Year:",
                                   years, selected = "2016"),
                       selectInput("feature1", "Feature:",
                                   features, selected = "speechiness"),
                       sliderInput("max", "Maximum Number of Songs:",
                                   min = 1,  max = 100,  value = 100)),
      
      # second tab
      conditionalPanel(condition = "input.tabselected==2",
                       selectInput("year2", "Year:",
                                   years, selected = "2016"),
                       selectInput("feature2", "Feature:",
                                   features, selected = "speechiness")),

      # third tab
      conditionalPanel(condition = "input.tabselected==3",
                       selectInput("year3", "Year:",
                                   years, selected = "2016"),
                       selectInput("x_var", "X-Axis Variable:",
                                   features, selected = "speechiness"),
                       selectInput("y_var", "Y-Axis Variable:",
                                   features, selected = "energy"),
                       numericInput('clusters', 'Cluster Count:',
                                    min = 1,
                                    max = 9,
                                    value = 5))
      ),
    
    # show outputs for each tab
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  # first tab
                  tabPanel("Word Cloud V1", value = 1,
                           plotOutput("wordcloud1")),
                  
                  # second tab
                  tabPanel("Word Cloud V2", value = 2,
                           wordcloud2Output("wordcloud2")),

                  # third tab
                  tabPanel("K-Means Cluster Plot", value = 3,
                           plotOutput("kplot",
                                      hover = hoverOpts(
                                        id = "plot_hover")),
                           h4("Song Info:"),
                           verbatimTextOutput("hover_info")),
                  
                  id = "tabselected"))
  )
)

# define server logic
server <- function(input, output) {
  
  # plot word cloud in first tab
  output$wordcloud1 <- renderPlot({
    
    # get data of chosen year
    data = get_data(input$year1)
    
    # get chosen feature
    feature = input$feature1
    
    # convert loudness into positive values
    lowest = min(data$loudness)
    data$loudness = data$loudness - lowest
    
    # rescale data
    for (i in 3:15) {
      minimum = min(data[ , i])
      maximum = max(data[ , i])
      data[ , i] = rescale(data[ , i], minimum, maximum)
    }
    
    # plot word cloud
    wordcloud(words = data[ , "song"], freq = data[ , feature],
              min.freq = 0, max.words = input$max, scale = c(3, 0.3),
              colors = brewer.pal(6, "Dark2"), fixed.asp = TRUE)
  })
  
  # plot word cloud in second tab
  output$wordcloud2 <- renderWordcloud2({
    
    # get data of chosen year
    data = get_data(input$year2)
    
    # get chosen feature
    feature = input$feature2
    
    # convert loudness into positive values
    lowest = min(data$loudness)
    data$loudness = data$loudness - lowest
    
    # rescale data
    for (i in 3:15) {
      minimum = min(data[ , i])
      maximum = max(data[ , i])
      data[ , i] = rescale(data[ , i], minimum, maximum)
    }
    
    # create data frame of songs and chosen feature
    data <- data.frame(data[ , "song"], data[ , feature])
    
    # plot word cloud
    wordcloud2(data, size = 0.35)
  })
  
  # plot k-means cluster plot in third tab
  output$kplot <- renderPlot({
    
    # get data of chosen year with chosen features
    data = get_data(input$year3)
    data = data[ , c(input$x_var, input$y_var)]
    
    # find clusters
    clusters = kmeans(data, input$clusters)
    
    # create color scheme
    palette(c("red", "deepskyblue", "green", "yellow2",
              "orange", "purple", "pink", "forestgreen", "grey"))
    
    # create k-means plot
    plot(data, col = clusters$cluster, pch = 20, cex = 3)
    
    # plot cluster points
    points(clusters$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  # generate song info in third tab
  output$hover_info <- renderPrint({
    
    # get point being hovered over
    point = input$plot_hover
    
    # get data of chosen year
    data = get_data(input$year3)
    data = data[ , c(input$x_var, input$y_var, "song", "artist")]
    
    # find song in data that is closest to hovered point
    for (i in 1:100) {
      if (all.equal.numeric(data[i, 1], point$x, tolerance = 0.03) == TRUE &
          all.equal.numeric(data[i, 2], point$y, tolerance = 0.03) == TRUE) {
        data = data[i, ]
        break
      }
    }
    
    # add NA values for hovering over no song
    empty <- rep(NA, ncol(data))
    if (nrow(data) > 1) {
      data <- rbind(empty, data)
    }
    
    # paste song info
    cat(paste(" Song: ", data$song[1], "\n", sep = ""),
        paste("Artist: ", data$artist[1], "\n", sep = ""),
        paste(input$x_var, ": ", data[1, 1], "\n", sep = ""),
        paste(input$y_var, ": ", data[1, 2], sep = "")
        )
  })
}

# run the application
shinyApp(ui = ui, server = server)
