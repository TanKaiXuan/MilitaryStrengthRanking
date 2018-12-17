library(shiny)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(markdown)
library(RColorBrewer)

MSR_df <- read.csv("2018 Military Strength Ranking.csv")

Africa <- MSR_df %>% filter(Region %in%"Africa")%>%arrange(Pwrindx)
Asia <- MSR_df %>% filter(Region %in%"Asia")%>%arrange(Pwrindx)
Europe <- MSR_df %>% filter(Region %in%"Europe")%>%arrange(Pwrindx)
America <- MSR_df %>% filter(Region %in%"America")%>%arrange(Pwrindx)

PowerIndexTop10 <- MSR_df %>%top_n(-10, Pwrindx)
AfricaTop <- Africa %>%top_n(-10,Pwrindx)
AsiaTop <- Asia%>%top_n(-10,Pwrindx)
EuropeTop <- Europe %>%top_n(-10,Pwrindx)
AmericaTop <-America %>%top_n(-10,Pwrindx)

mapped_data <- joinCountryData2Map(MSR_df, joinCode="NAME", nameJoinColumn = "Country")
colourPalette <- brewer.pal(6,'YlOrRd')

# Manpower: [5:11]
# Airpower: [12:18]
# Land Strength: [19:23]
# Naval Strength: [24:31]

ui <- navbarPage("2018 Military Strength Ranking",
                 tabPanel("Global Data",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "x", label = 'X Variable', choices = names((MSR_df)[,c(2,4:48)]),
                                          selected="Country"),
                              selectInput(inputId = "y", label = 'Y Variable', choices = names((MSR_df)[,c(2,4:48)]),
                                          selected="Total.Population")
                            ),
                            mainPanel(
                              plotOutput(outputId = "mapplot")
                            )
                          )
                 ),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "z", label = 'Factors', choices = names((MSR_df)[c(4,5,9,12,17,19,20,22,24)]),
                                          selected="Country")
                            ),
                            mainPanel(
                              plotOutput(outputId = "mPlot")
                            )
                          )
                 ),
                 tabPanel("Africa",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "a", label = 'Manpower', choices = names((AfricaTop)[5:11])),
                              selectInput(inputId = "b", label = 'Airpower', choices = names((AfricaTop)[12:18])),
                              selectInput(inputId = "c", label = 'Land Strength', choices = names((AfricaTop)[19:23])),
                              selectInput(inputId = "d", label = 'Naval Strength', choices = names((AfricaTop)[24:31]))
                            ),
                            mainPanel(
                              plotOutput(outputId = "manpowerAfrica"),
                              plotOutput(outputId = "airpowerAfrica"),
                              plotOutput(outputId = "landAfrica"),
                              plotOutput(outputId = "navalAfrica")
                            )
                          )
                 ),
                 tabPanel("Asia",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "e", label = 'Manpower', choices = names((AsiaTop)[5:11])),
                              selectInput(inputId = "f", label = 'Airpower', choices = names((AsiaTop)[12:18])),
                              selectInput(inputId = "g", label = 'Land Strength', choices = names((AsiaTop)[19:23])),
                              selectInput(inputId = "h", label = 'Naval Strength', choices = names((AsiaTop)[24:31]))
                            ),
                            mainPanel(
                              plotOutput(outputId = "manpowerAsia"),
                              plotOutput(outputId = "airpowerAsia"),
                              plotOutput(outputId = "landAsia"),
                              plotOutput(outputId = "navalAsia")
                            )
                          )
                 ),
                 tabPanel("Europe",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "i", label = 'Manpower', choices = names((EuropeTop)[5:11])),
                              selectInput(inputId = "j", label = 'Airpower', choices = names((EuropeTop)[12:18])),
                              selectInput(inputId = "k", label = 'Land Strength', choices = names((EuropeTop)[19:23])),
                              selectInput(inputId = "l", label = 'Naval Strength', choices = names((EuropeTop)[24:31]))
                            ),
                            mainPanel(
                              plotOutput(outputId = "manpowerEurope"),
                              plotOutput(outputId = "airpowerEurope"),
                              plotOutput(outputId = "landEurope"),
                              plotOutput(outputId = "navalEurope")
                            )
                          )
                 ),
                 tabPanel("America",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "m", label = 'Manpower', choices = names((AmericaTop)[5:11])),
                              selectInput(inputId = "n", label = 'Airpower', choices = names((AmericaTop)[12:18])),
                              selectInput(inputId = "o", label = 'Land Strength', choices = names((AmericaTop)[19:23])),
                              selectInput(inputId = "p", label = 'Naval Strength', choices = names((AmericaTop)[24:31]))
                            ),
                            mainPanel(
                              plotOutput(outputId = "manpowerAmerica"),
                              plotOutput(outputId = "airpowerAmerica"),
                              plotOutput(outputId = "landAmerica"),
                              plotOutput(outputId = "navalAmerica")
                            )
                          )
                 )
)


server <- function(input, output) {
  
  output$mapplot <- renderPlot({
    ggplot(MSR_df, aes_string(input$x, input$y)) + geom_point()
  })
  
  output$mPlot <- renderPlot({
    if(input$z=="Pwrindx"){
      mapCountryData(mapped_data, nameColumnToPlot=input$z, mapTitle='Global Firepower Factors',colourPalette = rev(colourPalette))
    }else{
      mapCountryData(mapped_data, nameColumnToPlot=input$z, mapTitle='Global Firepower Factors',colourPalette = colourPalette)
    }
  })
  
  #Africa
  output$manpowerAfrica <- renderPlot({
    ggplot(AfricaTop, aes_string(x='Country',y=input$a)) + geom_bar(stat = "identity", fill='orchid1')
  })
  
  output$airpowerAfrica <- renderPlot({
    ggplot(AfricaTop, aes_string(x='Country',y=input$b)) + geom_bar(stat = "identity", fill='skyblue1')
  })
  
  output$landAfrica <- renderPlot({
    ggplot(AfricaTop, aes_string(x='Country',y=input$c)) + geom_bar(stat = "identity", fill='darkseagreen2')
  })
  
  output$navalAfrica <- renderPlot({
    ggplot(AfricaTop, aes_string(x='Country',y=input$d)) + geom_bar(stat = "identity", fill='royalblue1')
  })
  
  #Asia
  output$manpowerAsia <- renderPlot({
    ggplot(AsiaTop, aes_string(x='Country',y=input$e)) + geom_bar(stat = "identity", fill='orchid1')
  })
  
  output$airpowerAsia <- renderPlot({
    ggplot(AsiaTop, aes_string(x='Country',y=input$f)) + geom_bar(stat = "identity", fill='skyblue1')
  })
  
  output$landAsia <- renderPlot({
    ggplot(AsiaTop, aes_string(x='Country',y=input$g)) + geom_bar(stat = "identity", fill='darkseagreen2')
  })
  
  output$navalAsia<- renderPlot({
    ggplot(AsiaTop, aes_string(x='Country',y=input$h)) + geom_bar(stat = "identity", fill='royalblue1')
  })
  
  #Europe
  output$manpowerEurope <- renderPlot({
    ggplot(EuropeTop, aes_string(x='Country',y=input$i)) + geom_bar(stat = "identity", fill='orchid1')
  })
  
  output$airpowerEurope <- renderPlot({
    ggplot(EuropeTop, aes_string(x='Country',y=input$j)) + geom_bar(stat = "identity", fill='skyblue1')
  })
  
  output$landEurope <- renderPlot({
    ggplot(EuropeTop, aes_string(x='Country',y=input$k)) + geom_bar(stat = "identity", fill='darkseagreen2')
  })
  
  output$navalEurope<- renderPlot({
    ggplot(EuropeTop, aes_string(x='Country',y=input$l)) + geom_bar(stat = "identity", fill='royalblue1')
  })
  
  #America
  output$manpowerAmerica <- renderPlot({
    ggplot(AmericaTop, aes_string(x='Country',y=input$m)) + geom_bar(stat = "identity", fill='orchid1')
  })
  
  output$airpowerAmerica <- renderPlot({
    ggplot(AmericaTop, aes_string(x='Country',y=input$n)) + geom_bar(stat = "identity", fill='skyblue1')
  })
  
  output$landAmerica <- renderPlot({
    ggplot(AmericaTop, aes_string(x='Country',y=input$o)) + geom_bar(stat = "identity", fill='darkseagreen2')
  })
  
  output$navalAmerica <- renderPlot({
    ggplot(AmericaTop, aes_string(x='Country',y=input$p)) + geom_bar(stat = "identity", fill='royalblue1')
  })
  
  
}


shinyApp(ui=ui, server=server)
