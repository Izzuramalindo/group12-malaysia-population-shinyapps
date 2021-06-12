library(tidyverse)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(dashboardthemes)
library(stringr)
library(reactable)

df <- read.csv("Malaysia population data.csv")
df_summary <- read.csv("Summary_population.csv")
df_table <- read.csv("Reactable.csv")

# for filters
xyear <- df %>% select(Year) %>% distinct
xcitizen <- df %>% select(Citizen.category) %>% distinct
xethnic <- df %>% select(Ethnic) %>% distinct
xagegroup <- df %>% select(Age.Group) %>% distinct
xgender <- df %>% select(Sex) %>% distinct

# data for data box of population by gender 
box1=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Gender_male=median(Population_by_Gender_male))
#box1
box2=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Gender_female=median(Population_by_Gender_female))

box3=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_bumiputera=median(Population_by_bumiputera))

box4=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Chinese=median(Population_by_Chinese))

box5=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Indians=median(Population_by_Indians))

box6=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Others=median(Population_by_Others))

box7=df_summary %>% 
  filter(xyear==2019)%>% 
  summarise(Population_by_Foreigners=median(Population_by_Foreigners))

# Overview - YoY population changes for citizen vs non citizen
line1=df_summary %>%
  group_by(Population_Year)%>%
  summarise(Population_by_Malaysian_citizens=median(Population_by_Malaysian_citizens))

# Age groups - Population by age groups bar chart 
bar1=df %>%
  group_by(Age.Group)%>%
  summarise(avg_Population_AgeGroup=mean(Population...000.))%>%
  arrange(desc(Age.Group))


# Ethnicity - Population breakdown by ethnicity
bar2=df %>%
  group_by(Ethnic)%>%
  summarise(avg_ethnic_population=sum(Population...000.))%>%
  arrange(desc(Ethnic))


######### ------------------------------------------------#####################
#Dashboard UI

ui <- dashboardPage(
  dashboardHeader(title = "Malaysia Population Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1"),
      menuItem("Population by age groups & ethnic", tabName = "page2"),
      menuItem("Population table", tabName = "page3"), 
      menuItem("Statistical Summary", tabName = "page4"),
      menuItem("Population Trend", tabName = "page5"), 
      menuItem("Population Structure", tabName = "page6")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      tabItem(tabName = "page1", h2("Overview"),
              h5("Breakdown of Malaysian population."),
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(4,infoBoxOutput("info_box1",width = 12))
                         ,column(2,"")
                         ,column(4,infoBoxOutput("info_box2",width = 12))
                         ,column(1,"")
                         ,column(4,infoBoxOutput("info_box3",width = 12))
                         ,column(2,"")
                         ,column(4,infoBoxOutput("info_box4",width = 12))
                         ,column(1,"")
                         ,column(4,infoBoxOutput("info_box5",width = 12))
                         ,column(2,"")
                         ,column(4,infoBoxOutput("info_box6",width = 12))
                         ,column(2,"")
                         ,column(4,infoBoxOutput("info_box7",width = 12))
                         ,column(1,"")
                       ),
                       hr(),
                       fluidRow(column(12,plotOutput("linechart1"))
                       )
                )
              )
      ),
      tabItem(tabName = "page2", h2("Population by Age Groups & Ethnic"), 
              h5("Summary of population by age groups and ethnic"),
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(12,plotlyOutput("barchart1")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,plotlyOutput("barchart2")
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "page3", h2("Population table"), 
              h5("Population data table"),
              fluidRow(
                reactableOutput("table1")
              )
      ),
      tabItem(tabName = 'page4', h2('Statistical Summary'),
              
              pageWithSidebar(
                headerPanel("Summary of Malaysia Population"), 
                sidebarPanel(
                  selectInput('Gender_gr', 'Gender', unique(cy_df$Sex), selected = 'Total'),
                  selectInput('Age_Group_gr', 'Age Group', unique(cy_df$Age_Group), selected = 'Total'),
                  selectInput('Ethnic_gr', 'Ethnic', unique(cy_df$Ethnic), selected = 'Total Malaysian citizens'),
                  submitButton('Submit')
                ), 
                mainPanel(
                  h3('The following shows the statistical summary of selected range:'), 
                  h4('Message:'), 
                  verbatimTextOutput('Message_gr'),
                  h4('Observations:'), 
                  verbatimTextOutput('Observations_gr'), 
                  h4('Maximum number:'), 
                  verbatimTextOutput('Maximum_number_gr'), 
                  h4('Minimum number:'), 
                  verbatimTextOutput('Minimum_number_gr'), 
                  h4('Median:'), 
                  verbatimTextOutput('Median_gr'), 
                  h4('Mean:'), 
                  verbatimTextOutput('Mean_gr'), 
                  h4('First Quartile:'), 
                  verbatimTextOutput('First_Quartile_gr'), 
                  h4('Third Quartile:'), 
                  verbatimTextOutput('Third_Quartile_gr')
                )
              )
              
              ),
      
      tabItem(tabName = 'page5', h2('Population Trend'),
              
              pageWithSidebar(
                headerPanel("Population Trend of Malaysia"), 
                sidebarPanel(
                  selectInput('PS_Ethnic', 'Ethnic', unique(cy_df$Ethnic), selected = 'Total Malaysia Population'),
                  selectInput('PS_Gender', 'Gender', unique(cy_df$Sex), selected = 'Total'),
                  selectInput('PS_Age_Group', 'Age Group', unique(cy_df$Age_Group), selected = 'Total'),
                  submitButton('Submit')
                ), 
                mainPanel(
                  h3('The following shows the bar plot of selected population structure:'), 
                  plotOutput('Pop_Structure')
                  )
                )
              ), 
      
      tabItem(tabName = 'page6', h2('Population Structure'),
              
              pageWithSidebar(
                
                headerPanel("Population Structure"), 
                sidebarPanel(
                  selectInput('TPS_Fill', 'Filling', colnames(cy_df)[c(-1,-5)], selected = 'Sex'),
                  selectInput('TPS_Facet', 'Faceting', c(colnames(cy_df)[c(-1,-5)], 'None'), selected = 'Sex'),
                  #selectInput('TPS_Age_Group', 'Age Group', unique(df$Age_Group), selected = 'Total'),
                  submitButton('Submit')
                ), 
                mainPanel(
                  h3('The following shows the bar plot of selected population structure:'), 
                  h4('Message:'), 
                  verbatimTextOutput('TPS_Message_gr'),
                  plotOutput('Total_Pop_Structure')
                )
                
              )
      )
      
      )
    )
  )
######### ------------------------------------------------#####################
# Server

server <- function(input, output) {
  # Overview output info box
  output$info_box1 <- renderInfoBox({
    infoBox("Male population ('000)", box1,icon = icon("male"), color='blue')
  })
  
  output$info_box2 <- renderInfoBox({
    infoBox("Female population ('000)", box2,icon = icon("female"), color='fuchsia')
  })  
  
  output$info_box3 <- renderInfoBox({
    infoBox("Bumiputera ('000)", box3,icon = icon("user"), color='green')
  }) 
  
  output$info_box4 <- renderInfoBox({
    infoBox("Chinese ('000)", box4,icon = icon("user"), color='red')
  }) 
  
  output$info_box5 <- renderInfoBox({
    infoBox("Indians ('000)", box5,icon = icon("user"), color='yellow')
  }) 
  
  output$info_box6 <- renderInfoBox({
    infoBox("Others ('000)", box6,icon = icon("user"), color='teal')
  }) 
  
  output$info_box7 <- renderInfoBox({
    infoBox("Foreigners ('000)", box7,icon = icon("globe"), color='purple')
  }) 
  
  # Overview - line chart of YoY changes in Malaysian citizen population
  output$linechart1 <- renderPlot({
    ggplot(data=line1, aes(x=Population_Year, y=Population_by_Malaysian_citizens)) +
      geom_line(size=3, color=rgb(99, 85, 140,maxColorValue = 255))+
      geom_point()+
      labs(title="Malaysian citizen population growth (2010-2019)",
           y = "Population ('000)", 
           x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=12,face='bold'),
            legend.position = "none")+
      geom_label(aes(label=Population_by_Malaysian_citizens))
  })
  
  # Age group - bar charts 1 of age group
  output$barchart1 <- renderPlotly({
    gb<-ggplot(data=bar1, aes(x=reorder(str_wrap(Age.Group,width=30),Age.Group), 
                              y=avg_Population_AgeGroup, 
                              fill=Age.Group)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Average population by age groups (2010-2019)",
           y = "Population('0000)", 
           x = "Age Group")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=avg_Population_AgeGroup),size=3)+
      aes(text=paste("</br> Age Group: ",Age.Group,
                     "</br> Population: ",avg_Population_AgeGroup))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })   
  
  # Ethnicity - bar chart 2 of population by ethnic 
  output$barchart2 <- renderPlotly({
    gb <- ggplot(data=bar2, aes(x=reorder(str_wrap(Ethnic,width = 30),Ethnic), 
                                y=avg_ethnic_population, 
                                fill=Ethnic)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title = "Average population breakdown by ethnicity (2010-2019)",
           y = "Population ('000)", 
           x = "Ethnic")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text = element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label = avg_ethnic_population),size=3)+
      aes(text=paste("</br> Ethnic: ",Ethnic,
                     "</br> Population: ",avg_ethnic_population))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })
  
  # Details - data table
  output$table1 <- renderReactable({
    reactable(df_table, filterable = TRUE, minRows=26, defaultPageSize = 26)
  })
  
  # Statistical Summary
  output$Message_gr = renderPrint({paste('You have selected observations with ', input$Gender_gr, ' gender, ', input$Age_Group_gr, ' age group, and ', input$Ethnic_gr, ' ethnic group !', sep = '')})
  output$Observations_gr = renderPrint({select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)})
  #output$Maximum_number_gr = renderPrint({paste('The maximum for total population and population growth is ', max(na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)[,5])), ' in ',, sep = '')})
  output$Maximum_number_gr = renderPrint({
    temp = na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate))
    paste('The maximum for total population and population growth is ', max(temp[,5]), ' and ', round(max(temp[,6]),1), sep = '')
    })
  output$Minimum_number_gr = renderPrint({
    temp = (na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)))
    paste('The minimum for total population and population growth is ', min(temp[,5]), ' and ', round(min(temp[,6]),1), sep = '')
    })
  output$Median_gr = renderPrint({
    temp = (na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)))
    paste('The median for total population and population growth is ', median(temp[,5]), ' and ', round(median(temp[,6]),1), sep = '')
    })
  output$Mean_gr = renderPrint({
    temp = (na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)))
    paste('The mean for total population and population growth is ', mean(temp[,5]), ' and ', round(mean(temp[,6]),1), sep = '')
    })
  output$First_Quartile_gr = renderPrint({
    temp = (na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)))
    paste('The first quartile for total population and population growth is ', quantile(temp[,5], 0.25), ' and ', round(quantile(temp[,6], 0.25),1), sep = '')
  })
  output$Third_Quartile_gr = renderPrint({
    temp = (na.omit(select(mutate(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr), Lag = lag(filter(cy_df, Sex == input$Gender_gr, Age_Group == input$Age_Group_gr, Ethnic == input$Ethnic_gr)$Population_K), Growth_Rate = Population_K - Lag), Year, Sex, Age_Group, Ethnic, Population_K, Growth_Rate)))
    paste('The third quartile for total population and population growth is ', quantile(temp[,5], 0.75), ' and ', round(quantile(temp[,6], 0.75),1), sep = '')
  })
  output$Pop_Structure = renderPlot({ggplot(filter(cy_df, Age_Group == input$PS_Age_Group, Ethnic == input$PS_Ethnic, Sex == input$PS_Gender), aes(x = Year, y = Population_K)) + geom_col()})
  
  output$Total_Pop_Structure = renderPlot({
    
    if (input$TPS_Fill == 'Sex' & input$TPS_Facet == 'Age_Group'){
      ggplot(filter(cy_df, Ethnic == 'Total Malaysia Population', Age_Group != 'Total', Sex != 'Total'), aes(x = Year, y = Population_K, fill = Sex)) + geom_col() + facet_wrap(~ Age_Group)
    }else if (input$TPS_Fill == 'Sex' & input$TPS_Facet == 'Ethnic'){
      ggplot(filter(cy_df, Ethnic %in% c('Bumiputera', 'Chinese', 'Indians', 'Others'), Age_Group == 'Total', Sex != 'Total'), aes(x = Year, y = Population_K, fill = Sex)) + geom_col() + facet_wrap(~ Ethnic)
    }else if (input$TPS_Fill == 'Age_Group' & input$TPS_Facet == 'Sex'){
      ggplot(filter(cy_df, Ethnic == 'Total Malaysia Population', Age_Group != 'Total', Sex != 'Total'), aes(x = Year, y = Population_K, fill = Age_Group)) + geom_col() + facet_wrap(~ Sex)
    }else if (input$TPS_Fill == 'Age_Group' & input$TPS_Facet == 'Ethnic'){
      ggplot(filter(cy_df, Ethnic %in% c('Bumiputera', 'Chinese', 'Indians', 'Others'), Age_Group != 'Total', Sex == 'Total'), aes(x = Year, y = Population_K, fill = Age_Group)) + geom_col() + facet_wrap(~ Ethnic)
    }else if (input$TPS_Fill == 'Ethnic' & input$TPS_Facet == 'Age_Group'){
      ggplot(filter(cy_df, Ethnic %in% c('Bumiputera', 'Chinese', 'Indians', 'Others'), Age_Group != 'Total', Sex == 'Total'), aes(x = Year, y = Population_K, fill = Ethnic)) + geom_col() + facet_wrap(~ Age_Group)
    }else if (input$TPS_Fill == 'Ethnic' & input$TPS_Facet == 'Sex'){
      ggplot(filter(cy_df, Ethnic %in% c('Bumiputera', 'Chinese', 'Indians', 'Others'), Age_Group == 'Total', Sex != 'Total'), aes(x = Year, y = Population_K, fill = Ethnic)) + geom_col() + facet_wrap(~ Sex)
    }else if (input$TPS_Fill == input$TPS_Facet){
      
    }else if(input$TPS_Fill == 'Age_Group' & input$TPS_Facet == 'None'){
      ggplot(filter(cy_df, Ethnic == 'Total Malaysia Population', Age_Group != 'Total', Sex == 'Total'), aes(x = Year, y = Population_K, fill = Age_Group)) + geom_col()
    }else if(input$TPS_Fill == 'Ethnic' & input$TPS_Facet == 'None'){
      ggplot(filter(cy_df, Ethnic %in% c('Bumiputera', 'Chinese', 'Indians', 'Others'), Age_Group == 'Total', Sex == 'Total'), aes(x = Year, y = Population_K, fill = Ethnic)) + geom_col()
    }else if(input$TPS_Fill == 'Sex' & input$TPS_Facet == 'None'){
      ggplot(filter(cy_df, Ethnic == 'Total Malaysia Population', Age_Group != 'Total', Sex != 'Total'), aes(x = Year, y = Population_K, fill = Sex)) + geom_col()
    }else{
      
    }
    
  })
  
}

##############################
# Run the application 
shinyApp(ui = ui, server = server)

