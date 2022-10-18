library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(usmap)
library(DT)
library(gganimate)
library(ggthemes)
library(ggrepel)
library(forcats)
library(scales)



Crime <-readr::read_csv("../data/CrimeRate_data2.csv")
Crime <- Crime %>%
  mutate(Year = as.numeric(Year), 
         Population = as.numeric(Population),
         Prison_population = as.numeric(Prison_population)) 
# %>% 
#   group_by(State) %>% 
#   arrange(Year, .by_group = TRUE)

States <- data.frame(state = unique(Crime$State)) %>% 
  pivot_wider(names_from = state, values_from = state)

Crime2 <- Crime %>% select(State, Year, Population, Total_crime, Violent_crime, Property_crime, Prison_population, GDP, Gun_number, Police_number)
Crime3 <- Crime %>% select(-State, -Year)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Crime Rate"),
                navbarPage("",
                           # tabPanel("Motivation"
                           #          ),
                           # tabPanel("Table",
                           #          dataTableOutput("table")
                           # ),
                           tabPanel("Plot",
                                    tabsetPanel(
                                      
                                      tabPanel("One State General",
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(
                                                   varSelectInput("OSGP_state",
                                                                  "State?",
                                                                  data = States),
                                                   varSelectInput("OSGP_var",
                                                                  "Variable?",
                                                                  data = Crime %>% 
                                                                    select(-c(State, Year)),
                                                                  selected = "Total_crime"),
                                                   sliderInput("range", "Year", min = 2008, max = 2019, value = c(2008, 2019))
                                                 ), # end of sidebarPanel
                                                 
                                                 mainPanel(
                                                   fluidRow(
                                                     column(6, plotOutput("OSGP_L") ),# the plot combine line and bar
                                                     column(6, plotOutput("OSGP_B"))
                                                   )
                                                   
                                                 ) # end of mainPanel
                                                 
                                               ) # end of sidebarLayout
                                      ), # end of One State General Plot inside tabPanel
                                      
                                      tabPanel("One State Specific",
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   varSelectInput("OSSCP_state",
                                                                  "State?",
                                                                  data = States),
                                                   varSelectInput("OSSCP_crime",
                                                                  "Crime Type?",
                                                                  data = Crime %>% 
                                                                    select(Violent_crime, Property_crime)),
                                                   conditionalPanel("input.OSSCP_crime == 'Violent_crime'",
                                                                    varSelectInput("OSSCP_specific",
                                                                                   "Specific?",
                                                                                   data = Crime[6:9])),
                                                   conditionalPanel("input.OSSCP_crime == 'Property_crime'",
                                                                    varSelectInput("OSSCP_specific2",
                                                                                   "Specific?",
                                                                                   data = Crime[11:13])),
                                                   sliderInput("range2", "Year", min = 2008, max = 2019, value = c(2008, 2019))
                                                 ), # end of sideabrPanel
                                                 
                                                 mainPanel(
                                                   fluidRow(
                                                     column(6, plotOutput("OSSCP_L") ),# the plot combine line and bar
                                                     column(6, plotOutput("OSSCP_B"))
                                                   ),
                                                   plotOutput("OSSCP_pie")
                                                   # plotOutput("OSSCP_LB")
                                                 ) # end of mainPanel
                                                 
                                               )), # end of tabPanel
                                      
                                      tabPanel("Multiple States General",
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(
                                                   # select input states upto 5.
                                                   numericInput("MSGP_state_n",
                                                                "How many states you want to compare?(up to 5)",
                                                                value = 2,
                                                                min = 2,
                                                                max = 5),
                                                   
                                                   conditionalPanel("input.MSGP_state_n == 2",
                                                                    varSelectInput("MSGP_state1",
                                                                                   "State1",
                                                                                   data = States,
                                                                                   selected = States[1]),
                                                                    varSelectInput("MSGP_state2",
                                                                                   "State2",
                                                                                   data = States,
                                                                                   selected = States[2])),
                                                   
                                                   conditionalPanel("input.MSGP_state_n == 3",
                                                                    varSelectInput("MSGP_state1",
                                                                                   "State1",
                                                                                   data = States,
                                                                                   selected = States[1]),
                                                                    varSelectInput("MSGP_state2",
                                                                                   "State2",
                                                                                   data = States,
                                                                                   selected = States[2]),
                                                                    varSelectInput("MSGP_state3",
                                                                                   "State3",
                                                                                   data = States,
                                                                                   selected = States[3])),
                                                   
                                                   conditionalPanel("input.MSGP_state_n == 4",
                                                                    varSelectInput("MSGP_state1",
                                                                                   "State1",
                                                                                   data = States,
                                                                                   selected = States[1]),
                                                                    varSelectInput("MSGP_state2",
                                                                                   "State2",
                                                                                   data = States,
                                                                                   selected = States[2]),
                                                                    varSelectInput("MSGP_state3",
                                                                                   "State3",
                                                                                   data = States,
                                                                                   selected = States[3]),
                                                                    varSelectInput("MSGP_state4",
                                                                                   "State4",
                                                                                   data = States,
                                                                                   selected = States[4])),
                                                   
                                                   conditionalPanel("input.MSGP_state_n == 5",
                                                                    varSelectInput("MSGP_state1",
                                                                                   "State1",
                                                                                   data = States,
                                                                                   selected = States[1]),
                                                                    varSelectInput("MSGP_state2",
                                                                                   "State2",
                                                                                   data = States,
                                                                                   selected = States[2]),
                                                                    varSelectInput("MSGP_state3",
                                                                                   "State3",
                                                                                   data = States,
                                                                                   selected = States[3]),
                                                                    varSelectInput("MSGP_state4",
                                                                                   "State4",
                                                                                   data = States,
                                                                                   selected = States[4]),
                                                                    varSelectInput("MSGP_state5",
                                                                                   "State5",
                                                                                   data = States,
                                                                                   selected = States[5])),
                                                   
                                                   varSelectInput("MSGP_var1",
                                                                  "X Variable",
                                                                  data = Crime %>% 
                                                                    select(-c(State, Year)),
                                                                  selected = "GDP"),
                                                   varSelectInput("MSGP_var2",
                                                                  "Y Variable",
                                                                  data = Crime %>% 
                                                                    select(-c(State, Year)),
                                                                  selected = "Total_crime")
                                                 ), # end of sidebarPanel
                                                 
                                                 mainPanel(
                                                   plotOutput("MSGP_LB")
                                                 ) # end of mainPanel
                                               ) # end of sidebarLayout
                                      ) # end of tabPanel
                                    ) # end of inside tabsetPanel
                           ), # end of outside tabPanel
                           tabPanel("Map",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("Year","Select a Year",c(sort(unique(Crime$Year),decreasing=TRUE))),
                                        varSelectInput("CrimeType", "Crime Type", data = Crime %>% select(Total_crime, Violent_crime, Property_crime), selected = "Total_crime")
                                        ),
                                      
                                      mainPanel(
                                        plotOutput("Map"),
                                        dataTableOutput("Map_df")
                                        ))
                           ),
                           tabPanel("Model",
                                    tabsetPanel(
                                      tabPanel("Regression",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   varSelectInput("Y", "Response variable:", 
                                                                  data = Crime %>% select(-State, -Prison_population, -Gun_number, -Police_number, -GDP, -Year),
                                                                  selected = "Total_crime"),
                                                   checkboxInput("logY", "Log Transform?"),
                                                   varSelectInput("X", "Explanatory variable:", data = Crime %>% select(Prison_population, Gun_number, Police_number, GDP, Year), 
                                                                  selected = "Prison_population"),
                                                   checkboxInput("logX", "Log Transform?")
                                                 ),
                                                 mainPanel(
                                                   
                                                   verbatimTextOutput("regsummary"),
                                                   plotOutput("regscatter"),
                                                   plotOutput("regresidual"),
                                                   plotOutput("regQQ")
                                                   
                                                   
                                                   
                                                   
                                                 )
                                               )
                                      ),
                                      tabPanel("t.test",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   varSelectInput("tteststate1", "State 1", data = States),
                                                   varSelectInput("tteststate2", "State 2", data = States),
                                                   varSelectInput("ttestvar", "Variable", data = Crime %>% select(-c(State, Year))),
                                                   sliderInput("range3", "Year", min = 2008, max = 2019, value = c(2008, 2019))
                                                   
                                                 ),
                                                 mainPanel(
                                                   
                                                   verbatimTextOutput("ttestsummary")
                                                   
                                                   
                                                   
                                                   
                                                 )
                                               )
                                      )
                                    ) #end of tabset
                                    
                           ),
                           tabPanel("Table",
                                    dataTableOutput("table")
                           )
                           
                )
                
                
)

server <- function(input, output, session) {
  
  
  # plot for One State General
  
  
  my_range <- reactive({
    cbind(input$range[1],input$range[2])
  })
  my_range2 <- reactive({
    cbind(input$range2[1],input$range2[2])
  })
  
  output$OSGP_L <- renderPlot({
    Crime %>% 
      filter(State == input$OSGP_state,
             Year >= my_range()[1],
             Year <= my_range()[2]) %>% 
      ggplot() +
      # geom_col(aes(x = Year, y = !!input$OSGP_var),
      #          color = "darkblue", 
      #          fill = "white") +
      geom_line(aes(x = Year, y = Violent_crime, color = "black"),
                size = 1) +
      geom_line(aes(x = Year, y = Property_crime, color = "red"), 
                size = 1) +
      scale_color_discrete(name = "Y series", labels = c("Y2", "Y1")) +
      scale_color_discrete(name = "Y series", labels = c("Violent Crime", "Property Crime")) +
      ylab("Crime Rate") +
      ggtitle("Two Types Crime Rate Change in 2008 - 2018") +
      scale_x_continuous(breaks=seq(2008, 2018, 2))
    # +
    #   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Vio. and Prop."))
  })
  
  output$OSGP_B <- renderPlot({
    Crime %>% 
      filter(State == input$OSGP_state,
             Year >= my_range()[1],
             Year <= my_range()[2]) %>% 
      ggplot() +
      geom_col(aes(x = Year, y = !!input$OSGP_var),
               color = "darkblue",
               fill = "white")+
      ggtitle(paste(input$OSGP_var, "change in 2008-2018")) +
      scale_x_continuous(breaks=seq(2008, 2018, 2))
    # geom_line(aes(x = Year, y = Violent_crime), 
    #           color = "black",
    #           size = 1) +
    # geom_line(aes(x = Year, y = Property_crime), 
    #           color = "red",
    #           size = 1) 
    # +
    #   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Vio. and Prop."))
  })
  
  
  # pie plot for One State Specific
  output$OSSCP_pie <- renderPlot({
    Crime %>% 
      filter(State == input$OSSCP_state,
             Year >= my_range2()[1],
             Year <= my_range2()[2]) -> Crime_OSSCP
    
    if (!!input$OSSCP_crime == "Violent_crime") {
      Crime_OSSCP[6:9] %>% 
        pivot_longer(cols = everything(),
                     names_to = "Crime",
                     values_to = "value") %>% 
        group_by(Crime) %>%
        summarise(value = sum(value)) %>%
        arrange(desc(value)) %>%
        mutate(prop = percent(value / sum(value))) %>%
        ggplot(aes(x = "", y = value, fill = fct_inorder(Crime))) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
        guides(fill = guide_legend(title = "Crime"))
    } else {
      Crime_OSSCP[11:13] %>% 
        pivot_longer(cols = everything(),
                     names_to = "Crime",
                     values_to = "value") %>% 
        group_by(Crime) %>%
        summarise(value = sum(value)) %>%
        arrange(desc(value)) %>%
        mutate(prop = percent(value / sum(value))) %>%
        ggplot(aes(x = "", y = value, fill = fct_inorder(Crime))) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
        guides(fill = guide_legend(title = "Crime")) 
    }
  })
  
  # line+col plot for One State Specific
  output$OSSCP_L <- renderPlot({
    Crime %>% 
      filter(State == input$OSSCP_state,
             Year >= my_range2()[1],
             Year <= my_range2()[2]) -> Crime_OSSCP
    
    if (!!input$OSSCP_crime == "Violent_crime") {
      Crime_OSSCP %>% 
        ggplot() +
        # geom_col(aes(x = Year, y = !!input$OSSCP_specific),
        #          color = "darkblue",
        #          fill = "white") +
        geom_line(aes(x = Year, y = Violent_crime)) +
        ylab("Violent Crime Rate") +
        scale_x_continuous(breaks=seq(2008, 2018, 2))
    } else {
      Crime_OSSCP %>% 
        ggplot() +
        # geom_col(aes(x = Year, y = !!input$OSSCP_specific2),
        #          color = "darkblue",
        #          fill = "white") +
        geom_line(aes(x = Year, y = Property_crime)) +
        ylab("Property Crime Rate")+
        scale_x_continuous(breaks=seq(2008, 2018, 2))
    }
  })
  
  output$OSSCP_B <- renderPlot({
    Crime %>% 
      filter(State == input$OSSCP_state,
             Year >= my_range2()[1],
             Year <= my_range2()[2]) -> Crime_OSSCP
    
    if (!!input$OSSCP_crime == "Violent_crime") {
      Crime_OSSCP %>% 
        ggplot() +
        geom_col(aes(x = Year, y = !!input$OSSCP_specific),
                 color = "darkblue",
                 fill = "white") +
        ylab("Violent Crime Rate") +
        scale_x_continuous(breaks=seq(2008, 2018, 2))
      # geom_line(aes(x = Year, y = Violent_crime))
    } else {
      Crime_OSSCP %>% 
        ggplot() +
        geom_col(aes(x = Year, y = !!input$OSSCP_specific2),
                 color = "darkblue",
                 fill = "white") +
        ylab("Property Crime Rate") +
        scale_x_continuous(breaks=seq(2008, 2018, 2))
      # geom_line(aes(x = Year, y = Property_crime))
    }
  })
  
  # animate for Multiple States General.
  output$MSGP_LB <- renderImage({
    if (input$MSGP_state_n == 2){
      if (input$MSGP_state1 == "ALABAMA" &
          input$MSGP_state2 == "ALASKA"  &
          input$MSGP_var1 == "GDP" &
          input$MSGP_var2 == "Total_crime") {
        list(src = "../www/2STATE.gif",
             contentType = 'image/gif')
      } else {
        p = Crime %>% 
          filter(State == input$MSGP_state1 | 
                   State == input$MSGP_state2) %>% 
          ggplot(aes(x = !!input$MSGP_var1,
                     y = !!input$MSGP_var2,
                     color = State,
                     size = Population,
                     frame = Year)) +
          geom_point() +
          transition_states(Year, transition_length = 1) +
          ggtitle('Year showing {closest_state}',
                  subtitle = 'Frame {frame} of {nframes}, Size = Population')
        anim_save("outfile.gif", animate(p))
        list(src = "outfile.gif",
             contentType = 'image/gif')
      }
    } else if (input$MSGP_state_n == 3) {
      p = Crime %>% 
        filter(State == input$MSGP_state1 | 
                 State == input$MSGP_state2 |
                 State == input$MSGP_state3) %>% 
        ggplot(aes(x = !!input$MSGP_var1,
                   y = !!input$MSGP_var2,
                   color = State,
                   size = Population,
                   frame = Year)) +
        geom_point() +
        transition_states(Year, transition_length = 1) +
        ggtitle('Year showing {closest_state}',
                subtitle = 'Frame {frame} of {nframes}, Size = Population')
      anim_save("outfile.gif", animate(p))
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
    } else if (input$MSGP_state_n == 4) {
      p = Crime %>% 
        filter(State == input$MSGP_state1 |
                 State == input$MSGP_state2 |
                 State == input$MSGP_state3 |
                 State == input$MSGP_state4) %>% 
        ggplot(aes(x = !!input$MSGP_var1,
                   y = !!input$MSGP_var2,
                   color = State,
                   size = Population,
                   frame = Year)) +
        geom_point() +
        transition_states(Year, transition_length = 1) +
        ggtitle('Year showing {closest_state}',
                subtitle = 'Frame {frame} of {nframes}, Size = Population')
      anim_save("outfile.gif", animate(p))
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
    } else if (input$MSGP_state_n == 5) {

      p = Crime %>% 
        filter(State == input$MSGP_state1 |
                 State == input$MSGP_state2 |
                 State == input$MSGP_state3 |
                 State == input$MSGP_state4 |
                 State == input$MSGP_state5) %>% 
        ggplot(aes(x = !!input$MSGP_var1,
                   y = !!input$MSGP_var2,
                   color = State,
                   size = Population,
                   frame = Year)) +
        geom_point() +
        transition_states(Year, transition_length = 1) +
        ggtitle('Year showing {closest_state}',
                subtitle = 'Frame {frame} of {nframes}, Size = Population')
      anim_save("outfile.gif", animate(p))
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
    }  
  })
  
  
  # plot map
  output$Map<-renderPlot({
    yeardata= Crime%>%
      filter(Year==input$Year)%>%
      rename(state=State)
    yeardata= yeardata[complete.cases(yeardata), ]
    if(input$CrimeType == "Total_crime"){
    plot_usmap(data=yeardata ,values = "Total_crime", color = "blue",labels = TRUE) + 
      scale_fill_continuous(low = "white", high = "red", 
                            name = "Total_crime", 
                            label = scales::comma)+
      labs(title = "US States",
           subtitle = paste0("Total crime number by state")) + 
      theme(panel.background = element_rect(color = "black", fill = "white")) +
      theme(legend.position = "right")
    }else if(input$CrimeType == "Violent_crime"){
      plot_usmap(data=yeardata,values = "Violent_crime", color = "blue",labels = TRUE) + 
        scale_fill_continuous(low = "white", high = "red", 
                              name = "Violent_crime", 
                              label = scales::comma)+
        labs(title = "US States",
             subtitle = paste0("Violent crime number by state")) + 
        theme(panel.background = element_rect(color = "black", fill = "white")) +
        theme(legend.position = "right")
    }else{
      plot_usmap(data=yeardata,values = "Property_crime", color = "blue",labels = TRUE) + 
        scale_fill_continuous(low = "white", high = "red", 
                              name = "Property_crime", 
                              label = scales::comma)+
        labs(title = "US States",
             subtitle = paste0("Property crime number by state")) + 
        theme(panel.background = element_rect(color = "black", fill = "white")) +
        theme(legend.position = "right")
    }
  })
  
  output$Map_df <- renderDataTable({
    if(input$CrimeType == "Total_crime"){
    DT::datatable(Crime %>% filter(Year == input$Year) %>% select(Year, input$CrimeType) %>% arrange(desc(Total_crime))
                  )
    }else if(input$CrimeType == "Violent_crime"){
      DT::datatable(Crime %>% filter(Year == input$Year) %>% select(Year, input$CrimeType) %>% arrange(desc(Violent_crime))
      )
    }else{
      DT::datatable(Crime %>% filter(Year == input$Year) %>% select(Year, input$CrimeType) %>% arrange(desc(Property_crime))
      )
    }
  },
  options = list(pageLength = 10)
  )
  
  output$regsummary <- renderPrint({
    if(input$logX){
      reg <- lm(Crime[[input$Y]] ~ log(Crime[[input$X]]), data = Crime)
    }else if(input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ Crime[[input$X]], data = Crime)
    }else if(input$logX && input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ log(Crime[[input$X]]), data = Crime)
    }else{reg <- lm(Crime[[input$Y]] ~ Crime[[input$X]], data = Crime)}
    summary(reg)
  })
  
  
  
  output$regscatter <- renderPlot({
    
    if(input$logX){
      reg <- lm(Crime[[input$Y]] ~ log(Crime[[input$X]]), data = Crime)
    }else if(input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ Crime[[input$X]], data = Crime)
    }else if(input$logX && input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ log(Crime[[input$X]]), data = Crime)
    }else{reg <- lm(Crime[[input$Y]] ~ Crime[[input$X]], data = Crime)}
    
    pl <- ggplot(Crime, aes(x = !!input$X, y = !!input$Y)) +
      geom_point() +
      geom_smooth(mapping = aes(x = !!input$X, y = !!input$Y), method = lm, se = F) +
      ggtitle("Scatter Plot")
    if(input$logX){
      pl <- pl+
        scale_x_log10()
    }
    if(input$logY){
      pl <- pl+
        scale_y_log10()
    }
    pl
  }) #End of regscatter
  
  output$regresidual <- renderPlot({
    
    if(input$logX){
      reg <- lm(Crime[[input$Y]] ~ log(Crime[[input$X]]), data = Crime)
    }else if(input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ Crime[[input$X]], data = Crime)
    }else if(input$logX && input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ log(Crime[[input$X]]), data = Crime)
    }else{reg <- lm(Crime[[input$Y]] ~ Crime[[input$X]], data = Crime)}
    
    data.frame(`1` = fitted(reg), `2` = residuals(reg)) %>%
      rename("X" = "X1", "Y" = "X2") %>%
      ggplot(aes(x = X, y = Y)) +
      geom_point() +
      ggtitle("Residual vs fitted value") +
      xlab("Fitted Value") +
      ylab("Residual")
    
  })
  
  output$regQQ <- renderPlot({
    
    if(input$logX){
      reg <- lm(Crime[[input$Y]] ~ log(Crime[[input$X]]), data = Crime)
    }else if(input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ Crime[[input$X]], data = Crime)
    }else if(input$logX && input$logY){
      reg <- lm(log(Crime[[input$Y]]) ~ log(Crime[[input$X]]), data = Crime)
    }else{reg <- lm(Crime[[input$Y]] ~ Crime[[input$X]], data = Crime)}
    
    qplot(sample = resid(reg), geom = "qq") + 
      geom_qq_line() +
      ggtitle("QQ Plot") +
      xlab("Theoretical") +
      ylab("Sample")
  })
  
  my_range3 <- reactive({
    cbind(input$range3[1],input$range3[2])
  })
  
  output$ttestsummary <- renderPrint({
    crime_state1 <-Crime %>% 
      filter(State == input$tteststate1,
             Year >= my_range3()[1],
             Year <= my_range3()[2]) %>%
      select(input$ttestvar)
    
    crime_state2 <-Crime %>% 
      filter(State == input$tteststate2,
             Year >= my_range3()[1],
             Year <= my_range3()[2]) %>%
      select(input$ttestvar)
    
    t.test(crime_state1, crime_state2)
    
  })
  
  output$table <- renderDataTable({
    
    DT::datatable(Crime2)
    
  },
  options = list(pageLength = 10)
  )
  
}

shinyApp(ui, server)