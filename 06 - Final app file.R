library(tidyverse)
library(MASS)
library(aod)
library(ggplot2)
library(MAS6005)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(colorspace)
library(scales)
library(plyr)
library(shinydashboard)

#read data 
mydata <- read.csv("regression_data.csv")
head(mydata)


#make factors
mydata$Speed_limit <- factor(mydata$Speed_limit)
mydata$Light_Conditions <- factor(mydata$Light_Conditions)
#mydata$Road_Type   <- factor(mydata$Road_Type)
mydata$Urban_or_Rural_Area <- factor(mydata$Urban_or_Rural_Area)
mydata$Day_of_Week <- factor(mydata$Day_of_Week)
mydata$X1st_Road_Class  <- factor(mydata$X1st_Road_Class)
mydata$Road_Surface_Conditions   <- factor(mydata$Road_Surface_Conditions )
mydata$Driver_Sex   <- factor(mydata$Driver_Sex)
mydata$new_Vehicle_Type   <- factor(mydata$new_Vehicle_Type)
mydata$Passenger_Involve    <- factor(mydata$Passenger_Involve)
mydata$Pedestrian_Involve  <- factor(mydata$Pedestrian_Involve)
mydata$Road_Surface_Conditions  <- factor(mydata$Road_Surface_Conditions)





mylogit <- glm(Severity~Speed_limit+Light_Conditions+Road_Type+Urban_or_Rural_Area+
                  Day_of_Week+Driver_Age+X1st_Road_Class+Road_Surface_Conditions+
                  Special_Conditions_at_Site+ Driver_Sex + new_Vehicle_Type +
                  Passenger_Involve + Pedestrian_Involve +
                  Light_Conditions:X1st_Road_Class,
                data = mydata,  family = binomial(link = probit))
summary(mylogit)



#experiment with histograms

SoilSciGuylabs <- c("test ", "hello")

mydata["newage"] <- (mydata["Driver_Age"]*14.0165) + 40.68


# ggplot(mydata,aes(x=(newage))) + 
#   geom_histogram(data=subset(mydata,Severity == 1),  fill = "red", alpha = 0.3, stat="density", bins=10 ) +
#   geom_histogram(data=subset(mydata,Severity == 0), fill = "blue", alpha = 0.3, stat="density", bins=10) +
#   ylab("Density") +
#   theme_minimal() +
#   #ggtitle("Distributions of selected variable, red = Severe, blue = Not Severe") +
#   ggtitle(expression(atop("Distributions of selected variable", atop(italic("Legend: Red = Severe, Blue = Not Severe"), "")))) +
#   theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
#         axis.title.x=element_blank(),
#         plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1)) 
#   #scale_x_discrete(labels= SoilSciGuylabs)





#cant find better one



#create dataframe for odds ratio graph

varnames <- c("Intercept",
              "30mph speed limit",
              "40mph speed limit",
              "50mph speed limit",
              "60mph speed limit",
              "70mph speed limit",
              "Nighttime with lights",
              "Nighttime without lights",
              "Accident at roundabout",
              "Accident in rural area",
              "Weekend",
              "Mean Driver Age",
              "A-road",
              "Motorway",
              "Wet Road",
              "Special Conditions",
              "Female involved",
              "Two-wheel vehicle involved",
              "Passenger involved",
              "Pedestrian involved",
              "Nighttime w/ lights and A-road",
              "Nighttime w/o lights and A-road",
              "Nighttime w/ lights and Motorway",
              "Nighttime w/o lights and Motorway"
              )
#create dataframe
dataframe <- as.data.frame(varnames)
row.names(dataframe) <- varnames



dataframe[,1] <- exp(coef(mylogit)) - 1 
#dataframe[,2] <- exp(confint(mylogit))[,1] -1 
#dataframe[,3] <- exp(confint(mylogit))[,2] -1
dataframe[,2] <- summary(mylogit)$coefficients[,4]
dataframe[,3] <- str_remove_all(rownames(dataframe), ":")

#get row names
rownames(data.matrix)


#delete intercept
dataframe <- dataframe[-1,]

#name columns
names(dataframe) <- c("OR", "P-value", "Variable")



# require(ggplot2)
# ggplot() + 
#   geom_bar(data = dataframe, aes(x= reorder(Variable, OR), y=OR, fill=OR > 0), stat = "identity") +
#   coord_flip() +
#   #scale_fill_brewer(type = "seq", palette = 1) +  
#   #geom_errorbar(aes(x=Variable, y=OR, ymin=Lower, ymax=Upper), 
#   #              width=.1, position=position_dodge(), data=dataframe) +
#   #theme_hc()+ scale_colour_hc() +
#   theme_minimal()+
#   ggtitle("Variable impact of likelihood of severe accident") +
#   xlab("Variable") + ylab("Likelihood of Severe Accident") +
#   theme(legend.position = "none",
#         axis.ticks.x=element_blank(),
#         axis.text.x=element_blank()
#         ) +
#   geom_hline(aes(yintercept=0)) +
#   geom_text(aes(0,0,label = "Just as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=0.5)) +
#   geom_text(aes(0,0.5,label = "1.5x as likely", vjust = -.5, hjust=-0.2)) +  
#   geom_hline(aes(yintercept=1.5)) +
#   geom_text(aes(0,1.5,label = "2.5x as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=1.0)) +
#   geom_text(aes(0,1.0,label = "2 x as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=-1.5)) +
#   geom_text(aes(0,-1.5,label = "2.5x less likely", vjust = -.5, hjust=-0.2))  +
#   geom_hline(aes(yintercept=-0.5)) +
#   geom_text(aes(0,-0.5,label = "1.5x less likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=-1.0)) +
#   geom_text(aes(0,-1.0,label = "2 x less likely", vjust = -.5, hjust=-0.2)) 
# 
# 





#test new data stuff ---- 


#
##create new data and use predict command

newdata = data.frame(Speed_limit =40,
                     Day_of_Week =0,
                     X1st_Road_Class = 2,
                     Road_Type =1,
                     Light_Conditions=0,
                     Road_Surface_Conditions=0,
                     Special_Conditions_at_Site=0,
                     Urban_or_Rural_Area=0,
                     Driver_Sex=0,
                     new_Vehicle_Type=1,
                     Passenger_Involve=1,
                     Pedestrian_Involve=0,
                     Driver_Age = 1.2)
newdatacol <- matrix(newdata)


##make factors so compatible with regressio nso can use predict command

newdata$Speed_limit <- factor(newdata$Speed_limit)
newdata$Light_Conditions <- factor(newdata$Light_Conditions)
#newdata$Road_Type   <- factor(newdata$Road_Type)
newdata$Urban_or_Rural_Area <- factor(newdata$Urban_or_Rural_Area)
newdata$Day_of_Week <- factor(newdata$Day_of_Week)
newdata$X1st_Road_Class  <- factor(newdata$X1st_Road_Class)
newdata$Road_Surface_Conditions   <- factor(newdata$Road_Surface_Conditions )
newdata$Driver_Sex   <- factor(newdata$Driver_Sex)
newdata$new_Vehicle_Type   <- factor(newdata$new_Vehicle_Type)
newdata$Passenger_Involve    <- factor(newdata$Passenger_Involve)
newdata$Pedestrian_Involve  <- factor(newdata$Pedestrian_Involve)
newdata$Road_Surface_Conditions  <- factor(newdata$Road_Surface_Conditions)



predict(mylogit, newdata, type="response") ##new odds ratio given data






#so >0.5 would be severe accident, <0.5 is not severe, so it's relatie to 0.5

#probability of severe accident,

#can add an arrow depending on the last value



#take the average values of the data

#average newdata difference oddschange

#get average

# Create the function. - CREATE MODE FUNCTION BUT DID NOT USE IN END
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(mydata$Day_of_Week)

#NEED TO CONSIDER HERE HOW GONNA CALCULATE  CHANGE IN RISK  MAKE SURE GET ORDER OF VARS LINED UP

#speed lim
if (newdatacol[1] == 20) {
  speedlim_of <- 0
} else if (newdatacol[1] == 30)  {
  speedlim_of <- 0.16103507

} else if (newdatacol[1] == 40)  {
  speedlim_of <- 0.33545664

} else if (newdatacol[1] == 50)  {
  speedlim_of <- 0.33216332

} else if (newdatacol[1] == 60)  {
  speedlim_of <- 0.59954852

} else if (newdatacol[1] == 70)  {
  speedlim_of <- 0.34471198
}


#DAYweek
if (newdatacol[2] == 0) {
  dw_of <- 0
} else if (newdatacol[2] == 1)  {
  dw_of <- 0.08118903
}


#first road class
if (newdatacol[3] == 0) {
  rcf_of <- 0
} else if (newdatacol[3] == 1)  {
  rcf_of <- -0.21771462

} else if (newdatacol[3] == 2)  {
  rcf_of <- -0.01621076

}



#road type
if (newdatacol[4] == 0) {
  rt_of <- 0
} else if (newdatacol[4] == 1)  {
  rt_of <- -0.19928643

} 





#light con
if (newdatacol[5] == 0) {
  lc_of <- 0
} else if (newdatacol[5] == 1)  {
  lc_of <- 0.14696655
  
} else if (newdatacol[5] == 2)  {
  lc_of <- 0.07245186

} 


#road surf
if (newdatacol[6] == 0) {
  rs_of <- 0
} else if (newdatacol[6] == 1)  {
  rs_of <- -0.0212826

} 

#spec cond
if (newdatacol[7] == 0) {
  sc_of <- 0
} else if (newdatacol[7] == 1)  {
  sc_of <- -0.11689608


}

#urb
if (newdatacol[8] == 0) {
  urb_of <- 0
} else if (newdatacol[8] == 1)  {
  urb_of <- 0.1808056

  
}

#driver sex
if (newdatacol[9] == 0) {
  sex_of <- 0
} else if (newdatacol[9] == 1)  {
  sex_of <- -0.16394341

  
}


# new vehicle type 
if (newdatacol[10] == 0) {
  vt_of <- 0
} else if (newdatacol[10] == 1)  {
  vt_of <- -0.45561551

  
}

# pass involve
if (newdatacol[11] == 0) {
  pass_of <- 0
} else if (newdatacol[11] == 1)  {
  pass_of <- 0.02259859

  
}


# ped involve 
if (newdatacol[12] == 0) {
  ped_of <- 0
} else if (newdatacol[12] == 1)  {
  ped_of <- 0.64597463

  
}

# driver age 

da_of <- (as.numeric(newdatacol[13]) - mean(mydata$Driver_Age)) * 0.06506718





#calculate tne chance of severity for mean and mode and then base 
#score on that 

#so initial report is with most common values, chance of sev is....
#then change values get new sev score















#means - remember this is all compared to means 
#inputted in modes manually
means <- c(30, 
           0,
           2,
           0,
           0,
           0, # getmode(mydata$Road_Surface_Conditions),
           0, #getmode(mydata$Special_Conditions_at_Site),
           0, #getmode(mydata$Urban_or_Rural_Area),
           0, #getmode(mydata$Driver_Sex),
           1, #getmode(mydata$new_Vehicle_Type),
           0, #getmode(mydata$Passenger_Involve),
           0, #getmode(mydata$Pedestrian_Involve),
           0.0 #mean(mydata$Driver_Age)
           ) #compare to bassline for the cat variable


newdata_or <- c( speedlim_of, dw_of, rcf_of, rt_of, lc_of, rs_of, sc_of, urb_of, sex_of, vt_of, pass_of, ped_of, da_of) 
  


#create variables that can be put in as odds ratio, then have change in odds ratio frame



#IF NUMBER, NEEDS TO TAKE ON ODDS RATIO OF THAT NUMBER IF CAT 


#difference dataframe for changing odds
riskdata <- as.data.frame(means)





#newdata 
riskdata$newdata1 <- newdatacol


#add of column

riskdata$newof <- newdata_or
#I AM HERE SAT 13
#check if this will match the previous code, this table and how need to move it to what it looks like





riskdata$vars <- c("Speed Limit",
                   "Weekend",
                   "Road Type",
                   "Roundabout",
                   "Light Conditions",
                   "Wet Road",
                   "Special Conditions",
                   "Rural",
                   "Gender",
                   "Vehicle Type",
                   "Passenger Involved",
                   "Pedestrian Involved",
                   "Driver Age"
                   )




# require(ggplot2)
# ggplot() + 
#   geom_bar(data = riskdata, aes(x= reorder(vars, newof), y=newof, fill=newof > 0), stat = "identity") +
#   coord_flip() +
#   #scale_fill_brewer(type = "seq", palette = 1) +  
#   #geom_errorbar(aes(x=Variable, y=OR, ymin=Lower, ymax=Upper), 
#   #              width=.1, position=position_dodge(), data=dataframe) +
#   #theme_hc()+ scale_colour_hc() +
#   theme_minimal()+
#   ggtitle("Variable impact of likelihood of severe accident") +
#   xlab("Variable") + ylab("Likelihood of Severe Accident") +
#   theme(legend.position = "none",
#         axis.ticks.x=element_blank(),
#         axis.text.x=element_blank()
#   ) +
#   geom_hline(aes(yintercept=0)) +
#   geom_text(aes(0,0,label = "Just as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=0.5), linetype='dotted') +
#   geom_text(aes(0,0.5,label = "1.5x as likely", vjust = -.5, hjust=-0.2)) +  
#   geom_hline(aes(yintercept=1.5), linetype='dotted') +
#   geom_text(aes(0,1.5,label = "2.5x as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=1.0), linetype='dotted') +
#   geom_text(aes(0,1.0,label = "2x as likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=-1.5), linetype='dotted') +
#   geom_text(aes(0,-1.5,label = "2.5x less likely", vjust = -.5, hjust=-0.2))  +
#   geom_hline(aes(yintercept=-0.5), linetype='dotted') +
#   geom_text(aes(0,-0.5,label = "1.5x less likely", vjust = -.5, hjust=-0.2)) +
#   geom_hline(aes(yintercept=-1.0), linetype='dotted') +
#   geom_text(aes(0,-1.0,label = "2x less likely", vjust = -.5, hjust=-0.2)) 



riskvallist <- c()



# dashboard stuf###
###################################
######################
#############################
#########################################





## app.R ##
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Car Accident Severity Model",
                  titleWidth = 450),
  dashboardSidebar(
    width=300,
    disable = FALSE,
    sidebarMenu(
      
      menuItem( # Add a menu item
        text = "Click to input custom values into the model",

        tabName = "input",
        
        # Insert your inputs within the menuItem
        # Range de Selecao de Data
        selectInput("sl", label = h5("Speed Limit"), 
                    choices = list("20mph" = 20, "30mph" = 30, "40mph" = 40, "50mph" = 50, "60mph" = 60, "70mph" = 70 ), 
                    selected = 0),
        sliderInput(inputId ="ageid", label = h5("Driver Age"),
                    min = 17, max = 100, value = 40, sep = "", step=1),
        selectInput("dw", label = h5("Day type"), 
                    choices = list("Weekday" = 0, "Weekend" = 1 ), 
                    selected = 0),
        selectInput("rc", label = h5("Road type"), 
                    choices = list("A-road" = 1, "Motorway" = 2, "Other" = 0 ), 
                    selected = 0), 
        selectInput("ra", label = h5("Accident at roundabout"), 
                    choices = list("Accident not at roundable" = 0, "Accident at roundabout" = 1 ), 
                    selected = 0), 
        selectInput("lc", label = h5("Light Conditions"), 
                    choices = list("Daylight" = 0, "Darkness with lights" = 1 , "Darkness without lights" = 2), 
                    selected = 0), 
        selectInput("rs", label = h5("Wet/Dry Road"), 
                    choices = list("Dry" = 0, "Wet" = 1 ), 
                    selected = 0), 
        selectInput("sc", label = h5("Special Conditions (such as roadworks)"), 
                    choices = list("None" = 0, "Special Conditions" = 1), 
                    selected = 0), 
        selectInput("urb", label = h5("Urban or Rural"), 
                    choices = list("Urban" = 0, "Rural" = 1), 
                    selected = 0), 
        selectInput("vt", label = h5("Vehicle Type"), 
                    choices = list("Four Wheelers" = 0, "Two Wheelers" = 1), 
                    selected = 0), 
        selectInput("gen", label = h5("Gender"), 
                    choices = list("No Females involved" = 0, "Female involved" = 1), 
                    selected = 0), 
        selectInput("pass", label = h5("Passenger involved"), 
                    choices = list("No passenger" = 0, "Passenger involved" = 1), 
                    selected = 0), 
        selectInput("ped", label = h5("Pedestrian involved"), 
                    choices = list("No pedestrian involved" = 0, "Pedestrian involved" = 1), 
                    selected = 0)
        
        
        
        
        
        
        
        
        
      ) # /menuItem 
    ) # /sidebarMenu
  ), # dashboardSidebar
          
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      #height = 3,
      
      # box(
      #   width = 1, 
      #   "A box with a solid black background"
      # ),
      
      #column( width=7,
      

      
      valueBoxOutput(
        width = 3,
        "progressBox"),
      
      #),
      
      box(
        title = "Interactive variable impact on likelihood of severity compared to mean values",
        width = 9,
        status="warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(outputId = "dynPlot")
      )
    ),
    
    #row 2
    fluidRow(
      column(
        width= 6,
        box(
          #width = 2,
          title = "Choose Variable Distribution Control",
          "Distribution of the variables, comparing severe and non-severe accidents",
          status="primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectInput("var1", label = h3("Select box"), 
                      choices = list(
                        "Speed limit" = "Speed_limit",
                        "Weekend" = "Day_of_Week",
                        "Road Type" = "X1st_Road_Class",
                        "Roundabout" = "Road_Type",
                        "Light conditions" = "Light_Conditions",
                        "Wet/Dry Road" = "Road_Surface_Conditions",
                        "Special Conditions" = "Special_Conditions_at_Site",
                        "Urban/Rural" = "Urban_or_Rural_Area",
                        "Gender" = "Driver_Sex",
                        "Vehicle Type" = "new_Vehicle_Type",
                        "Passenger involved" = "Passenger_Involve",
                        "Pedestrian involved" = "Pedestrian_Involve",
                        "Driver Age" = "Driver_Age"
                        ), 
                      selected = 1)
        ),
        
        box(
          title = "Variable Distribution - Comparing Severe and Non-Sever Accidents",
          #height = 3,
          status="warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "distPlot")
        )
        
        
      ),
      box(
        title = "What impacts likelihood of severe accident?",
        #height = 3,
        status="warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(outputId = "sumPlot")
      )
    )
    
    
  )
)

server <- function(input, output, session) {
  
  
  
  #box with predicted value for new data
  
  output$progressBox <- renderValueBox({
    
    
    
    ##create new data and use predict command
    
    newdata = data.frame(Speed_limit =input$sl,
                         Day_of_Week =input$dw,
                         X1st_Road_Class = input$rc,
                         Road_Type =input$ra,
                         Light_Conditions=input$lc,
                         Road_Surface_Conditions=input$rs,
                         Special_Conditions_at_Site=input$sc,
                         Urban_or_Rural_Area=input$urb,
                         Driver_Sex=input$gen,
                         new_Vehicle_Type=input$vt,
                         Passenger_Involve=input$pass,
                         Pedestrian_Involve=input$ped,
                         Driver_Age = (input$ageid -40.68357)/14.0165)
    newdatacol <- matrix(newdata)
    
    
    ##make factors so compatible with regressio nso can use predict command
    
    newdata$Speed_limit <- factor(newdata$Speed_limit)
    newdata$Light_Conditions <- factor(newdata$Light_Conditions)
    newdata$Urban_or_Rural_Area <- factor(newdata$Urban_or_Rural_Area)
    newdata$Day_of_Week <- factor(newdata$Day_of_Week)
    newdata$X1st_Road_Class  <- factor(newdata$X1st_Road_Class)
    newdata$Road_Surface_Conditions   <- factor(newdata$Road_Surface_Conditions )
    newdata$Driver_Sex   <- factor(newdata$Driver_Sex)
    newdata$new_Vehicle_Type   <- factor(newdata$new_Vehicle_Type)
    newdata$Passenger_Involve    <- factor(newdata$Passenger_Involve)
    newdata$Pedestrian_Involve  <- factor(newdata$Pedestrian_Involve)
    newdata$Road_Surface_Conditions  <- factor(newdata$Road_Surface_Conditions)
    
    newdata$Road_Type   <- as.numeric(newdata$Road_Type)
    newdata$Special_Conditions_at_Site   <- as.numeric(newdata$Special_Conditions_at_Site)
    
    
    
    riskvalue <- predict(mylogit, newdata, type="response") ##new odds ratio given data
    
    
    
    #if want to do a change in percent, can do a list and take away from previous
    #didnt work, prob have to delete this number
    #riskvalueprev <- riskvalue
    riskvallist <- c(riskvallist, riskvalue)
    
    valueBox(
      
      paste0(format(round(riskvalue*100, 0), nsmall = 0),"%"
             #,"(", format(round((riskvalue - 0.21875543)*100, 0), nsmall = 0) ,")" 
      ), 
      #"Chance of severe accident given the value of variables selected",
      paste0("Chance of severe accident given the value of variables selected.  ", format(round((riskvalue - 0.21875543)*100, 0), nsmall = 0) ,"% change of chance of severe accident compared to the average." 
      ), 
      "Test",
      icon = icon("car"),
      color = "maroon"
    )
  })
  
  
  
  
  
  
  
  
  
  #changable odd ratio plot
  
  
  
  output$dynPlot <- renderPlot({
    
    
    
    
    #data stuff####################################################################
    
    
    #data stuff 
    ##create new data and use predict command
    ##create new data and use predict command
    
    newdata = data.frame(Speed_limit =input$sl,
                         Day_of_Week =input$dw,
                         X1st_Road_Class = input$rc,
                         Road_Type =input$ra,
                         Light_Conditions=input$lc,
                         Road_Surface_Conditions=input$rs,
                         Special_Conditions_at_Site=input$sc,
                         Urban_or_Rural_Area=input$urb,
                         Driver_Sex=input$gen,
                         new_Vehicle_Type=input$vt,
                         Passenger_Involve=input$pass,
                         Pedestrian_Involve=input$ped,
                         Driver_Age = (input$ageid -40.68357)/14.0165 )
    newdatacol <- matrix(newdata)
    
    
    ##make factors so compatible with regressio nso can use predict command
    
    newdata$Speed_limit <- factor(newdata$Speed_limit)
    newdata$Light_Conditions <- factor(newdata$Light_Conditions)
    newdata$Urban_or_Rural_Area <- factor(newdata$Urban_or_Rural_Area)
    newdata$Day_of_Week <- factor(newdata$Day_of_Week)
    newdata$X1st_Road_Class  <- factor(newdata$X1st_Road_Class)
    newdata$Road_Surface_Conditions   <- factor(newdata$Road_Surface_Conditions )
    newdata$Driver_Sex   <- factor(newdata$Driver_Sex)
    newdata$new_Vehicle_Type   <- factor(newdata$new_Vehicle_Type)
    newdata$Passenger_Involve    <- factor(newdata$Passenger_Involve)
    newdata$Pedestrian_Involve  <- factor(newdata$Pedestrian_Involve)
    
    newdata$Road_Surface_Conditions  <- as.numeric(newdata$Road_Surface_Conditions)
    newdata$Road_Type   <- as.numeric(newdata$Road_Type)
    
    
    
    
    #predict(mylogit, newdata, type="response") ##new odds ratio given data
    
    #so >0.5 would be severe accident, <0.5 is not severe, so it's relatie to 0.5
    
    #probability of severe accident,
    
    #can add an arrow depending on the last value
    
    
    
    #take the average values of the data
    
    #average newdata difference oddschange
    
    #get average
    
    
    # Create the function. - CREATE MODE FUNCTION BUT DID NOT USE IN END
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    getmode(mydata$Day_of_Week)
    
    #NEED TO CONSIDER HERE HOW GONNA CALCULATE  CHANGE IN RISK  MAKE SURE GET ORDER OF VARS LINED UP
    
    #speed lim
    if (newdatacol[1] == 20) {
      speedlim_of <- 0
    } else if (newdatacol[1] == 30)  {
      speedlim_of <- 0.16103507
      
    } else if (newdatacol[1] == 40)  {
      speedlim_of <- 0.33545664
      
    } else if (newdatacol[1] == 50)  {
      speedlim_of <- 0.33216332
      
    } else if (newdatacol[1] == 60)  {
      speedlim_of <- 0.59954852
      
    } else if (newdatacol[1] == 70)  {
      speedlim_of <- 0.34471198
    }
    
    
    #DAYweek
    if (newdatacol[2] == 0) {
      dw_of <- 0
    } else if (newdatacol[2] == 1)  {
      dw_of <- 0.08118903
    }
    
    
    #first road class
    if (newdatacol[3] == 0) {
      rcf_of <- 0
    } else if (newdatacol[3] == 1)  {
      rcf_of <- -0.21771462
      
    } else if (newdatacol[3] == 2)  {
      rcf_of <- -0.01621076
      
    }
    
    
    
    #road type
    if (newdatacol[4] == 0) {
      rt_of <- 0
    } else if (newdatacol[4] == 1)  {
      rt_of <- -0.19928643
      
    } 
    
    
    
    
    
    #light con
    if (newdatacol[5] == 0) {
      lc_of <- 0
    } else if (newdatacol[5] == 1)  {
      lc_of <- 0.14696655
      
    } else if (newdatacol[5] == 2)  {
      lc_of <- 0.07245186
      
    } 
    
    
    #road surf
    if (newdatacol[6] == 0) {
      rs_of <- 0
    } else if (newdatacol[6] == 1)  {
      rs_of <- -0.0212826
      
    } 
    
    #spec cond
    if (newdatacol[7] == 0) {
      sc_of <- 0
    } else if (newdatacol[7] == 1)  {
      sc_of <- -0.11689608
      
      
    }
    
    #urb
    if (newdatacol[8] == 0) {
      urb_of <- 0
    } else if (newdatacol[8] == 1)  {
      urb_of <- 0.1808056
      
      
    }
    
    #driver sex
    if (newdatacol[9] == 0) {
      sex_of <- 0
    } else if (newdatacol[9] == 1)  {
      sex_of <- -0.16394341
      
      
    }
    
    
    # new vehicle type 
    if (newdatacol[10] == 0) {
      vt_of <- 0
    } else if (newdatacol[10] == 1)  {
      vt_of <- -0.45561551
      
      
    }
    
    # pass involve
    if (newdatacol[11] == 0) {
      pass_of <- 0
    } else if (newdatacol[11] == 1)  {
      pass_of <- 0.02259859
      
      
    }
    
    
    # ped involve 
    if (newdatacol[12] == 0) {
      ped_of <- 0
    } else if (newdatacol[12] == 1)  {
      ped_of <- 0.64597463
      
      
    }
    
    # driver age 
    
    da_of <- (as.numeric(newdatacol[13]) - mean(mydata$Driver_Age)) * 0.06506718
    
    
    
    
    
    #means - remember this is all compared to means 
    #inputted in modes manually
    means <- c(30, 
               0,
               2,
               0,
               0,
               0, # getmode(mydata$Road_Surface_Conditions),
               0, #getmode(mydata$Special_Conditions_at_Site),
               0, #getmode(mydata$Urban_or_Rural_Area),
               0, #getmode(mydata$Driver_Sex),
               1, #getmode(mydata$new_Vehicle_Type),
               0, #getmode(mydata$Passenger_Involve),
               0, #getmode(mydata$Pedestrian_Involve),
               0.0 #mean(mydata$Driver_Age)
    ) #compare to bassline for the cat variable
    
    
    newdata_or <- c( speedlim_of, dw_of, rcf_of, rt_of, lc_of, rs_of, sc_of, urb_of, sex_of, vt_of, pass_of, ped_of, da_of) 
    
    
    
    #create variables that can be put in as odds ratio, then have change in odds ratio frame
    
    
    
    #IF NUMBER, NEEDS TO TAKE ON ODDS RATIO OF THAT NUMBER IF CAT 
    
    
    #difference dataframe for changing odds
    riskdata <- as.data.frame(means)
    
    
    
    
    
    #newdata 
    riskdata$newdata1 <- newdatacol
    
    
    #add of column
    
    riskdata$newof <- newdata_or
    #I AM HERE SAT 13
    #check if this will match the previous code, this table and how need to move it to what it looks like
    
    
    
    
    
    riskdata$vars <- c("Speed Limit",
                       "Weekend",
                       "Road Type",
                       "Roundabout",
                       "Light Conditions",
                       "Wet Road",
                       "Special Conditions",
                       "Rural",
                       "Gender",
                       "Vehicle Type",
                       "Passenger Involved",
                       "Pedestrian Involved",
                       "Driver Age"
    )
    
    ############## END DATA STUFF ################################################
    
    
    
    
    
    require(ggplot2)
    ggplot() + 
      geom_bar(data = riskdata, aes(x= reorder(vars, newof), y=newof, fill=newof > 0), stat = "identity") +
      coord_flip() +
      #scale_fill_brewer(type = "seq", palette = 1) +  
      #geom_errorbar(aes(x=Variable, y=OR, ymin=Lower, ymax=Upper), 
      #              width=.1, position=position_dodge(), data=dataframe) +
      #theme_hc()+ scale_colour_hc() +
      theme_minimal()+
      ggtitle("Variable impact of likelihood of severe accident") +
      xlab("Variable") + ylab("Likelihood of Severe Accident") +
      theme(legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank()
      ) +
      geom_hline(aes(yintercept=0)) +
      geom_text(aes(0,0,label = "Just as likely", vjust = -.5, hjust=-0.2)) +
      geom_hline(aes(yintercept=0.5), linetype='dotted') +
      geom_text(aes(0,0.5,label = "1.5x as likely", vjust = -.5, hjust=-0.2)) +  
      geom_hline(aes(yintercept=1.5), linetype='dotted') +
      geom_text(aes(0,1.5,label = "2.5x as likely", vjust = -.5, hjust=-0.2)) +
      geom_hline(aes(yintercept=-1.5), linetype='dotted') +
      geom_text(aes(0,-1.5,label = "2.5x less likely", vjust = -.5, hjust=-0.2))  +
      geom_hline(aes(yintercept=-0.5), linetype='dotted') +
      geom_text(aes(0,-0.5,label = "1.5x less likely", vjust = -.5, hjust=-0.2)) 
  }) 
  
  
  #####DISTRIBUTION PLOTS
  
  
  output$distPlot <- renderPlot({
    
    if (input$var1 != "Driver_Age") {
      
      if (input$var1 == "Day_of_Week") {
        labelvec = c("Weekday","Weekend")
      } else if (input$var1 == "Speed_limit") {
        labelvec = c("20mph", "30mph", "40mph", "50mph", "60mph", "70mph")
      } else if (input$var1 == "X1st_Road_Class") {
        labelvec = c("Others", "A-road", "Motorway")
      } else if (input$var1 == "Road_Type") {
        labelvec = c("Not roundabout", "At roundabout")
      } else if (input$var1 == "Light_Conditions") {
        labelvec = c("Daylight", "Darkness with lights", "Darkness not lights")
      } else if (input$var1 == "Road_Surface_Conditions") {
        labelvec = c("Dry", "Wet")
      } else if (input$var1 == "Special_Conditions_at_Site") {
        labelvec = c("No special conditions", "Special conditions")
      } else if (input$var1 == "Urban_or_Rural_Area") {
        labelvec = c("Urban", "Rural")
      } else if (input$var1 == "Driver_Sex") {
        labelvec = c("All males", "Female involved")
      } else if (input$var1 == "new_Vehicle_Type") {
        labelvec = c()
      } else if (input$var1 == "Passenger_Involve") {
        labelvec = c("No passenger casuality", "Passenger casuality")
      } else if (input$var1 == "Pedestrian_Involve") {
        labelvec = c("No pedestrian casuality", "Pedestrian casuality")
      } 
      
      
      
      ggplot(mydata,aes_string(x=input$var1)) + 
        geom_histogram(data=subset(mydata,Severity == 1), aes(y = (..count..)/sum(..count..)), fill = "red", alpha = 0.3, stat="count", bins=10 ) +
        geom_histogram(data=subset(mydata,Severity == 0),aes(y = (..count..)/sum(..count..)), fill = "blue", alpha = 0.3, stat="count", bins=10) +
        ylab("Density") +
        theme_minimal() +
        #ggtitle("Distributions of selected variable, red = Severe, blue = Not Severe") +
        ggtitle(expression(atop("Distributions",
                                atop(italic(""),
                                atop(italic("Red = Severe, Blue = Not Severe")),
                                     
                                     "")))) +
        theme(axis.text.x = element_text(size = 12, angle=-45, hjust=0, vjust=1), 
              axis.title.x=element_blank(),
              plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1)) +
        scale_x_discrete(labels= labelvec)
      
    } else {
      
      
      ggplot(mydata,aes(x=newage)) + 
        geom_histogram(data=subset(mydata,Severity == 1),  fill = "red", alpha = 0.3, stat="density", bins=10 ) +
        geom_histogram(data=subset(mydata,Severity == 0), fill = "blue", alpha = 0.3, stat="density", bins=10) +
        ylab("Density") +
        theme_minimal() +
        #ggtitle("Distributions of selected variable, red = Severe, blue = Not Severe") +
        ggtitle(expression(atop("Distributions",
                                atop(italic(""),
                                     atop(italic("Red = Severe, Blue = Not Severe")),
                                     
                                     "")))) +
        theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
              axis.title.x=element_blank(),
              plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = -1)) 
      
      
      
    }
    
    
    
    
    
  })
  
  
  output$sumPlot <- renderPlot({
    
    
    
    require(ggplot2)
    ggplot() + 
      geom_bar(data = dataframe, aes(x= reorder(Variable, OR), y=OR, fill=OR > 0), stat = "identity") +
      coord_flip() +
      #scale_fill_brewer(type = "seq", palette = 1) +  
      #geom_errorbar(aes(x=Variable, y=OR, ymin=Lower, ymax=Upper), 
      #              width=.1, position=position_dodge(), data=dataframe) +
      #theme_hc()+ scale_colour_hc() +
      theme_minimal()+
      ggtitle("Variable impact of likelihood of severe accident") +
      xlab("Variable") + ylab("Likelihood of Severe Accident") +
      theme(legend.position = "none",
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank()
      ) +
      geom_hline(aes(yintercept=0)) +
      geom_text(aes(0,0,label = "Just as likely", vjust = -0.12, hjust=-0.2)) +
      geom_hline(aes(yintercept=0.5) , linetype='dotted') +
      geom_text(aes(0,0.5,label = "1.5x as likely", vjust = -0.12, hjust=-0.2)) +  
      geom_hline(aes(yintercept=-0.5), linetype='dotted') +
      geom_text(aes(0,-0.5,label = "1.5x less likely", vjust = -0.12, hjust=-0.2)) 

    
    
  })
  
  
  
  
  
  
}
shinyApp(ui, server)












