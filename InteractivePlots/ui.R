# if shiny package is not installed, install shiny
if(!require(shiny)) {
  install.packages("shiny")
}
library(shiny)

# create shiny interactive User Interface
shinyUI(fluidPage(
  # create the title of page
  titlePanel("Optimism Score vs Response Variables!"),
  
  # put our control widgets on the left hand side
  sidebarLayout(
    sidebarPanel(
      
      # create dropdown menu with response variables
      # selected = 1 is default for selecting the first variable
      selectInput("select", label = h3("Response Variables"), 
                  choices = list("Abortion" = 1, 
                                 "Embryonic Stem Cells" = 2, 
                                 "Stem Cells without human embryos" = 3,
                                 "Vitro Fertilization" = 4,
                                 "Death Penalty" = 5,
                                 "Belief in life after death" = 6,
                                 "Belief in heaven for form 1 and 3" = 7,
                                 "Belief in heaven for form 2 and 4" = 8,
                                 "Belief in hell for form 1 and 3" = 9,
                                 "Belief in hell for form 2 and 4" = 10,
                                 "Party" = 11,
                                 "Ideology" = 12, 
                                 "More gay and lesbian couples raising children" = 13,
                                 "More people of different races marrying each other" = 14, 
                                 "More people who are not religious" = 15,
                                 "More elderly people in the population" = 16,
                                 "Doctors always try to save patients' lives" = 17, 
                                 "Prescription of lethal doses of drugs" = 18), 
                  selected = 1),
      
      # condition statement- if dropdown menu is either 1,2,3,..., create radio button
      # default radio button is bar chart
      conditionalPanel(
        condition = "input.select == 1 | input.select == 2 | input.select == 3 | input.select == 4 | input.select == 6 |
        input.select == 7 | input.select == 8 | input.select == 9 | input.select == 10 |input.select == 13 | input.select == 14 | input.select == 15 | input.select == 16",
        radioButtons("radio", label = h3("Types of Plots"),
                     choices = list("Bar Chart" = 1, 
                                    "Heatmap" = 2),                                   
                     selected = 1)
      ),
      
      # condition statement - if dropdown menu is 1,2,3 or 4, create checkboxes with these names and values
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 1 | input.select == 2 | input.select == 3 | input.select == 4",
        checkboxGroupInput("checkGroup", label = h3("Types of responses"), 
                           choices = list("Morally acceptable" = "accept", 
                                          "Not a moral issue" = "no_issue", 
                                          "Morally wrong" = "wrong",
                                          "Depends on situation" = "depend"),
                           selected = list("Morally acceptable" = "accept", 
                                           "Not a moral issue" = "no_issue", 
                                           "Morally wrong" = "wrong",
                                           "Depends on situation" = "depend"))
      ),
      
      # condition statement - if dropdown menu is 5, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 5",
        checkboxGroupInput("checkGroup5", label = h3("Types of responses"), 
                           choices = list("Strongly favor" = "s_favor", 
                                          "Favor" = "favor", 
                                          "Oppose" = "oppose",
                                          "Strongly oppose" = "s_oppose"),
                           selected = list("Strongly favor" = "s_favor", 
                                           "Favor" = "favor", 
                                           "Oppose" = "oppose",
                                           "Strongly oppose" = "s_oppose"))
      ),
      
      # condition statement - if dropdown menu is 6, 7, 8, 9 or 10, create checkboxes with these names and values 
      # default is selecting all 
      conditionalPanel(
        condition = "input.select == 6 | input.select == 7 | input.select == 8 | input.select == 9 | input.select == 10",
        checkboxGroupInput("checkGroup6", label = h3("Types of responses"), 
                           choices = list("No, don't believe" = "no", 
                                          "Yes, believe in" = "yes"),
                           selected = list("No, don't believe" = "no", 
                                           "Yes, believe in" = "yes"))
      ),
      
      # condition statement - if dropdown menu is 11, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 11",
        checkboxGroupInput("checkGroup11", label = h3("Types of responses"), 
                           choices = list("Republican" = "rep", 
                                          "Democrat" = "demo", 
                                          "Independent" = "inde",
                                          "No preference" = "no_pref",
                                          "Other party" = "other"),
                           selected = list("Republican" = "rep", 
                                           "Democrat" = "demo", 
                                           "Independent" = "inde",
                                           "No preference" = "no_pref",
                                           "Other party" = "other"))
      ),
      
      
      # condition statement - if dropdown menu is 12, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 12",
        checkboxGroupInput("checkGroup12", label = h3("Types of responses"), 
                           choices = list("Very conservative" = "v_con", 
                                          "Conservative" = "con", 
                                          "Moderate" = "mod",
                                          "Liberal" = "lib",
                                          "Very liberal" = "v_lib"),
                           selected = list("Very conservative" = "v_con", 
                                           "Conservative" = "con", 
                                           "Moderate" = "mod",
                                           "Liberal" = "lib",
                                           "Very liberal" = "v_lib"))
      ),
      
      # condition statement - if dropdown menu is 13,14,15 or 16, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 13 | input.select == 14 | input.select == 15 | input.select == 16",
        checkboxGroupInput("checkGroup13", label = h3("Types of responses"), 
                           choices = list("Good for society" = "good", 
                                          "Not much difference" = "no_diff", 
                                          "Bad for society" = "bad"),
                           selected = list("Good for society" = "good", 
                                           "Not much difference" = "no_diff", 
                                           "Bad for society" = "bad"))
      ),        
      
      # condition statement - if dropdown menu is 17, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 17",
        checkboxGroupInput("checkGroup17", label = h3("Types of responses"), 
                           choices = list("Sometimes let a patient die" = "yes", 
                                          "Always save a life" = "no"),
                           selected = list("Sometimes let a patient die" = "yes", 
                                           "Always save a life" = "no"))
      ),
      
      # condition statement - if dropdown menu is 18, create checkboxes with these names and values 
      # default is selecting all
      conditionalPanel(
        condition = "input.select == 18",
        checkboxGroupInput("checkGroup18", label = h3("Types of responses"), 
                           choices = list("Approve" = "yes", 
                                          "Disapprove" = "no"),
                           selected = list("Approve" = "yes", 
                                           "Disapprove" = "no"))
      ),
      
      # adds a HTML tag horizontal line
      hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("abortion")
    )
)
))
