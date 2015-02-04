if(!require(wq)) {
  install.packages("wq")
}

if(!require(animation)) {
  install.packages("animation")
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
}

if(!require(reshape2)) {
  install.packages("reshpae2")
}

if(!require(shiny)) {
  install.packages("shiny")
}

library(animation)
library(ggplot2)
library(reshape2)
library(wq)
library(shiny)

source("func.R")

# create shiny Server to host User Interface(UI)
# input - values parsed from UI 
# output - objects and graphs to return to UI 
shinyServer(function(input, output) {
  
  # output graph named abortion 
  output$abortion <- renderPlot(
    
    # if first item in dropdown and bar chart is selected, 
    # display bar chart
    if(input$select == 1 & input$radio == 1){
          # abortion q58a 
          accept <- views_prop("Morally acceptable", "opt.q58a")
          wrong <- views_prop("Morally wrong", "opt.q58a")
          no_issue <- views_prop("Not a moral issue", "opt.q58a")
          depend <- views_prop("Depends on situation (VOL.)", "opt.q58a")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
          view_DF <- melt(view_DF)
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score", ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects the views on abortion")
           + theme(plot.title=element_text(size=17,face="bold"))
           + scale_fill_discrete("Views on Abortion", labels=c("Morally acceptable", 
                                                               "Morally wrong", "Not an issue", "Depends on situation"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
    } else if ((input$select == 1 | input$select == 2 | input$select == 3 | input$select == 4) & (input$radio == 2)){
      # draw heat map of response variable 1, 2 ,3 or 4 
      good_7a <- views_prop("Good thing for society", "opt.q7a")
      no_diff_7a <- views_prop("Doesn't make much difference", "opt.q7a")
      bad_7a <- views_prop("Bad thing for society", "opt.q7a")
      
      good_7b <- views_prop("Good thing for society", "opt.q7b")
      no_diff_7b <- views_prop("Doesn't make much difference", "opt.q7b")
      bad_7b <- views_prop("Bad thing for society", "opt.q7b")
      
      good_7c <- views_prop("Good thing for society", "opt.q7c")
      no_diff_7c <- views_prop("Doesn't make much difference", "opt.q7c")
      bad_7c <- views_prop("Bad thing for society", "opt.q7c")
      
      good_7e <- views_prop("Good thing for society", "opt.q7e")
      no_diff_7e <- views_prop("Doesn't make much difference", "opt.q7e")
      bad_7e <- views_prop("Bad thing for society", "opt.q7e")
      
      # getting those into matrix
      answers <- c("answers", "Good", "No diff", "Bad")
      
      xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5", "1.5 -> 3", ">= 3")
      
      view_DF_a <- data.frame(xLabels, good_7a, no_diff_7a, bad_7a)
      colnames(view_DF_a) <- answers
      view_DF_a <- melt(view_DF_a)
      
      view_DF_b <- data.frame(xLabels, good_7b, no_diff_7b, bad_7b)
      colnames(view_DF_b) <- answers
      view_DF_b <- melt(view_DF_b)
      
      view_DF_c <- data.frame(xLabels, good_7c, no_diff_7c, bad_7c)
      colnames(view_DF_c) <- answers
      view_DF_c <- melt(view_DF_c)
      
      view_DF_e <- data.frame(xLabels, good_7e, no_diff_7e, bad_7e)
      colnames(view_DF_e) <- answers
      view_DF_e <- melt(view_DF_e)
      
      heat7a <- 
        # plotting a heat map with blue (default) color
        (ggplot(view_DF_a, aes(answers,  variable))
         + geom_tile(aes(fill = value), colour = "white")
         
         # background grid to only x and y axis
         + theme_classic()
         
         # reordering the x axis labels
         + scale_x_discrete(limits=xLabels)
         
         # changing the color of the map with middle color at 0.4
         + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4)
         
         # labels
         + labs(title="People's Views on Society", x=NULL, y="LGs couples\nwith kids", fill="Proportion")
         
         # place color guide on the top, horizontally, 0.5 towards margin, 2 cm long,
         # change the size of title of the plot and make it bold,
         # and remove the x-axis labels and ticks (for the first three plots)
         + theme(legend.position="top", legend.direction="horizontal",
                 plot.title=element_text(size=17,face="bold"),
                 axis.ticks=element_blank(), axis.text.x = element_blank())
         
         # the size of the text inside cells
         + geom_text(aes(label=format(round(value,2))), size = 4))
      
      heat7b <- (ggplot(view_DF_b, aes(answers,  variable))
                 + geom_tile(aes(fill = value), colour = "white")
                 + theme_classic()
                 + scale_x_discrete(limits=xLabels)
                 + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                 + labs(x=NULL, y="Interracial\nMarriage", fill="Proportion")
                 + geom_text(aes(label=format(round(value,2))), size = 4)
                 + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
      
      heat7c <- (ggplot(view_DF_c, aes(answers,  variable))
                 + geom_tile(aes(fill = value), colour = "white")
                 + theme_classic()
                 + scale_x_discrete(limits=xLabels)
                 + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                 + labs(x=NULL, y="More\nNon-religious", fill="Proportion")
                 + geom_text(aes(label=format(round(value,2))), size = 4)
                 + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
      
      heat7e <- (ggplot(view_DF_e, aes(answers,  variable))
                 + geom_tile(aes(fill = value), colour = "white")
                 + theme_classic()
                 + scale_x_discrete(limits=xLabels)
                 + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                 + labs(x = "Optimism Score", y="More\nElderly", fill="Proportion")
                 + geom_text(aes(label=format(round(value,2))), size = 4))
      
      # drawing the heat map
          
      layOut(list(heat7a, 1:17, 1),
             list(heat7b, 17:25, 1),
             list(heat7c, 25:34, 1),
             list(heat7e, 34:45, 1))
      
    } else if(input$select == 2 & input$radio == 1){
         # draw bar chart of response variable Embryonic stem cells 
         accept <- views_prop("Morally acceptable", "opt.q58b")
         wrong <- views_prop("Morally wrong", "opt.q58b")
         no_issue <- views_prop("Not a moral issue", "opt.q58b")
         depend <- views_prop("Depends on situation (VOL.)", "opt.q58b")
         
         xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
         view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
         view_DF <- melt(view_DF)
          
         (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                variable %in% unlist(input$checkGroup)), geom="bar",
                stat="identity", position="dodge", xlab="Optimism Score",
                ylab="Proportion")
          + theme_bw()
          + ggtitle("How optimism effects the views on research\non embryonic stem cells")
          + theme(plot.title=element_text(size=17,face="bold"))
          + scale_fill_discrete("Views on research\non embryonic stem cells", labels=c("Morally acceptable", 
                                 "Morally wrong", "Not an issue", "Depends on situation"))
          + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                      "1.5 -> 3", ">= 3")))
      } else if (input$select == 3 & input$radio == 1){
          # draw bar chart of views on research of non-human embryonic cells 
          accept <- views_prop("Morally acceptable", "opt.q58c")
          wrong <- views_prop("Morally wrong", "opt.q58c")
          no_issue <- views_prop("Not a moral issue", "opt.q58c")
          depend <- views_prop("Depends on situation (VOL.)", "opt.q58c")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
          view_DF <- melt(view_DF)
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects the views on research\non non-human embryonic stem cells")
           + theme(plot.title=element_text(size=17,face="bold"))
           +  scale_fill_discrete("Views on research\non non-human\nembrynoic stem cells", labels=c("Morally acceptable", 
                        "Morally wrong", "Not an issue", "Depends on situation"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
         
      } else if (input$select == 4 & input$radio == 1){
          # draw bar chart of response variable vitro fertilization
          accept <- views_prop("Morally acceptable", "opt.q58d")
          wrong <- views_prop("Morally wrong", "opt.q58d")
          no_issue <- views_prop("Not a moral issue", "opt.q58d")
          depend <- views_prop("Depends on situation (VOL.)", "opt.q58d")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
          view_DF <- melt(view_DF)
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects the views\non using vitro fertilization")
           + theme(plot.title=element_text(size=17,face="bold"))
           +  scale_fill_discrete("Views on vitro\nfertilization", labels=c("Morally acceptable", 
                  "Morally wrong", "Not an issue", "Depends on situation"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
      } else if (input$select == 5){
          # draw bar chart of response variable death penalty
          s_favor <- views_prop("Strongly favor", "opt.rq10")
          favor <- views_prop("Favor", "opt.rq10")
          oppose <- views_prop("Oppose", "opt.rq10")
          s_oppose <- views_prop("Strongly oppose", "opt.rq10")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, s_favor, favor, oppose, s_oppose)
          view_DF <- melt(view_DF)
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup5)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects the views on death penalty")
           + theme(plot.title=element_text(size=16,face="bold"))
           +  scale_fill_discrete("Views on death penalty", labels=c("Strongly favor",
                                "Favor", "Oppose", "Strongly Oppose"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
          
      } else if (input$select == 6 & input$radio == 1){
          # draw bar chart of response variable belief in life after death
          yes <- views_prop("Yes, believe in", "opt.rq9a")
          no <- views_prop("No, don't believe", "opt.rq9a")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, yes, no)
          view_DF <- melt(view_DF)
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                variable %in% unlist(input$checkGroup6)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects the belief in life after death")
           + theme(plot.title=element_text(size=16,face="bold"))
           +  scale_fill_discrete("Belief in life\nafter death", labels=c("Yes", "No"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
      } else if ((input$select == 6 | input$select == 7 | input$select == 8 | input$select == 9 | input$select == 10) & (input$radio == 2)){
        # draw heat map 
        yes_a <- views_prop("Yes, believe in", "opt.rq9a")
        no_a <- views_prop("No, don't believe", "opt.rq9a")
        
        yes_b <- views_prop("Yes, believe in", "opt.rq9bf13")
        no_b <- views_prop("No, don't believe", "opt.rq9bf13")
        
        yes_c <- views_prop("Yes, believe in", "opt.rq9cf24")
        no_c <- views_prop("No, don't believe", "opt.rq9cf24")
        
        yes_d <- views_prop("Yes, believe in", "opt.rq9df13")
        no_d <- views_prop("No, don't believe", "opt.rq9df13")
        
        yes_e <- views_prop("Yes, believe in", "opt.rq9ef24")
        no_e <- views_prop("No, don't believe", "opt.rq9ef24")
        
        # getting those into matrix
        answers <- c("answers", "Yes, believe in", "No, don't believe")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5", "1.5 -> 3", ">= 3")
        
        view_DF_a <- data.frame(xLabels, yes_a, no_a)
        colnames(view_DF_a) <- answers
        view_DF_a <- melt(view_DF_a)
        
        view_DF_b <- data.frame(xLabels, yes_b, no_b)
        colnames(view_DF_b) <- answers
        view_DF_b <- melt(view_DF_b)
        
        view_DF_c <- data.frame(xLabels, yes_c, no_c)
        colnames(view_DF_c) <- answers
        view_DF_c <- melt(view_DF_c)
        
        view_DF_d <- data.frame(xLabels, yes_d, no_d)
        colnames(view_DF_d) <- answers
        view_DF_d <- melt(view_DF_d)
        
        view_DF_e <- data.frame(xLabels, yes_e, no_e)
        colnames(view_DF_e) <- answers
        view_DF_e <- melt(view_DF_e)
        
        
        heata <- 
          # plotting a heat map with blue (default) color
          (ggplot(view_DF_a, aes(answers,  variable))
           + geom_tile(aes(fill = value), colour = "white")
           
           # background grid to only x and y axis
           + theme_classic()
           
           # reordering the x axis labels
           + scale_x_discrete(limits=xLabels)
           
           # changing the color of the map with middle color at 0.4
           + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4)
           
           # labels
           + labs(title="People's Views on beliefs after death", x=NULL, y="Life after\ndeath", fill="Proportion")
           
           # place color guide on the top, horizontally, 0.5 towards margin, 2 cm long,
           # change the size of title of the plot and make it bold,
           # and remove the x-axis labels and ticks (for the first three plots)
           + theme(legend.position="top", legend.direction="horizontal", 
                   plot.title=element_text(size=17,face="bold"),
                   axis.ticks=element_blank(), axis.text.x = element_blank())
           
           # the size of the text inside cells
           + geom_text(aes(label=format(round(value,2))), size = 4))
        
        heatb <- (ggplot(view_DF_b, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x=NULL, y="Rewarding\nheaven", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4)
                  + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
        
        heatc <- (ggplot(view_DF_c, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x=NULL, y="Heaven\n", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4)
                  + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
        
        heatd <- (ggplot(view_DF_d, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(y="Punishing hell\n", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4))
        
        heate <- (ggplot(view_DF_e, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x = "Optimism Score", y="Hell\n", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4))

        
        layOut(list(heata, 1:14, 1),
               list(heatb, 14:21, 1),
               list(heatc, 21:28, 1),
               list(heatd, 28:37, 1),
               list(heate, 37:46, 1))

        
      } else if (input$select == 7 & input$radio == 1){
          # draw bar chart of response variable belief in rewarded heaven 
          yes <- views_prop("Yes, believe in", "opt.rq9bf13")
          no <- views_prop("No, don't believe", "opt.rq9bf13")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, yes, no)
          view_DF <- melt(view_DF)
          
          
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup6)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects belief in rewarded heaven")
           + theme(plot.title=element_text(size=15,face="bold"))
           +  scale_fill_discrete("Belief in rewarded heaven", labels=c("Yes", "No"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))
          
      } else if (input$select == 8 & input$radio == 1){
          # draw bar chart of response variable belief in heaven 
          yes <- views_prop("Yes, believe in", "opt.rq9cf24")
          no <- views_prop("No, don't believe", "opt.rq9cf24")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, yes, no)
          view_DF <- melt(view_DF)
          
      
          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup6)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects belief in heaven")
           + theme(plot.title=element_text(size=16,face="bold"))
           +  scale_fill_discrete("Belief in heaven", labels=c("Yes", "No"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))

      } else if (input$select == 9 & input$radio == 1){
          # draw bar chart of response variable belief in punished hell
          yes <- views_prop("Yes, believe in", "opt.rq9df13")
          no <- views_prop("No, don't believe", "opt.rq9df13")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, yes, no)
          view_DF <- melt(view_DF)
          

          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup6)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects belief in punished hell")
           + theme(plot.title=element_text(size=15,face="bold"))
           +  scale_fill_discrete("Belief in punished hell", labels=c("Yes", "No"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))

      } else if (input$select == 10 & input$radio == 1){
        # draw bar chart of response variable belief in hell
        yes <- views_prop("Yes, believe in", "opt.rq9ef24")
        no <- views_prop("No, don't believe", "opt.rq9ef24")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, yes, no)
        view_DF <- melt(view_DF)
        

        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
               variable %in% unlist(input$checkGroup6)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects belief in hell")
         + theme(plot.title=element_text(size=16,face="bold"))
         +  scale_fill_discrete("Belief in hell", labels=c("Yes", "No"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))

      } else if (input$select == 11){
        # draw bar chart of response variable political party
        rep <- views_prop("Republican", "opt.party")
        demo <- views_prop("Democrat", "opt.party")
        inde <- views_prop("Independent", "opt.party")
        no_pref <- views_prop("No preference (VOL.)", "opt.party")
        other <- views_prop("Other party (VOL.)", "opt.party")
         
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                    "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, rep, demo, inde, other, no_pref)
        view_DF <- melt(view_DF)
         
        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF, 
               variable %in% unlist(input$checkGroup11)), geom="bar",
                stat="identity", position="dodge", xlab="Optimism Score",
                ylab="Proportion")
          + theme_bw()
          + ggtitle("How optimism effects the political party")
          + theme(plot.title=element_text(size=16,face="bold"))
          +  scale_fill_discrete("Political party", labels=c("Republican",
                         "Democrat", "Independent", "Other party", "No preference"))
          + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                      "1.5 -> 3", ">= 3")))
        
      } else if (input$select == 12){
          # draw bar chart of response variable ideology
          v_con <- views_prop("Very conservative", "opt.ideo")
          con <- views_prop("Conservative", "opt.ideo")
          mod <- views_prop("Moderate", "opt.ideo")
          lib <- views_prop("Liberal [OR]", "opt.ideo")
          v_lib <- views_prop("Very liberal", "opt.ideo")
          
          xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                       "1.5 -> 3", ">= 3")
          view_DF <- data.frame(xLabels, v_con, con, mod, lib, v_lib)
          view_DF <- melt(view_DF)
          

          (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
                 variable %in% unlist(input$checkGroup12)), geom="bar",
                 stat="identity", position="dodge", xlab="Optimism Score",
                 ylab="Proportion")
           + theme_bw()
           + ggtitle("How optimism effects\nthe ideology of political view")
           + theme(plot.title=element_text(size=16,face="bold"))
           +  scale_fill_discrete("Ideology", labels=c("Very conservative",
                      "Conservative", "Moderate", "Liberal", "Very liberal"))
           + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                       "1.5 -> 3", ">= 3")))

      } else if (input$select == 13 & input$radio == 1){
        # draw bar chart of response variable gays and lesbians couples having babies
        good <- views_prop("Good thing for society", "opt.q7a")
        no_diff <- views_prop("Doesn't make much difference", "opt.q7a")
        bad <- views_prop("Bad thing for society", "opt.q7a")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, good, no_diff, bad)
        view_DF <- melt(view_DF)
        

        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
               variable %in% unlist(input$checkGroup13)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects the views\non gays and lesbians couples with babies")
         + theme(plot.title=element_text(size=16,face="bold"))
         +  scale_fill_discrete("Views on Society", 
                                labels=c("Good", "No difference", "Bad"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))
      
      } else if ((input$select == 13 | input$select == 14 | input$select == 15 | input$select == 16) & (input$radio == 2)){
        
        # draw heat map 
        accept_58a <- views_prop("Morally acceptable", "opt.q58a")
        wrong_58a <- views_prop("Morally wrong", "opt.q58a")
        no_issue_58a <- views_prop("Not a moral issue", "opt.q58a")
        depend_58a <- views_prop("Depends on situation (VOL.)", "opt.q58a")
        
        accept_58b <- views_prop("Morally acceptable", "opt.q58b")
        wrong_58b <- views_prop("Morally wrong", "opt.q58b")
        no_issue_58b <- views_prop("Not a moral issue", "opt.q58b")
        depend_58b <- views_prop("Depends on situation (VOL.)", "opt.q58b")
        
        accept_58c <- views_prop("Morally acceptable", "opt.q58c")
        wrong_58c <- views_prop("Morally wrong", "opt.q58c")
        no_issue_58c <- views_prop("Not a moral issue", "opt.q58c")
        depend_58c <- views_prop("Depends on situation (VOL.)", "opt.q58c")
        
        accept_58d <- views_prop("Morally acceptable", "opt.q58d")
        wrong_58d <- views_prop("Morally wrong", "opt.q58d")
        no_issue_58d <- views_prop("Not a moral issue", "opt.q58d")
        depend_58d <- views_prop("Depends on situation (VOL.)", "opt.q58d")
        
        
        # getting those into matrix
        answers <- c("answers", "Morally acceptable", "Morally wrong",
                     "Not an issue", "Depends on situation")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5", "1.5 -> 3", ">= 3")
        
        view_DF_a <- data.frame(xLabels, accept_58a, wrong_58a, no_issue_58a,
                                depend_58a)
        colnames(view_DF_a) <- answers
        view_DF_a <- melt(view_DF_a)
        
        view_DF_b <- data.frame(xLabels, accept_58b, wrong_58b, no_issue_58b,
                                depend_58b)
        colnames(view_DF_b) <- answers
        view_DF_b <- melt(view_DF_b)
        
        view_DF_c <- data.frame(xLabels, accept_58c, wrong_58c, no_issue_58c,
                                depend_58c)
        colnames(view_DF_c) <- answers
        view_DF_c <- melt(view_DF_c)
        
        view_DF_d <- data.frame(xLabels, accept_58d, wrong_58d, no_issue_58d,
                                depend_58d)
        colnames(view_DF_d) <- answers
        view_DF_d <- melt(view_DF_d)
        
        heata <- 
          # plotting a heat map with blue (default) color
          (ggplot(view_DF_a, aes(answers,  variable))
           + geom_tile(aes(fill = value), colour = "white")
           
           # background grid to only x and y axis
           + theme_classic()
           
           # reordering the x axis labels
           + scale_x_discrete(limits=xLabels)
           
           # changing the color of the map with middle color at 0.4
           + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4)
           
           # labels
           + labs(title="People's Views on Moral Issue", x=NULL, y="Abortion", fill="Proportion")
           
           # place color guide on the top, horizontally, 0.5 towards margin, 2 cm long,
           # change the size of title of the plot and make it bold,
           # and remove the x-axis labels and ticks (for the first three plots)
           + theme(legend.position="top", legend.direction="horizontal", 
                   plot.title=element_text(size=17,face="bold"),
                   axis.ticks=element_blank(), axis.text.x = element_blank())
           
           # the size of the text inside cells
           + geom_text(aes(label=format(round(value,2))), size = 4))
        
        heatb <- (ggplot(view_DF_b, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x=NULL, y="Embryonic", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4)
                  + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
        
        heatc <- (ggplot(view_DF_c, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x=NULL, y="Non-human embryo", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4)
                  + theme(axis.ticks=element_blank(), axis.text.x = element_blank()))
        
        heatd <- (ggplot(view_DF_d, aes(answers,  variable))
                  + geom_tile(aes(fill = value), colour = "white")
                  + theme_classic()
                  + scale_x_discrete(limits=xLabels)
                  + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=0.4, guide="none")
                  + labs(x = "Optimism Score", y="Vitro", fill="Proportion")
                  + geom_text(aes(label=format(round(value,2))), size = 4))
        
        # drawing the heat map

        
        layOut(list(heata, 1:15, 1),
               list(heatb, 15:24, 1),
               list(heatc, 24:33, 1),
               list(heatd, 33:44, 1))

      } else if (input$select == 14 & input$radio == 1){
        # draw bar chart of response variable interracial marriage 
        good <- views_prop("Good thing for society", "opt.q7b")
        no_diff <- views_prop("Doesn't make much difference", "opt.q7b")
        bad <- views_prop("Bad thing for society", "opt.q7b")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, good, no_diff, bad)
        view_DF <- melt(view_DF)
        

        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
               variable %in% unlist(input$checkGroup13)), geom="bar",
               stat="identity", position="dodge")
         + theme_bw()
         + labs(title="How optimism effects the views\non interracial marriage",
                x="Optimism Score", y="Proportion")
         + theme(plot.title=element_text(size=17,face="bold"))
         +  scale_fill_discrete("Views on Society", 
                                labels=c("Good for society", "Not much difference",
                                         "Bad for society"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))

      } else if (input$select == 15 & input$radio == 1){
        # draw bar chart of response variable more non-religious people 
        good <- views_prop("Good thing for society", "opt.q7c")
        no_diff <- views_prop("Doesn't make much difference", "opt.q7c")
        bad <- views_prop("Bad thing for society", "opt.q7c")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, good, no_diff, bad)
        view_DF <- melt(view_DF)
        

        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
               variable %in% unlist(input$checkGroup13)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects the views\non more non-religious people")
         + theme(plot.title=element_text(size=17,face="bold"))
         +  scale_fill_discrete("Views on Society", 
                                labels=c("Good", "No difference", "Bad"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))

      } else if (input$select == 16 & input$radio == 1){
        # draw bar chart of response variable more Elderly people 
        good <- views_prop("Good thing for society", "opt.q7e")
        no_diff <- views_prop("Doesn't make much difference", "opt.q7e")
        bad <- views_prop("Bad thing for society", "opt.q7e")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, good, no_diff, bad)
        view_DF <- melt(view_DF)
        

        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF,
               variable %in% unlist(input$checkGroup13)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects the views on more Elderly")
         + theme(plot.title=element_text(size=17,face="bold"))
         +  scale_fill_discrete("Views on Society", 
                                labels=c("Good", "No difference", "Bad"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))

      } else if (input$select == 17){
        # draw bar chart of reponse variable view on doctor's decision
        yes <- views_prop("Sometimes let a patient die", "opt.q25")
        no <- views_prop("Always save a life", "opt.q25")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, yes, no)
        view_DF <- melt(view_DF)
        
        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF, variable %in% 
              unlist(input$checkGroup17)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects their view on doctor's decision")
         + theme(plot.title=element_text(size=15,face="bold"))
         +  scale_fill_discrete("What doctor\nshould do", labels=c("Sometimes let\na patient die",
                                                                   "Always save a life"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))

      } else if (input$select == 18){
        # draw bar chart of response variable on doctor's lethal doses of drugs prescription
        yes <- views_prop("Approve", "opt.q26")
        no <- views_prop("Disapprove", "opt.q26")
        
        xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                     "1.5 -> 3", ">= 3")
        view_DF <- data.frame(xLabels, yes, no)
        view_DF <- melt(view_DF)
                
        (qplot(x = xLabels, y = value, fill = variable, data=subset(view_DF, variable %in% 
               unlist(input$checkGroup18)), geom="bar",
               stat="identity", position="dodge", xlab="Optimism Score",
               ylab="Proportion")
         + theme_bw()
         + ggtitle("How optimism effects their view on\ndoctor's lethal doses of drugs prescription")
         + theme(plot.title=element_text(size=15,face="bold"))
         +  scale_fill_discrete("Lethal doses of drugs\nprescription", labels=c("Approve", "Disapprove"))
         + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                                     "1.5 -> 3", ">= 3")))
      })
                  
})