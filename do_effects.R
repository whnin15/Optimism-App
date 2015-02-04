  ### draw graphs for optimism score vs. response variables

  # loading the required packages and sourcing the func.R

if(!require(ggplot2)) {
  install.packages("ggplot2")
}

if(!require(reshape2)) {
  install.packages("reshpae2")
}

library(ggplot2)
library(reshape2)

source("func.R")
#-----------------------------------------------------------------------------
  ### loading the datasets
load("data/cleaned_pew.rdata")

load("data/cleaned_gallup.rdata")

#-----------------------------------------------------------------------------

### q7a
# dividing the optimism score into six groups
# finding the proportions for the question, more gay and lesbian couples raising children
# Example: to find proportion who answered "good thing" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on more gay and lesbian couples raising children
# and saving

good <- views_prop("Good thing for society", "opt.q7a")
no_diff <- views_prop("Doesn't make much difference", "opt.q7a")
bad <- views_prop("Bad thing for society", "opt.q7a")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, good, no_diff, bad)
view_DF <- melt(view_DF)

#jpeg("graphs/gays_babies.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views\non gays and lesbians couples with babies")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Views on Society", 
                        labels=c("Good", "No difference", "Bad"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

###q7b
# dividing the optimism score into six groups
# finding the proportions for the question, more people of different races marrying each other
# Example: to find proportion who answered "good thing" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on more people of different races marrying each other
# and saving

good <- views_prop("Good thing for society", "opt.q7b")
no_diff <- views_prop("Doesn't make much difference", "opt.q7b")
bad <- views_prop("Bad thing for society", "opt.q7b")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, good, no_diff, bad)
view_DF <- melt(view_DF)

#jpeg("graphs/interracial.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
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
#dev.off()

#-----------------------------------------------------------------------------

### q7c
# dividing the optimism score into six groups
# finding the proportions for the question, more non-religious people
# Example: to find proportion who answered "good thing" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on more non-religious people
# and saving

good <- views_prop("Good thing for society", "opt.q7c")
no_diff <- views_prop("Doesn't make much difference", "opt.q7c")
bad <- views_prop("Bad thing for society", "opt.q7c")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, good, no_diff, bad)
view_DF <- melt(view_DF)

#jpeg("graphs/non_religious.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views\non more non-religious poeple")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on Society", 
                        labels=c("Good", "No difference", "Bad"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q7e
# dividing the optimism score into six groups
# finding the proportions for the question, more elderly people
# Example: to find proportion who answered "good thing" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on more elderly people
# and saving

good <- views_prop("Good thing for society", "opt.q7e")
no_diff <- views_prop("Doesn't make much difference", "opt.q7e")
bad <- views_prop("Bad thing for society", "opt.q7e")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, good, no_diff, bad)
view_DF <- melt(view_DF)

#jpeg("graphs/elderly.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views on more Elderly")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on Society", 
                        labels=c("Good", "No difference", "Bad"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q58a
# dividing the optimism score into six groups
# finding the proportions for the question, having an abortion is morally acceptable
# Example: to find proportion who answered "morally acceptable" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on abortion
# and saving

accept <- views_prop("Morally acceptable", "opt.q58a")
wrong <- views_prop("Morally wrong", "opt.q58a")
no_issue <- views_prop("Not a moral issue", "opt.q58a")
depend <- views_prop("Depends on situation (VOL.)", "opt.q58a")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
view_DF <- melt(view_DF)

#jpeg("graphs/abortion.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views on abortion")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on Abortion", labels=c("Morally acceptable", 
                "Morally wrong", "Not an issue", "Depends on situation"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q58b
# dividing the optimism score into six groups
# finding the proportions for the question, research on embryonic stem cells is morally acceptable
# Example: to find proportion who answered "morally acceptable" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on research on embryonic stem cells
# and saving

accept <- views_prop("Morally acceptable", "opt.q58b")
wrong <- views_prop("Morally wrong", "opt.q58b")
no_issue <- views_prop("Not a moral issue", "opt.q58b")
depend <- views_prop("Depends on situation (VOL.)", "opt.q58b")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
view_DF <- melt(view_DF)

#jpeg("graphs/stem_cells.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views on research\non embryonic stem cells")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on research\non embryonic stem cells", labels=c("Morally acceptable", 
                        "Morally wrong", "Not an issue", "Depends on situation"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q58c
# dividing the optimism score into six groups
# finding the proportions for the question, research on non-human embryonic stem cells is morally acceptable
# Example: to find proportion who answered "morally acceptable" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on research on non-human embryonic stem cells
# and saving

accept <- views_prop("Morally acceptable", "opt.q58c")
wrong <- views_prop("Morally wrong", "opt.q58c")
no_issue <- views_prop("Not a moral issue", "opt.q58c")
depend <- views_prop("Depends on situation (VOL.)", "opt.q58c")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
view_DF <- melt(view_DF)

#jpeg("graphs/nonhuman_cells.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views on research\non non-human embryonic stem cells")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on research\non non-human\nembrynoic stem cells", labels=c("Morally acceptable", 
              "Morally wrong", "Not an issue", "Depends on situation"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q58d
# dividing the optimism score into six groups
# finding the proportions for the question, using vitro fertilization is morally acceptable
# Example: to find proportion who answered "morally acceptable" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the view on using vitro fertilization
# and saving

accept <- views_prop("Morally acceptable", "opt.q58d")
wrong <- views_prop("Morally wrong", "opt.q58d")
no_issue <- views_prop("Not a moral issue", "opt.q58d")
depend <- views_prop("Depends on situation (VOL.)", "opt.q58d")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, accept, wrong, no_issue, depend)
view_DF <- melt(view_DF)

#jpeg("graphs/vitro.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views\non using vitro fertilization")
 + theme(plot.title=element_text(size=17,face="bold"))
 +  scale_fill_discrete("Views on vitro\nfertilization", labels=c("Morally acceptable", 
        "Morally wrong", "Not an issue", "Depends on situation"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------
  
  ### rq10
  # dividing the optimism score into six groups
  # finding the proportions for the question, death penalty for people who convicted murder
  # Example: to find proportion who answered "Favor" and scored less than -3,
  # divide the total number who answered that and scored less than -3 by the total number
  # who scored < -3
  # drawing how optimism effects the view on death penalty
  # and saving
  
oppose <- views_prop("Oppose", "opt.rq10")
s_oppose <- views_prop("Strongly oppose", "opt.rq10")
s_favor <- views_prop("Strongly favor", "opt.rq10")
favor <- views_prop("Favor", "opt.rq10")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, s_favor, favor, oppose, s_oppose)
view_DF <- melt(view_DF)

#jpeg("graphs/death_penalty.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the views on death penalty")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Views on death penalty", labels=c("Stronly favor",
                      "Favor", "Oppose", "Strongly Oppose"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

  ### rq9a
  # dividing the optimism score into six groups
  # finding the proportions for the question, belief in life after death
  # Example: to find proportion who answered "Yes, believe in" and scored less than -3,
  # divide the total number who answered that and scored less than -3 by the total number
  # who scored < -3
  # drawing how optimism effects the belief in life after death
  # and saving

yes <- views_prop("Yes, believe in", "opt.rq9a")
no <- views_prop("No, don't believe", "opt.rq9a")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/life_after_death.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the belief in life after death")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Belief in life\nafter death", labels=c("Yes", "No"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### rq9b
# dividing the optimism score into six groups
# finding the proportions for the question, belief in heaven, where people who lived well are rewarded
# Example: to find proportion who answered "Yes, believe in" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the belief in heaven, where people who lived well are rewarded
# and saving

yes <- views_prop("Yes, believe in", "opt.rq9bf13")
no <- views_prop("No, don't believe", "opt.rq9bf13")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/rewarded_heaven.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects belief in rewarded heaven")
 + theme(plot.title=element_text(size=15,face="bold"))
 +  scale_fill_discrete("Belief in rewarded heaven", labels=c("Yes", "No"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### rq9c
# dividing the optimism score into six groups
# finding the proportions for the question, belief in heaven
# Example: to find proportion who answered "Yes, believe in" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the belief in heaven
# and saving

yes <- views_prop("Yes, believe in", "opt.rq9cf24")
no <- views_prop("No, don't believe", "opt.rq9cf24")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/heaven.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects belief in heaven")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Belief in heaven", labels=c("Yes", "No"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### rq9d
# dividing the optimism score into six groups
# finding the proportions for the question, belief in hell, where people who lived badly are punished
# Example: to find proportion who answered "Yes, believe in" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the belief in hell, where people who lived badly are punished
# and saving

yes <- views_prop("Yes, believe in", "opt.rq9df13")
no <- views_prop("No, don't believe", "opt.rq9df13")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/punished_hell.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects belief in punished hell")
 + theme(plot.title=element_text(size=15,face="bold"))
 +  scale_fill_discrete("Belief in punished hell", labels=c("Yes", "No"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### rq9e
# dividing the optimism score into six groups
# finding the proportions for the question, belief in hell
# Example: to find proportion who answered "Yes, believe in" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects the belief in hell
# and saving

yes <- views_prop("Yes, believe in", "opt.rq9ef24")
no <- views_prop("No, don't believe", "opt.rq9ef24")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/hell.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects belief in hell")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Belief in hell", labels=c("Yes", "No"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### party
# dividing the optimism score into six groups
# finding the proportions for the question, their political party
# Example: to find proportion who answered "Republican" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects their political party
# and saving

rep <- views_prop("Republican", "opt.party")
demo <- views_prop("Democrat", "opt.party")
inde <- views_prop("Independent", "opt.party")
no_pref <- views_prop("No preference (VOL.)", "opt.party")
other <- views_prop("Other party (VOL.)", "opt.party")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, rep, demo, inde, other, no_pref)
view_DF <- melt(view_DF)

#jpeg("graphs/party.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects the political party")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Political party", labels=c("Republican",
                "Democrat", "Independent", "Other party", "No preference"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### ideo
# dividing the optimism score into six groups
# finding the proportions for the question, their ideology of political view
# Example: to find proportion who answered "Republican" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing how optimism effects their ideology of political view
# and saving

v_con <- views_prop("Very conservative", "opt.ideo")
con <- views_prop("Conservative", "opt.ideo")
mod <- views_prop("Moderate", "opt.ideo")
lib <- views_prop("Liberal [OR]", "opt.ideo")
v_lib <- views_prop("Very liberal", "opt.ideo")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, v_con, con, mod, lib, v_lib)
view_DF <- melt(view_DF)

#jpeg("graphs/ideo.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects\nthe ideology of political view")
 + theme(plot.title=element_text(size=16,face="bold"))
 +  scale_fill_discrete("Ideology", labels=c("Very conservative",
            "Conservative", "Moderate", "Liberal", "Very liberal"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------

### q25
# drawing how optimism score effects the answers for the question, if a patient's life
# should always be saved or sometimes allowed to die, and saving 

yes <- views_prop("Sometimes let a patient die", "opt.q25")
no <- views_prop("Always save a life", "opt.q25")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/life_save_bar.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects their view on doctor's decision")
 + theme(plot.title=element_text(size=15,face="bold"))
 +  scale_fill_discrete("What doctor\nshould do", labels=c("Sometimes let\na patient die",
                                 "Always save a life"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))

#dev.off()

#-----------------------------------------------------------------------------

### q26
# drawing how optimism score effects the answers for the question, if doctors
# should help the patients end their life, and saving 

yes <- views_prop("Approve", "opt.q26")
no <- views_prop("Disapprove", "opt.q26")

xLabels <- c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
             "1.5 -> 3", ">= 3")
view_DF <- data.frame(xLabels, yes, no)
view_DF <- melt(view_DF)

#jpeg("graphs/lethal_drugs.jpg")
(qplot(x = xLabels, y = value, fill = variable, data=view_DF, geom="bar",
       stat="identity", position="dodge", xlab="Optimism Score",
       ylab="Proportion")
 + theme_bw()
 + ggtitle("How optimism effects their view on\ndoctor's lethal doses of drugs prescription")
 + theme(plot.title=element_text(size=15,face="bold"))
 + scale_fill_discrete("Lethal doses of drugs\nprescription", labels=c("Approve", "Disapprove"))
 + scale_x_discrete(limits=c("< -3", "-3 -> -1.5", "-1.5 -> 0", "0 -> 1.5",
                             "1.5 -> 3", ">= 3")))
#dev.off()

#-----------------------------------------------------------------------------