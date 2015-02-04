### drawing heatmap for three groups that contains similar topics, moral issues, societal
### issues and beliefs related to after death

# loading the required packages

if(!require(wq)) {
  install.packages("wq")
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
}

if(!require(reshape2)) {
  install.packages("reshpae2")
}

if(!require(grid)) {
  install.packages("grid")
}

library(wq)
library(ggplot2)
library(reshape2)
library(grid)

# sourcing func.R
source("func.R")

#-----------------------------------------------------------------------------
# loading the required packages
load("data/cleaned_pew.rdata")

load("data/cleaned_gallup.rdata")

#-----------------------------------------------------------------------------

### q7a-e
# dividing the optimism score into six groups
# getting the proportions for each question and each group
# Example: to find proportion who answered "good thing" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing heatmaps and combining in one plot
# and saving

# calculating the proportions
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
 + theme(legend.position="top", legend.direction="horizontal", legend.margin=unit(-0.5,"cm"),
         legend.key.width=unit(2, "cm"), plot.title=element_text(size=17,face="bold"),
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
jpeg("graphs/heatmap.jpg")

layOut(list(heat7a, 1:15, 1),
      list(heat7b, 15:24, 1),
      list(heat7c, 24:33, 1),
      list(heat7e, 33:44, 1))

dev.off()

#-----------------------------------------------------------------------------

### q58a-d
# dividing the optimism score into six groups
# getting the proportions for each question
# Example: to find proportion who answered "Morally acceptable" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing the heat maps and putting all together in one plot
# and saving

# calculating the proportions

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
   + theme(legend.position="top", legend.direction="horizontal", legend.margin=unit(-0.5,"cm"),
           legend.key.width=unit(2, "cm"), plot.title=element_text(size=17,face="bold"),
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
jpeg("graphs/heatmap_58.jpg")

layOut(list(heata, 1:15, 1),
       list(heatb, 15:24, 1),
       list(heatc, 24:33, 1),
       list(heatd, 33:44, 1))

dev.off()

#-----------------------------------------------------------------------------

### rq9a-e
# dividing the optimism score into six groups
# getting the proportions for each question
# Example: to find proportion who answered "Yes, believe in" and scored less than -3,
# divide the total number who answered that and scored less than -3 by the total number
# who scored < -3
# drawing the heat maps and putting all together in one plot
# and saving

# calculating the proportions
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
   + theme(legend.position="top", legend.direction="horizontal", legend.margin=unit(-0.5,"cm"),
           legend.key.width=unit(2, "cm"), plot.title=element_text(size=17,face="bold"),
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

# drawing the heat map
jpeg("graphs/heatmap_rq9.jpg")

layOut(list(heata, 1:14, 1),
       list(heatb, 14:21, 1),
       list(heatc, 21:28, 1),
       list(heatd, 28:37, 1),
       list(heate, 37:46, 1))

dev.off()

#-----------------------------------------------------------------------------