# Draw boxplots for explanatory vairables vs. optimism score  

  # loading the required packages and do not install if already installed them
if(!require(animation)) {
  install.packages("animation")
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
}

library(ggplot2)
library(animation)
#-----------------------------------------------------------------------------

# loading the cleaned pew dataset
load("data/cleaned_pew.rdata")

#--------------------------------------------------------------------------------

# loading the cleaned gallup dataset
load("data/cleaned_gallup.rdata")

#--------------------------------------------------------------------------------

### printing out graphs for the distribution of optimism score

# save a gif of histograms of the % of optimism scores of the datasets with
# the mean line in the same path as this file (do.R)
# output: histogram.gif
saveGIF({
  hist((original_table$optimism_score * 10) + 50, xlim=c(0,100), xlab="Optimism score in %",
       main = "Percentage of Optimism Score from Pew Dataset")
  abline(v=mean((original_table$optimism_score * 10)+50), col = "red", lwd = 2)
  
  hist(gallup$X..City.optimism, xlim=c(0,100),  xlab="Optimism score in %",
       main = "Percentage of Optimism Score from Gallup Dataset")
  abline(v=mean(gallup$X..City.optimism), col = "red", lwd = 2)
  
}, movie.name="histogram.gif", 
interval=1, ani.width=550, ani.height=350, cmd.fun=system)


# saving histograms that displays the distribution of optimism scores as separated jpg files
# output: histogram for pew dataset - graphs/hist_pew.jpg
jpeg("graphs/hist_pew.jpg")
hist((original_table$optimism_score * 10) + 50, xlim=c(0,100), xlab="Optimism score in %",
     main = "Percentage of Optimism Score from Pew Dataset")
abline(v=mean((original_table$optimism_score * 10)+50), col = "red", lwd = 2)
dev.off()

# output: histogram for pew dataset - graphs/hist_gallup.jpg
jpeg("graphs/hist_gallup.jpg")
hist(gallup$X..City.optimism, xlim=c(0,100),  xlab="Optimism score in %",
     main = "Percentage of Optimism Score from Gallup Dataset")
abline(v=mean(gallup$X..City.optimism), col = "red", lwd = 2)
dev.off()

#--------------------------------------------------------------------------------

#GRAPHS USED IN PRESENTATION

survey <- original_table

#Gender Boxplot
jpeg("graphs/gender_score.jpg")
ggplot(data=survey, aes(opt.sex, optimism_score))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot(aes(fill=factor(opt.sex)))+
  scale_fill_manual(values = c("red", "blue"))+
  ggtitle("Optimism Score by Gender")+
  xlab("Gender") + ylab("Optimism Score")+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=15,face="bold"), 
        plot.title=element_text(size=25, face="bold"), 
        legend.text=element_text(size=12, face="bold"), panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        panel.grid.major.y = element_line(colour = "bisque4"))+
  labs(fill="")+scale_y_continuous(breaks=-5:5)
dev.off()

#--------------------------------------------------------------------------------
# age vs. optimism score
#make age brackets
age <- original_table$opt.agerec

original_table$age_gp <- character(dim(original_table)[1])

# get age groups
original_table$age_gp[which(age >= 18 & age <= 30)] <- "18 - 30"
original_table$age_gp[which(age > 30 & age <= 44)] <- "31 - 44"
original_table$age_gp[which(age > 44 & age <= 58)] <- "45 - 58"
original_table$age_gp[which(age > 58 & age <= 72)] <- "59 - 72"
original_table$age_gp[which(age > 72 & age <= 86)] <- "73 - 86"
original_table$age_gp[which(age > 86)] <- "> 86"

original_table$age_gp <- factor(original_table$age_gp, c("18 - 30", "31 - 44", "45 - 58",
                                                         "59 - 72", "73 - 86", "> 86"))

#Age Boxplot
jpeg("graphs/age_score.jpg")
ggplot(data=original_table, aes(age_gp, optimism_score))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=factor(age_gp)))+
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+
  ggtitle("Optimism Score by Age")+
  xlab("Age") + ylab("Optimism Score")+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=15,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        panel.grid.major.y = element_line(colour = "bisque4"))+
  labs(fill="")+scale_y_continuous(breaks=-5:5)
dev.off()
#--------------------------------------------------------------------------------

# income vs. optimism score
#Get income groups
income <- original_table$opt.income

original_table$income_gp <- character(dim(original_table)[1])

original_table$income_gp[which(income== "Less than $10,000")] <- "< $10,000"
original_table$income_gp[which(income== "$10,000 to under $20,000")] <- "$10,000-20,000"
original_table$income_gp[which(income== "$20,000 to under $30,000")] <- "$20,000-30,000"
original_table$income_gp[which(income== "$30,000 to under $40,000")] <- "$30,000-40,000"
original_table$income_gp[which(income== "$40,000 to under $50,000")] <- "$40,000-50,000"
original_table$income_gp[which(income== "$50,000 to under $75,000")] <- "$50,000-75,000"
original_table$income_gp[which(income== "$75,000 to under $100,000")] <- "$75,000-100,000"
original_table$income_gp[which(income== "$100,000 to under $150,000")] <- "$100,000-150,000"
original_table$income_gp[which(income== "$150,000 or over")] <- "> $150,000"

original_table$income_gp <- factor(original_table$income_gp , c("< $10,000", "$10,000-20,000", "$20,000-30,000",
                                                                "$30,000-40,000", "$40,000-50,000", "$50,000-75,000", "$75,000-100,000",
                                                                "$100,000-150,000", "> $150,000"))


#Income Boxplot
jpeg("graphs/income_score.jpg")
ggplot(subset(original_table, !is.na(income_gp)), aes(income_gp, optimism_score))+
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(aes(fill=factor(income_gp)))+
  scale_fill_manual(values = c("red", "pink", "purple", "blue", "dark blue", "green", "dark green", "yellow", "orange"))+
  ggtitle("Optimism Score by Income")+
  xlab("Income") + ylab("Optimism Score")+
  scale_x_discrete(labels=c("<10","10-20","20-30","30-40","40-50", "50-75", "75-100", "100-150",">150"))+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=12,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        panel.grid.major.y = element_line(colour = "bisque4"))+
  labs(fill="")+scale_y_continuous(breaks=-5:5)
dev.off()

#--------------------------------------------------------------------------------

#Good Boxplot of Marital
ggplot(subset(survey, !is.na(opt.marital)), aes(opt.marital, optimism_score))+
  geom_boxplot(aes(fill=factor(opt.marital)))+
  scale_fill_manual(values = c("red", "orange", "purple", "green", "blue", "yellow"))+
  ggtitle("Optimism Score by Marital Status")+
  xlab("Marital Status") + ylab("Optimism Score")+
  scale_x_discrete(labels=c("D","LWP","M","NM","S", "W"))+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=10.5,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        legend.text=element_text(size=12, face="bold"), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank())+
  labs(fill="")+scale_y_continuous(breaks=-5:5)

#--------------------------------------------------------------------------------

#Good Boxplot of Employment
ggplot(subset(survey, !is.na(opt.employ)), aes(opt.employ, optimism_score))+
  geom_boxplot(aes(fill=factor(opt.employ)))+
  scale_fill_manual(values = c("red", "orange", "purple"))+
  ggtitle("Optimism Score by Employment")+
  xlab("Employment Status") + ylab("Optimism Score")+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=10.5,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        legend.text=element_text(size=12, face="bold"), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank())+
  labs(fill="")+scale_y_continuous(breaks=-5:5)

#--------------------------------------------------------------------------------

#Boxplot of Religion
ggplot(subset(survey, !is.na(opt.relig)), aes(opt.relig, optimism_score))+
  geom_boxplot(aes(fill=factor(opt.relig)))+
  scale_fill_manual(values = c("red", "orange", "purple", "green", "blue", "yellow", "dark blue", "dark green", "pink", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "dark red"))+
  ggtitle("Optimism Score by Religion")+
  xlab("Religion") + ylab("Optimism Score")+
  scale_x_discrete(labels=c("Ag","At","B","Ca","Ch", "H", "J", "Mo", "Mu", "NA", "O", "P", "So", "U"))+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=10.5,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        legend.text=element_text(size=12, face="bold"), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank())+
  labs(fill="")+scale_y_continuous(breaks=-5:5)

#--------------------------------------------------------------------------------

#Boxplot of Race
ggplot(subset(survey, !is.na(opt.racethn)), aes(opt.racethn, optimism_score))+
  geom_boxplot(aes(fill=factor(opt.racethn)))+
  scale_fill_manual(values = c("red", "orange", "purple", "green"))+
  ggtitle("Optimism Score by Race")+
  xlab("Race") + ylab("Optimism Score")+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=10.5,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        legend.text=element_text(size=12, face="bold"), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank())+
  labs(fill="")+scale_y_continuous(breaks=-5:5)

#--------------------------------------------------------------------------------

#Education Boxplot
original_table$opt.educ2 <- as.character(original_table$opt.educ2)
original_table$opt.educ2[grep("Four year", original_table$opt.educ2)] <- "Four year College"
original_table$opt.educ2[grep("Some college", original_table$opt.educ2)] <- "No degree College"
original_table$opt.educ2[grep("Postgraduate", original_table$opt.educ2)] <- "Postgraduate"
original_table$opt.educ2[grep("High school graduate", original_table$opt.educ2)] <- "High School Graduate"
original_table$opt.educ2[grep("Less than high school", original_table$opt.educ2)] <- "Less than high school"
original_table$opt.educ2[grep("High school incomplete", original_table$opt.educ2)] <- "High school incomplete"
original_table$opt.educ2[grep("Two year associate", original_table$opt.educ2)] <- "Two year associate degree"
original_table$opt.educ2[grep("Some postgraduate or", original_table$opt.educ2)] <- "Some postgraduate"

original_table$opt.educ2 <- factor(original_table$opt.educ2, c("Less than high school", "High school incomplete", "High School Graduate",
                                                               "No degree College", "Two year associate degree", "Four year College", 
                                                               "Some postgraduate", "Postgraduate"))

ggplot(subset(original_table, !is.na(opt.educ2)), aes(opt.educ2, optimism_score))+
  geom_boxplot(aes(fill=factor(opt.educ2)))+
  scale_fill_manual(values = c("red", "orange", "purple", "green", "blue", "yellow", "dark blue", "dark green"))+
  ggtitle("Optimism Score by Education")+
  xlab("Education") + ylab("Optimism Score")+
  scale_x_discrete(labels=c("< HS","HS I","HS","No Clg","2y Clg", "4y Clg", "PG i"))+
  theme(axis.title=element_text(size=19),
        axis.text=element_text(size=10.5,face="bold"), 
        plot.title=element_text(size=25, face="bold"),
        legend.text=element_text(size=12, face="bold"), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank())+
  labs(fill="")+scale_y_continuous(breaks=-5:5)

#--------------------------------------------------------------------------------
