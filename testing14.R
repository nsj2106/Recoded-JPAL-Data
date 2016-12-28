rm(list=ls()) 
setwd("/Users/nsj2106/Desktop/Police Story/JPAL Analysis/JPAL Data")
install.packages('foreign', repos = "http://cran.us.r-project.org")
library('foreign')

install.packages('ggplot2', repos = "http://cran.us.r-project.org")
library(ggplot2)

#a package that helps ggplot; turning data from wide to long. 
install.packages('reshape2', repos = "http://cran.us.r-project.org")
library(reshape2)


##loading the household level data from JPAL
#hh = read.dta("hhsurvey_base.dta")

##loading crime incident survey 
#cs = read.dta("crimeincidentsurvey_base.dta")

##loading public opinion survey (will ignore police survey because of social desirability bias)
#pos = read.dta("publicopinionsurvey_base.dta")

#merging pos and hh by "hhid" (household ID and creating a new larger dataset) (already done)
##hhpos = merge(hh,pos,by="hhid")

#write merged "total" (of hh and pos) to CSV to I can add jatis in Excel (already done)
##write.table(hhpos, file = "hhpos2.csv",row.names=FALSE, col.names=TRUE, sep=",") #Version 2

#lets see if it works (and it does!)
#hhpos_final.csv is the original merged, clean file with Muslim OBCs and SC/STs separated. 
#new = read.csv("hhpos_final.csv")

################

#read new data
new2 = read.csv("final3.csv")

##Crosstabulations with new Dataset (I'm merging Muslim OBCs into OBC category and combining 
#SCs and STs into one category: SC/ST). There are of course problems with this- tribal groups are 
#different from scheduled caste, but these data are to provide a rough estimate of how certain groups
#interact with the police. 

#names of variables
names(new2)

#names of castecdoe- to make sure I coded correctly
table(new2$castecode)

#26.7 percent general, 39 percent OBC, 23 percent SC/ST, and 1 percent Muslim
prop.table(table(new2$castecode))
barplot(table(new2$castecode))

#Now sorting to find out what are the dominant Jatis in each category and did I code them correctly. 
test = data.frame(table(new2$castename, new2$castecode))

General=subset(test, test$Var2=='General')
head(General[ order(General$Freq, decreasing=TRUE), ])

OBC=subset(test, test$Var2=='OBC')
head(OBC[ order(OBC$Freq, decreasing=TRUE), ])

SC_ST=subset(test, test$Var2=='SC_ST')
head(SC_ST[ order(SC_ST$Freq, decreasing=TRUE), ])

Other=subset(test, test$Var2=='Other')
head(Other[ order(Other$Freq, decreasing=TRUE), ])

Muslim_General=subset(test, test$Var2=='Muslim_General')
head(Muslim_General[ order(Muslim_General$Freq, decreasing=TRUE), ])


General = c(120, 52,44,15,13)
Other = c(42, 3,2,2,2)
Muslim_General = c(23,23,19,10,8)
OBC = c(162,78,57,54,38)
SC_ST = c(147,97,40,35,21)

#Putting variables above into a data.frame because that what ggplot2 uses.
dtest <- data.frame(General, Other, Muslim_General, OBC, SC_ST)
dtest

#Now using reshape2 to switch it from wide to long format. This is better for variables comparison. 
dtestm <- melt(dtest)
dtestm

#adding a new column called 'rowid' as an identifying variable. 
dtestm$rowid <- 1:25
dtestm

#adding new column where I label women and men for each region one numerical value. This will help me color in ggplot2.
dtestm$wtf <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
dtestm

#new column of labels
dtestm$labels = c("Rajput","Sharma","Brahman","Agarwal","Rajpoot",
                  "Jain","Punjabi","Gari","Haji","Himpa",
                  "Musalman","Muslim","Pathan","Khan", "Sheikh",
                  "Jaat","Rawat","Yadav","Kumhar","Gurjar",
                  "Meena","Meghwal","Bheel","Regar","Jatav")
dtestm

#Breakdown of Caste Categories and Levels of Literacy in JPAL Dataset
figure_1 <- ggplot(dtestm, aes(variable, value, fill=factor(wtf), group=factor(rowid))) + 
  geom_bar(stat="identity", colour = "grey", width = .7, position=position_dodge(0.9)) + 
  geom_text(aes(label=labels), vjust=0.5, hjust=-0.01, colour = "black", angle = 0, position=position_dodge(.9))+
  xlab("") + ylab("No. of Respondents") + ggtitle("Self-Identified Jatis for Each Caste Category") + 
  theme(text=element_text(size=15), axis.text.x=element_text(angle=0, hjust=0.5, vjust=0.5)) +  
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() + scale_fill_brewer(palette = "Paired") +
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 0))
figure_1

#Can you read this paragraph? 
prop.table(table(new2$castecode, new2$literateyn),1)
ptable1 = as.data.frame(prop.table(table(new2$castecode, new2$literateyn),1))
ptable1

figure_2 <- ggplot(ptable1, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Can you read this paragraph? (Are you literate?)") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_2


#####SC/ST and Interactions With the Police 
prop.table(table(new2$castecode, new2$thanavisityn),1)
ptable2 = as.data.frame(prop.table(table(new2$castecode, new2$thanavisityn),1))
ptable2

figure_3 <- ggplot(ptable2, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Have you visited a police station in the past 12 months?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_3 


#Which constable patrols your village? 
#90 percent of SCST don't see any constable. report not seeing any cop in their village
prop.table(table(new2$castecode, new2$patroller_code),1)
constable = new2$patroller_code
levels(constable)
levels(constable)[2] = "Don't Know"
levels(constable)
levels(constable)[2] = "Know the constable"
levels(constable)
levels(constable)[3] = "Know the constable"
levels(constable)
levels(constable)[3] = "Know the constable"
levels(constable)

prop.table(table(new2$castecode, constable),1)
ptable0 = as.data.frame(prop.table(table(new2$castecode, constable),1))
ptable0

figure_4 <- ggplot(ptable0, aes(Var1, Freq, fill=factor(constable), group=factor(constable))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Which constable patrols your village? Do you know his name?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_4 


#how do the police behave with normal citizens? Appears to be social desirability bias. FULL of it. 
#other surveys conducted by NES show that police most loathed institution. 
prop.table(table(new2$castecode, new2$policebehavior),1)
behavior = new2$policebehavior
levels(behavior)
levels(behavior)[1] = 'courteous'
levels(behavior)
levels(behavior)[2] = 'rude'
levels(behavior)
levels(behavior)[4] = 'courteous'
levels(behavior)
levels(behavior)[4] = 'rude'
levels(behavior)
prop.table(table(new2$castecode, behavior),1)


behavior2 = as.data.frame(prop.table(table(new2$castecode, behavior),1))
behavior2


figure_5 <- ggplot(behavior2, aes(Var1, Freq, fill=factor(behavior), group=factor(behavior))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("How do the police behave with normal citizens?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_5


#would you say it's easy or difficult to file an FIR?
fireasy = new2$firregistereasy
levels(fireasy)[1] = 'Depends'
levels(fireasy)
levels(fireasy)[2] = 'Depends'
levels(fireasy)
levels(fireasy)[2] = 'Depends'
levels(fireasy)
prop.table(table(new2$castecode, fireasy),1)


ptable7 = as.data.frame(prop.table(table(new2$castecode, fireasy),1))
ptable7

figure_6 <- ggplot(ptable7, aes(Var1, Freq, fill=factor(fireasy), group=factor(fireasy))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Would you say it's easy or difficult to file an FIR?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_6

#Do the police help citizens when they are required? 
#majority say police help. Doesn't make sense. 
policehelp = new2$policehelpful
levels(policehelp)
levels(policehelp)[1] = 'They Help'
levels(policehelp)
levels(policehelp)[3] = 'They Help'
levels(policehelp)
levels(policehelp)[3] = 'Rarely'
levels(policehelp)
levels(policehelp)[6] = 'NA'
levels(policehelp)
prop.table(table(new2$castecode, policehelp),1)

ptable5 = as.data.frame(prop.table(table(new2$castecode, policehelp),1))
ptable5

figure_7 <- ggplot(ptable5, aes(Var1, Freq, fill=factor(policehelp), group=factor(policehelp))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Do the police help when they are required?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_7 


#SHould the size of the police force increase or decrease? Over half want more police! That's interesting.
prop.table(table(new2$castecode, new2$policesize),1)
size = as.data.frame(prop.table(table(new2$castecode, new2$policesize),1))
size

figure_8 <- ggplot(size, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Should size of the police force be increased or decreased?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_8 



#SC/ST Perceptions of Police 
#Are the police in your area even handed? So no, they aren't evenhanded. Unclear. 
prop.table(table(new2$castecode, new2$policefair),1)

ptable11 = as.data.frame(prop.table(table(new2$castecode, new2$policefair),1))
ptable11

figure_9 <- ggplot(ptable11, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Are the police in your area evenhanded, or do they take sides in disputes?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_9


#Do you think citizens like you fear the police? 
prop.table(table(new2$castecode, new2$fearpolice),1)

ptable8 = as.data.frame(prop.table(table(new2$castecode, new2$fearpolice),1))
ptable8

figure_10 <- ggplot(ptable8, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Do you think citizens like yourself are afraid of the police?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_10


#Are police in your area generally honest or generally corrupt? Nothing unusual. 
prop.table(table(new2$castecode, new2$corruption),1)

ptable10 = as.data.frame(prop.table(table(new2$castecode, new2$corruption),1))
ptable10

figure_11 <- ggplot(ptable10, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Would you say the police in your area are generally honest or corrupt?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_11



#how do police treat in custody? but differences in groups.
prop.table(table(new2$castecode, new2$treatmentbypolice),1)

cruelty = new2$treatmentbypolice
levels(cruelty)
levels(cruelty)[1] = "Usually cruel"
levels(cruelty)
levels(cruelty)[3] = "Rarely cruel" 
levels(cruelty)

prop.table(table(new2$castecode, cruelty),1)
mad = as.data.frame(prop.table(table(new2$castecode, cruelty),1))
mad

figure_12 <- ggplot(mad, aes(Var1, Freq, fill=factor(cruelty), group=factor(cruelty))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("How do the police treat those arrested or in custody?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_12

#most do not like constables! But they do like officers. they are able to differentiate!!!
comparison = as.data.frame(prop.table(table(new2$castecode, new2$compareofficerconstable),1))
comparison


figure_13 <- ggplot(comparison, aes(Var1, Freq, fill=factor(Var2), group=factor(Var2))) + 
  geom_bar(stat="identity", colour = "grey", width = 0.5, position=position_dodge(0.7)) + 
  xlab("") + ylab("") + ggtitle("Who behaves better: police officers or constables?") + 
  labs(y="percent") + theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  scale_fill_brewer(palette = "Paired") + theme(axis.text.x = element_text(size = 15)) + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(plot.title = element_text(size = 15)) + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(text=element_text(size=15)) 
figure_13



#makes my project harder, but then the Indian states are spending so much money
#and so if there isn't a demand for these stations, and the SC/STs are not more likely to 
#have negative views about the police, why are these special police staitons being opened?
#Are they, as Rao has argued, a manifestation of a bureacratic legal machinery politicans
#have set up? Are there electoral considerations?
#Of course, these data are only from the INdian state of Rajasthan, and 
#it's entirely possible that hte communities of Bihar, UP would have different opinions. 


install.packages("cowplot")
library(cowplot)

plot_grid(figure_1, figure_2, labels=c("A", "B"), ncol = 2, nrow = 1)


plot_grid(figure_3, figure_4, figure_5, figure_6, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
plot_grid(figure_7, figure_8, labels=c("E", "F"), ncol = 2, nrow = 1)
plot_grid(figure_9, figure_10, figure_11, figure_12, figure_13, labels=c("G", "H", "I", "J"), ncol = 2, nrow = 2)


plot_grid(figure_1, figure_2)

one = plot_grid(figure_1, figure_2)
save_plot("one.pdf", one, ncol=2, nrow=1, base_aspect_ratio = 1.4)


plot_grid(figure_3, figure_4, figure_5)
plot_grid(figure_6, figure_7, figure_8)
plot_grid(figure_9, figure_10, figure_11, figure_12, figure_13, ncol=3)


