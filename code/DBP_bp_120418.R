require(gamlss)
require(plyr)

setwd('H:/Wong_centiles/')

#load("DBP_centiles.RData")

load("centiles_dat.Rda")

data8 <- data6


male_nobp <- subset(data8,data8$gender=="M" & is.na(meds)==T)

female_nobp <- subset(data8,data8$gender=="F" & is.na(meds)==T)

male_bp <- subset(data8,data8$gender=="M" & meds==1)

female_bp <- subset(data8,data8$gender=="F" & meds==1)

myvars <- c("age", "DBP")
newdata <- male_nobp[myvars]
dat<-na.omit(newdata)
dat<-dat[(dat$age>=18 & dat$age<100),]

#DBP_m_s_ST31 <- lms(DBP, age, data=dat,trans.x=TRUE, families=c("ST3"), n.cyc=100)

DBP_m_n_FP1 <- gamlss(DBP ~ fp( age, 3), sigma.fo=~ fp( age, 3),family=ST3,data=dat,n.cyc=200)

mat1 <- centiles.pred(DBP_m_n_FP1, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat1t <- centiles.pred(DBP_m_n_FP1, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))


myvars <- c("age", "DBP")
newdata <- female_nobp[myvars]
dat<-na.omit(newdata)
dat<-dat[(dat$age>=18 & dat$age<100),]

#DBP_f_s_ST31 <- lms(DBP, age, data=dat,trans.x=TRUE, families=c("ST3"), n.cyc=100)

DBP_f_n_FP1 <- gamlss(DBP ~ fp( age, 3), sigma.fo=~ fp( age, 3),family=SEP3,data=dat,n.cyc=200)

mat2 <- centiles.pred(DBP_f_n_FP1, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat2t <- centiles.pred(DBP_f_n_FP1, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))


myvars <- c("age", "DBP")
newdata <- male_bp[myvars]
dat<-na.omit(newdata)
dat<-dat[(dat$age>=18 & dat$age<100),]

#DBP_m_m_ST31 <- lms(DBP, age, data=dat,trans.x=TRUE, families=c("ST3"), n.cyc=100)

DBP_m_b_FP1 <- gamlss(DBP ~ fp( age, 3), sigma.fo=~ fp( age, 3),family=SEP3,data=dat,n.cyc=200)

mat3 <- centiles.pred(DBP_m_b_FP1, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat3t <- centiles.pred(DBP_m_b_FP1, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))


myvars <- c("age", "DBP")
newdata <- female_bp[myvars]
dat<-na.omit(newdata)
dat<-dat[(dat$age>=18 & dat$age<100),]

#DBP_f_m_ST31 <- lms(DBP, age, data=dat,trans.x=TRUE, families=c("ST3"), n.cyc=100)

DBP_f_b_FP1 <- gamlss(DBP ~ fp( age, 3), sigma.fo=~ fp( age, 3),family=SEP3,data=dat,n.cyc=200)

mat4 <- centiles.pred(DBP_f_b_FP1, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat4t <- centiles.pred(DBP_f_b_FP1, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))



#Male no BP meds


g <- ggplot(mat1, aes(age))
g <- g + geom_ribbon(aes(ymin=C1, ymax=C5,x=age, fill="red"))
g <- g + geom_ribbon(aes(ymin=C5, ymax=C10,x=age, fill="darkorange2"))
g <- g + geom_ribbon(aes(ymin=C10, ymax=C25,x=age, fill="orange"))
g <- g + geom_ribbon(aes(ymin=C25, ymax=C75,x=age, fill="yellow"))
g <- g + geom_ribbon(aes(ymin=C75, ymax=C90,x=age, fill="orange"))
g <- g + geom_ribbon(aes(ymin=C90, ymax=C95,x=age, fill="darkorange2"))
g <- g + geom_ribbon(aes(ymin=C95, ymax=C99,x=age, fill="red"))

g <- g + scale_fill_manual(values=c("yellow"="yellow","orange"="orange","darkorange2"="darkorange2","red"="red"))

g <- g + geom_line(aes(y=C50), colour="black", size=1)

g <- g + theme_bw() + scale_x_continuous(breaks = seq(20,90,10), limits=c(20,90)) + 
  scale_y_continuous(breaks = c(40,60,80,100,120), limits=c(40,120)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Diastolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Male Without BP Medication")


#Female no BP meds

g1 <- ggplot(mat2, aes(age))
g1 <- g1 + geom_ribbon(aes(ymin=C1, ymax=C5,x=age, fill="red"))
g1 <- g1 + geom_ribbon(aes(ymin=C5, ymax=C10,x=age, fill="darkorange2"))
g1 <- g1 + geom_ribbon(aes(ymin=C10, ymax=C25,x=age, fill="orange"))
g1 <- g1 + geom_ribbon(aes(ymin=C25, ymax=C75,x=age, fill="yellow"))
g1 <- g1 + geom_ribbon(aes(ymin=C75, ymax=C90,x=age, fill="orange"))
g1 <- g1 + geom_ribbon(aes(ymin=C90, ymax=C95,x=age, fill="darkorange2"))
g1 <- g1 + geom_ribbon(aes(ymin=C95, ymax=C99,x=age, fill="red"))

g1 <- g1 + scale_fill_manual(values=c("yellow"="yellow","orange"="orange","darkorange2"="darkorange2","red"="red"))

g1 <- g1 + geom_line(aes(y=C50), colour="black", size=1)

g1 <- g1 + theme_bw() + scale_x_continuous(breaks = seq(20,90,10), limits=c(20,90)) + 
  scale_y_continuous(breaks = c(40,60,80,100,120), limits=c(40,120)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Diastolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Female Without BP Medication")


#Male BP meds

g2 <- ggplot(mat3, aes(age))
g2 <- g2 + geom_ribbon(aes(ymin=C1, ymax=C5,x=age, fill="red"))
g2 <- g2 + geom_ribbon(aes(ymin=C5, ymax=C10,x=age, fill="darkorange2"))
g2 <- g2 + geom_ribbon(aes(ymin=C10, ymax=C25,x=age, fill="orange"))
g2 <- g2 + geom_ribbon(aes(ymin=C25, ymax=C75,x=age, fill="yellow"))
g2 <- g2 + geom_ribbon(aes(ymin=C75, ymax=C90,x=age, fill="orange"))
g2 <- g2 + geom_ribbon(aes(ymin=C90, ymax=C95,x=age, fill="darkorange2"))
g2 <- g2 + geom_ribbon(aes(ymin=C95, ymax=C99,x=age, fill="red"))

g2 <- g2 + scale_fill_manual(values=c("yellow"="yellow","orange"="orange","darkorange2"="darkorange2","red"="red"))

g2 <- g2 + geom_line(aes(y=C50), colour="black", size=1)

g2 <- g2 + theme_bw() + scale_x_continuous(breaks = seq(20,90,10), limits=c(20,90)) + 
  scale_y_continuous(breaks = c(40,60,80,100,120), limits=c(40,120)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Diastolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Male With BP Medication")


#Female BP meds

g3 <- ggplot(mat4, aes(age))
g3 <- g3 + geom_ribbon(aes(ymin=C1, ymax=C5,x=age, fill="red"))
g3 <- g3 + geom_ribbon(aes(ymin=C5, ymax=C10,x=age, fill="darkorange2"))
g3 <- g3 + geom_ribbon(aes(ymin=C10, ymax=C25,x=age, fill="orange"))
g3 <- g3 + geom_ribbon(aes(ymin=C25, ymax=C75,x=age, fill="yellow"))
g3 <- g3 + geom_ribbon(aes(ymin=C75, ymax=C90,x=age, fill="orange"))
g3 <- g3 + geom_ribbon(aes(ymin=C90, ymax=C95,x=age, fill="darkorange2"))
g3 <- g3 + geom_ribbon(aes(ymin=C95, ymax=C99,x=age, fill="red"))

g3 <- g3 + scale_fill_manual(values=c("yellow"="yellow","orange"="orange","darkorange2"="darkorange2","red"="red"))

g3 <- g3 + geom_line(aes(y=C50), colour="black", size=1)

g3 <- g3 + theme_bw() + scale_x_continuous(breaks = seq(20,90,10), limits=c(20,90)) + 
  scale_y_continuous(breaks = c(40,60,80,100,120), limits=c(40,120)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Diastolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Female With BP Medication")



table1 <- mat1t

table1$cent_1 <- format(table1$C1, digits=4, nsmall=1)
table1$cent_5 <- format(table1$C5, digits=4, nsmall=1)
table1$cent_10 <- format(table1$C10, digits=4, nsmall=1)
table1$cent_25 <- format(table1$C25, digits=4, nsmall=1)
table1$cent_50 <- format(table1$C50, digits=4, nsmall=1)
table1$cent_75 <- format(table1$C75, digits=4, nsmall=1)
table1$cent_90 <- format(table1$C90, digits=4, nsmall=1)
table1$cent_95 <- format(table1$C95, digits=4, nsmall=1)
table1$cent_99 <- format(table1$C99, digits=4, nsmall=1)

table_paper1 <- table1[c(1,11,12,13,14,15,16,17,18,19)]
colnames(table_paper1) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")


table1 <- mat2t

table1$cent_1 <- format(table1$C1, digits=4, nsmall=1)
table1$cent_5 <- format(table1$C5, digits=4, nsmall=1)
table1$cent_10 <- format(table1$C10, digits=4, nsmall=1)
table1$cent_25 <- format(table1$C25, digits=4, nsmall=1)
table1$cent_50 <- format(table1$C50, digits=4, nsmall=1)
table1$cent_75 <- format(table1$C75, digits=4, nsmall=1)
table1$cent_90 <- format(table1$C90, digits=4, nsmall=1)
table1$cent_95 <- format(table1$C95, digits=4, nsmall=1)
table1$cent_99 <- format(table1$C99, digits=4, nsmall=1)

table_paper2 <- table1[c(1,11,12,13,14,15,16,17,18,19)]
colnames(table_paper2) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")


table1 <- mat3t

table1$cent_1 <- format(table1$C1, digits=4, nsmall=1)
table1$cent_5 <- format(table1$C5, digits=4, nsmall=1)
table1$cent_10 <- format(table1$C10, digits=4, nsmall=1)
table1$cent_25 <- format(table1$C25, digits=4, nsmall=1)
table1$cent_50 <- format(table1$C50, digits=4, nsmall=1)
table1$cent_75 <- format(table1$C75, digits=4, nsmall=1)
table1$cent_90 <- format(table1$C90, digits=4, nsmall=1)
table1$cent_95 <- format(table1$C95, digits=4, nsmall=1)
table1$cent_99 <- format(table1$C99, digits=4, nsmall=1)

table_paper3 <- table1[c(1,11,12,13,14,15,16,17,18,19)]
colnames(table_paper3) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")


table1 <- mat4t

table1$cent_1 <- format(table1$C1, digits=4, nsmall=1)
table1$cent_5 <- format(table1$C5, digits=4, nsmall=1)
table1$cent_10 <- format(table1$C10, digits=4, nsmall=1)
table1$cent_25 <- format(table1$C25, digits=4, nsmall=1)
table1$cent_50 <- format(table1$C50, digits=4, nsmall=1)
table1$cent_75 <- format(table1$C75, digits=4, nsmall=1)
table1$cent_90 <- format(table1$C90, digits=4, nsmall=1)
table1$cent_95 <- format(table1$C95, digits=4, nsmall=1)
table1$cent_99 <- format(table1$C99, digits=4, nsmall=1)

table_paper4 <- table1[c(1,11,12,13,14,15,16,17,18,19)]
colnames(table_paper4) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")






library(htmltools)
library(ReporteRs)
library(magrittr)
library(gridExtra)
library(cowplot)
library(ggplot2)

doc <- docx( )%>%
  addTitle("Male no BP meds") %>%
  addFlexTable(table_paper1 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  addPageBreak() %>%
  addTitle("Female no BP meds") %>%
  addFlexTable(table_paper2 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  addPageBreak() %>%
  addTitle("Male BP meds") %>%
  addFlexTable(table_paper3 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  addPageBreak() %>%
  addTitle("Female BP meds") %>%
  addFlexTable(table_paper4 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  addPageBreak() 

doc <-  addPlot( doc = doc, fun = print, x = g, 
                 vector.graphic = T,  
                 width = 6.5, height = 6.5)

doc <-  addPlot( doc = doc, fun = print, x = g1, 
                 vector.graphic = T,  
                 width = 6.5, height = 6.5)

doc <-  addPlot( doc = doc, fun = print, x = g2, 
                 vector.graphic = T,  
                 width = 6.5, height = 6.5)

doc <-  addPlot( doc = doc, fun = print, x = g3, 
                 vector.graphic = T,  
                 width = 6.5, height = 6.5)

doc <-  addPlot( doc = doc, fun = print, x = plot_grid(g, g1, g2, g3, align = "v", ncol= 2), 
                 vector.graphic = T,  
                 width = 9, height = 6.5)

writeDoc(doc, file = "DBP_bp_091018_100_FP.docx")



library(dplyr)


mround <- function(x,base){ 
  base*round(x/base) 
} 

dat$year<-mround(dat$age,2)

empirical1<-ddply(dat, "year", here(summarise), P = quantile(DBP, .01))
empirical5<-ddply(dat, "year", here(summarise), P = quantile(DBP, .05))
empirical10<-ddply(dat, "year", here(summarise), P = quantile(DBP, .1))
empirical25<-ddply(dat, "year", here(summarise), P = quantile(DBP, .25))
empirical50<-ddply(dat, "year", here(summarise), P = quantile(DBP, .5))
empirical75<-ddply(dat, "year", here(summarise), P = quantile(DBP, .75))
empirical90<-ddply(dat, "year", here(summarise), P = quantile(DBP, .9))
empirical95<-ddply(dat, "year", here(summarise), P = quantile(DBP, .95))
empirical99<-ddply(dat, "year", here(summarise), P = quantile(DBP, .99))

empirical <- rbind(empirical1,empirical5,empirical10,empirical25,empirical50,empirical75,empirical90,empirical95,empirical99)

empirical<-empirical[(empirical$year>=20 & empirical$year<=90),]

colnames(empirical) <- c("age","P")


g_empirical <- g3 + geom_point(data=empirical, mapping=aes(x=age, y=P), size=2, color="green")
g_empirical
