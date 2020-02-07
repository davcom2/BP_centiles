require(gamlss)
require(plyr)
require(ggplot2)

setwd('H:/Wong_centiles/')

#load("SBP_centiles.RData")

load("centiles_dat.Rda")

male <- subset(data6,data6$gender=="M")

female <- subset(data6,data6$gender=="F")



myvars <- c("age", "SBP")
newdata <- male[myvars]

dat<-na.omit(newdata)

dat<-dat[(dat$age>=18 & dat$age<100),]

SBP_BCTo1 <- lms(SBP, age, data=dat,trans.x=TRUE, families=c("BCTo"), n.cyc=100)

mat1 <- centiles.pred(SBP_BCTo1, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat1t <- centiles.pred(SBP_BCTo1, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))

mround <- function(x,base){ 
  base*round(x/base) 
} 

dat$year<-mround(dat$age,2)

empirical1<-ddply(dat, "year", here(summarise), P = quantile(SBP, .01))
empirical5<-ddply(dat, "year", here(summarise), P = quantile(SBP, .05))
empirical10<-ddply(dat, "year", here(summarise), P = quantile(SBP, .10))
empirical25<-ddply(dat, "year", here(summarise), P = quantile(SBP, .25))
empirical50<-ddply(dat, "year", here(summarise), P = quantile(SBP, .50))
empirical75<-ddply(dat, "year", here(summarise), P = quantile(SBP, .75))
empirical90<-ddply(dat, "year", here(summarise), P = quantile(SBP, .90))
empirical95<-ddply(dat, "year", here(summarise), P = quantile(SBP, .95))
empirical99<-ddply(dat, "year", here(summarise), P = quantile(SBP, .99))

empirical <- rbind(empirical1,empirical5,empirical10,empirical25,empirical50,empirical75,empirical90,empirical95,empirical99)

empirical<-empirical[(empirical$year>=20 & empirical$year<=90),]

colnames(empirical) <- c("age","P")

empirical_male <- empirical



myvars <- c("age", "SBP")
newdata <- female[myvars]

dat<-na.omit(newdata)

dat<-dat[(dat$age>=18 & dat$age<100),]


SBP_BCTo2 <- lms(SBP, age, data=dat,trans.x=TRUE, families=c("BCTo"), n.cyc=100)

mat2 <- centiles.pred(SBP_BCTo2, xname="age",xvalues=seq(18,90,0.1), plot=TRUE, cent=seq(1,99,1), legend=F, xlab="Age (years)", ylab="Systolic Blood Pressure (mmHg)" )
mat2t <- centiles.pred(SBP_BCTo2, xname="age",xvalues=seq(20,90,2), plot=F, cent=c(1,5,10,25,50,75,90,95,99))

mround <- function(x,base){ 
  base*round(x/base) 
} 

dat$year<-mround(dat$age,2)

empirical1<-ddply(dat, "year", here(summarise), P = quantile(SBP, .01))
empirical5<-ddply(dat, "year", here(summarise), P = quantile(SBP, .05))
empirical10<-ddply(dat, "year", here(summarise), P = quantile(SBP, .10))
empirical25<-ddply(dat, "year", here(summarise), P = quantile(SBP, .25))
empirical50<-ddply(dat, "year", here(summarise), P = quantile(SBP, .50))
empirical75<-ddply(dat, "year", here(summarise), P = quantile(SBP, .75))
empirical90<-ddply(dat, "year", here(summarise), P = quantile(SBP, .90))
empirical95<-ddply(dat, "year", here(summarise), P = quantile(SBP, .95))
empirical99<-ddply(dat, "year", here(summarise), P = quantile(SBP, .99))

empirical <- rbind(empirical1,empirical5,empirical10,empirical25,empirical50,empirical75,empirical90,empirical95,empirical99)

empirical<-empirical[(empirical$year>=20 & empirical$year<=90),]

colnames(empirical) <- c("age","P")

empirical_female <- empirical





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
  scale_y_continuous(breaks = c(80,100,120,140,160,180,200), limits=c(80,210)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Systolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Male")


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
  scale_y_continuous(breaks = c(80,100,120,140,160,180,200), limits=c(80,210)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position="none") + ylab("Systolic Blood Pressure (mmHg)") + xlab("Age (years)") + ggtitle("Female")


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



library(htmltools)
library(ReporteRs)
library(magrittr)
library(gridExtra)
library(cowplot)

doc <- docx( )%>%
  addTitle("Male") %>%
  addFlexTable(table_paper1 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  addPageBreak() %>%
  addTitle("Female") %>%
  addFlexTable(table_paper2 %>%
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

doc <-  addPlot( doc = doc, fun = print, x = plot_grid(g, g1, align = "v", ncol= 2), 
                 vector.graphic = T,  
                 width = 9, height = 6.5)

writeDoc(doc, file = "SBP_main_091018_100.docx")


g.e <- g + geom_point(data=empirical_male, mapping=aes(x=age, y=P), size=2, color="green")
g.e


g1.e <- g1 + geom_point(data=empirical_female, mapping=aes(x=age, y=P), size=2, color="green")
g1.e


sbp_m <- g
sbp_f <- g1
sbp_m.e <- g.e
sbp_f.e <- g1.e

save(sbp_m,sbp_f,sbp_m.e,sbp_f.e,file="C:/Users/sgerry/Dropbox/Steve work/VM/Precursor/SBP_plots.RData")