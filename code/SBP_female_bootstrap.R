#Save plots, tables, etc. in a list. Return them with stnadrad names in the list. Give them unique names when calling the function.



require(readstata13)
require(gamlss)
require(plyr)
library(doSNOW)
library(doParallel)
library(doRNG)
require(rms)
require(compiler)
require(ipred)
require(parallel)
require(plyr)
require(data.table)
require(mfp)
require(rms)
require(survival)
require(reshape)
require(ggplot2)
require(boot)
require(survAUC)
require(ggplot2)
require(MASS)
require(mice)
require(VIM)
require(reshape2)
require(corrplot)
require(Hmisc)
require(glmnet)
require(ReporteRs)

setwd('H:/Wong_centiles/')

load("centiles_dat.Rda")

male <- subset(data6,data6$gender=="M")

female <- subset(data6,data6$gender=="F")



myvars <- c("age", "SBP")
newdata <- female[myvars]

dat<-na.omit(newdata)

dat<-dat[(dat$age>=18 & dat$age<100),]

SBP_BCTo1 <- lms(SBP, age, data=dat,trans.x=TRUE, families=c("BCTo"), n.cyc=100)

boot_model_final <- SBP_BCTo1


#dataforfunction <- head(dat,10000)

dataforfunction <- dat



bootstapping <- function(data, model) {
  
  dataforfunction <- data


cluster <- makeCluster(4)

BOOT_CI <- function(){
  
  ind <- sample(nrow(dataforfunction), size=nrow(dataforfunction), replace=TRUE)
  data_samp <- dataforfunction[ind,]
  
  boot_model <- model(data_samp)
  
  newx<-seq(20,90,2)
  mat <- centiles.pred(boot_model, xname="age", xvalues=newx, cent=c(1,5,10,25,50,75,90,95,99) )
  id <- i
  mat <- cbind(id=id, mat)
  
  w <- reshape(mat, 
               timevar = "age",
               idvar = c("id"),
               direction = "wide")
  
  return(w)
}


registerDoParallel(cluster)
system.time(OUT1 <- foreach(i = 1:50, .errorhandling = "remove", .packages = c('gamlss', 'boot', 'survival', 'foreach', 'doSNOW', 'parallel'), .combine = 'rbind', .options.RNG = 1247612) %dorng% { BOOT_CI() })
stopCluster(cluster)

attr(OUT1, "rng") <- NULL
rownames(OUT1)    <- NULL
OUT1              <- data.frame(OUT1)



bootdata <- reshape(OUT1, 
                    timevar = "age",
                    idvar = c("id"),
                    direction = "long",
                    varying=2:325)

stdErr <- function(x) {sd(x)/ sqrt(length(x))}
collapse<-ddply(bootdata, .(age), colwise(sd))


newx<-seq(20,90,2)
mat <- centiles.pred(boot_model_final, xname="age",xvalues=newx, plot=F, cent=c(1,5,10,25,50,75,90,95,99))


CI.up.1 <- as.numeric(mat$C1)+as.numeric(collapse$C1)*1.96
CI.down.1 <- as.numeric(mat$C1)-as.numeric(collapse$C1)*1.96
CI.up.5 <- as.numeric(mat$C5)+as.numeric(collapse$C5)*1.96
CI.down.5 <- as.numeric(mat$C5)-as.numeric(collapse$C5)*1.96
CI.up.10 <- as.numeric(mat$C10)+as.numeric(collapse$C10)*1.96
CI.down.10 <- as.numeric(mat$C10)-as.numeric(collapse$C10)*1.96
CI.up.25 <- as.numeric(mat$C25)+as.numeric(collapse$C25)*1.96
CI.down.25 <- as.numeric(mat$C25)-as.numeric(collapse$C25)*1.96
CI.up.50 <- as.numeric(mat$C50)+as.numeric(collapse$C50)*1.96
CI.down.50 <- as.numeric(mat$C50)-as.numeric(collapse$C50)*1.96
CI.up.75 <- as.numeric(mat$C75)+as.numeric(collapse$C75)*1.96
CI.down.75 <- as.numeric(mat$C75)-as.numeric(collapse$C75)*1.96
CI.up.90 <- as.numeric(mat$C90)+as.numeric(collapse$C90)*1.96
CI.down.90 <- as.numeric(mat$C90)-as.numeric(collapse$C90)*1.96
CI.up.95 <- as.numeric(mat$C95)+as.numeric(collapse$C95)*1.96
CI.down.95 <- as.numeric(mat$C95)-as.numeric(collapse$C95)*1.96
CI.up.99 <- as.numeric(mat$C99)+as.numeric(collapse$C99)*1.96
CI.down.99 <- as.numeric(mat$C99)-as.numeric(collapse$C99)*1.96

C <- cbind(CI.up.1,CI.down.1,CI.up.5,CI.down.5,CI.up.10,CI.down.10,CI.up.25,CI.down.25,CI.up.50,CI.down.50,CI.up.75,CI.down.75,CI.up.90,CI.down.90,CI.up.95,CI.down.95,CI.up.99,CI.down.99)



S1<-qBCTo(0.01,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S5<-qBCTo(0.05,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S10<-qBCTo(0.1,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S25<-qBCTo(0.25,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S50<-qBCTo(0.50,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S75<-qBCTo(0.75,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S90<-qBCTo(0.90,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S95<-qBCTo(0.95,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))
S99<-qBCTo(0.99,fitted(boot_model_final,"mu"),fitted(boot_model_final,"sigma"),fitted(boot_model_final,"nu"),fitted(boot_model_final,"tau"))


S <- as.data.frame(cbind(dat$age, S1, S5, S10, S25, S50, S75, S90, S95, S99))

S <- S[order(S$V1),]



table <- cbind(mat, CI.down.1,CI.up.1,CI.down.5,CI.up.5,CI.down.10,CI.up.10,CI.down.25,CI.up.25,CI.down.50,CI.up.50,
               CI.down.75,CI.up.75,CI.down.90,CI.up.90,CI.down.95,CI.up.95,CI.down.99,CI.up.99)

table$a <- " ("
table$b <- ", "
table$c <- ")"

table$t1 <- do.call(paste, c(list(format(table$C1, digits=4, nsmall=1), table$a, format(table$CI.down.1, digits=4, nsmall=1), table$b, format(table$CI.up.1, digits=4, nsmall=1), table$c), sep=""))
table$t5 <- do.call(paste, c(list(format(table$C5, digits=4, nsmall=1), table$a, format(table$CI.down.5, digits=4, nsmall=1), table$b, format(table$CI.up.5, digits=4, nsmall=1), table$c), sep=""))
table$t10 <- do.call(paste, c(list(format(table$C10, digits=4, nsmall=1), table$a, format(table$CI.down.10, digits=4, nsmall=1), table$b, format(table$CI.up.10, digits=4, nsmall=1), table$c), sep=""))
table$t25 <- do.call(paste, c(list(format(table$C25, digits=4, nsmall=1), table$a, format(table$CI.down.25, digits=4, nsmall=1), table$b, format(table$CI.up.25, digits=4, nsmall=1), table$c), sep=""))
table$t50 <- do.call(paste, c(list(format(table$C50, digits=4, nsmall=1), table$a, format(table$CI.down.50, digits=4, nsmall=1), table$b, format(table$CI.up.50, digits=4, nsmall=1), table$c), sep=""))
table$t75 <- do.call(paste, c(list(format(table$C75, digits=4, nsmall=1), table$a, format(table$CI.down.75, digits=4, nsmall=1), table$b, format(table$CI.up.75, digits=4, nsmall=1), table$c), sep=""))
table$t90 <- do.call(paste, c(list(format(table$C90, digits=4, nsmall=1), table$a, format(table$CI.down.90, digits=4, nsmall=1), table$b, format(table$CI.up.90, digits=4, nsmall=1), table$c), sep=""))
table$t95 <- do.call(paste, c(list(format(table$C95, digits=4, nsmall=1), table$a, format(table$CI.down.95, digits=4, nsmall=1), table$b, format(table$CI.up.95, digits=4, nsmall=1), table$c), sep=""))
table$t99 <- do.call(paste, c(list(format(table$C99, digits=4, nsmall=1), table$a, format(table$CI.down.99, digits=4, nsmall=1), table$b, format(table$CI.up.99, digits=4, nsmall=1), table$c), sep=""))

table_paper <- table[c(1,32,33,34,35,36,37,38,39,40)]
colnames(table_paper) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")


return(list(S,table,table_paper))


}




lmsdfj <- function(data_samp) {lms(SBP, age, data=data_samp, trans.x=TRUE, families=c("BCTo"), n.cyc=250)}


OUTPUT <- bootstapping(dataforfunction, lmsdfj)


table <- as.data.frame(OUTPUT[3])


colnames(table) <- c("Age","_1st","_5th","_10th","_25th","_50th","_75th","_90th","_95th","_99th")



library(htmltools)
library(ReporteRs)
library(magrittr)
library(gridExtra)
library(cowplot)

doc <- docx( )%>%
  addTitle("Female SBP centiles with 95% CI") %>%
  addFlexTable(table %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
 
writeDoc(doc, file = "SBP_female_ci_091018.docx")

sbp_f_table <- table
save(sbp_f_table,file="C:/Users/sgerry/Dropbox/Steve work/VM/Precursor/SBP_f_table.RData")


