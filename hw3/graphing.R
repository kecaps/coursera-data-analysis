setwd("~/classes/dataAnalysis/hw3")
outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses="character")
head(outcome)
str(outcome)
nrow(outcome)
ncol(outcome)

opar = par()

outcome[,11] <- as.numeric(outcome[,11])
par(las=0)
hist(outcome[,11],
     main="Heart Attack 30-day Death Rate",
      xlab="30-day Death Rate")


par(opar)
idx = c(11,17,23)
names(idx) = c("Heart Attack", "Heart Failure", "Pneumonia")


for (i in seq_along(idx)) {
  outcome[,idx[i]] <- as.numeric(outcome[,idx[i]])
}
par(mfrow=c(3,1))
xlim=range(outcome[,idx],na.rm=T)
for (i in seq_along(idx)) {
  rate <- outcome[,idx[i]]
  hist(rate,xlim=xlim, ylim=c(0,1000), main=as.expression(substitute(paste(n," ", (bar(X)==m)), list(n=names[i],m=mean(rate,na.rm=T)))),xlab="30-day Death Rate")
  abline(v=median(rate, na.rm=TRUE),col='red')
}


hist(rate,xlim=xlim, ylim=c(0,1000), main=substitute(paste("Heart Attack", " ", (bar(X) == 15.4485294117647))),xlab="30-day Death Rate")
expression(paste("Heart Attack", (bar(X) == 15.4485294117647)))



outcome$State 
st.table = table(outcome$State)
st.filter = rownames(st.table)[st.table >= 20]
outcome2 <- outcome[outcome$State %in% st.filter,]
dim(outcome2)
par(mfrow=c(2,1))
death <- outcome2[,11]
state <- outcome2$State
par(las=2)
boxplot(death ~ state)
state_death.l = split(death, state)
state_sorted.v = names(sort(sapply(state_death.l,median, na.rm=T)))
state_death.l <- state_death.l[state_sorted.v]
boxplot(state_death.l, xaxt='n')
xlabels = sprintf("%s (%d)", names(state_death.l), sapply(state_death.l, length)) 
axis(1,seq_along(state_sorted.v), labels=xlabels, cex.axis=0.7)
title(main="Heart Attack 30-day Death Rate by State", ylab="30-day Death Rate")


hospitals <- read.csv("data/hospital-data.csv", colClasses="character")
outcome.hospital <- merge(outcome, hospitals, by="Provider.Number")

death <- as.numeric(outcome.hospital[,11])
npatient <- as.numeric(outcome.hospital[,15])
owner <- factor(outcome.hospital$Hospital.Ownership)
library("lattice")
xyplot(death ~ npatient | owner, 
       xlab="Number of Patients Seen",
       ylab="30-day Death Rate",
       main="Heart Attack 30-day Death Rate by Ownership",
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.lmline(x,y)
       }
)