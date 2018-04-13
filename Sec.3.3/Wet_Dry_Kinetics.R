#Code to read all of the data from the Wet Dry Trials and Calculate the zero-order kinetics
#for each well (1-8) for each trial for each treatment (6-72 hours)

pathD<-"G:/PHDwork/Bioreactor/Wetting Drying Experiments/"
filename<-"KineticRateCalcs.csv"

data<-read.table(file=paste(pathD,filename,sep=""),sep=",",dec=".",header=TRUE)

trials = c("3.29.16","3.31.16","4.5.16","4.7.16","4.12.16","4.18.16","4.20.16","4.22.16",
           "4.25.16","4.27.16","4.29.16")
drain=c("24","24","24","24","24","12","12","12","12","12","12")
replicate=c("1","2","4","5","6","1","2","3","4","5","6")
well= c(1:8)

##Calculate the number of hours the bioreactor was saturated before the next experiment
#data$PriorStop<-strptime(as.character(data$PriorStop), "%Y%m/%d/%Y  %H:%M")
data$PriorStop<-as.POSIXct(data$PriorStop, format="%m/%d/%Y  %H:%M")
#data$PriorStop<-as.Date(data$PriorStop, format="%Y-%m-%d  %H:%M:%S")


#data$DrainTime<-strptime(as.character(data$DrainTime), "%Y-%m-%d  %H:%M:%S")
data$DrainTime<-as.POSIXct(data$DrainTime, format="%m/%d/%Y  %H:%M")
#data$DrainTime<-as.Date(data$DrainTime, format="%Y-%m-%d  %H:%M:%S")
data$HoursSat=difftime(data$DrainTime,data$PriorStop, units="h")


foxy = lapply(c(1:length(trials)), function(i)
{
     # i=1
      
      face=trials[i]
            trial=subset(data,data$Trial==face)
       haul=trial$HoursSat[1]
       haul=as.numeric(haul)
      answers = lapply(well, function(k)
      {
     # k = 1
            wellset= subset(trial,trial$Port==k)
            
           
            
            f3<-function(C0,rho){ ##set the zero order rate function
                  X<-C0-(rho)*t
                  return(X)}
            
            t0=wellset$Time[1]  #set the initial time stamp for this data series
            t=as.numeric((wellset$Time-t0)) #make an array where time is equal to the time elapsed from t0
            C=wellset$NO3 ##array of concentration values
            
            
            fit3=try ( nls(C ~ f3(C0,rho),
                           start = list(C0=max(C),rho = .1),
                           lower = list(C0=0,  rho = -1),
                           upper = list(C0=20,rho = 4),
                           algorithm="port"),TRUE)
            
            if('try-error' %in% class(fit3)) { 
                  co=c(rep("error",4))
                  rh=c(rep("error",4))
                  b3=data.frame(rbind(co,rh))
                  colnames(b3)=c("Estimate","Std. Error","t value","Pr(>|t|)")}else{
                        b3=as.data.frame(summary(fit3)$coefficients)
                  }
            b3$fit="Zero"
            b3$Well=k
            fake=b3
            return(fake)
      })
      maker=do.call(rbind,answers)
      maker$Trial=face
      lake=drain[i]
      maker$Drain=lake
      stake=replicate[i]
      maker$Replicate=stake
      maker$HoursSat=haul
      
      borrow=maker
      return(borrow)
})
final=do.call(rbind,foxy)
slip=c(1:(nrow(final)/2))
hip=c(2*slip)
master=final[hip,]
master=master[-c(3:5)]
head(master)

twentyfour=subset(master,master$Drain==24)
twelve=subset(master,master$Drain==12)

biovol=493.95
watervol=296.31
twentyfour$rate=twentyfour$Estimate*24*watervol/biovol
twelve$rate=twelve$Estimate*24*watervol/biovol

beanplot(twelve$Estimate,twentyfour$Estimate,names=c("12 h Drain","24 h Drain"),cex.axis=2, ylab="Removal (mg/L/hour)",
         xlab="Treatment",cex.lab=2)

##Plot the number of hours the bioreactor was saturated against removal rate for each well

par(mfrow=c(2,2))

for (i in well) {
      #i=1
      now=subset(twelve,twelve$Well==i)
      plot(now$HoursSat,now$Estimate,xlab="Hours Saturated",ylab="Zero-Order Rate (mg/L/hour)",cex=2,pch=16)
      reg<-lm(now$Estimate~now$Replicate)
      #abline(reg)
}



for (i in well) {
      #i=1
      now=subset(twelve,twentyfour$Well==i)
      plot(now$Replicate,now$Estimate,xlab="Replicate",ylab="Zero-Order Rate (mg/L/hour)",cex=2,pch=16)
      reg<-lm(now$Estimate~now$Replicate)
      #abline(reg)
}
