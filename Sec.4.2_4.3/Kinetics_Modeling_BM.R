
trial<-"Sep18"  ##Don't have to retype the date all the time
location<-"Downstream"  ##Don't have to retype the location all the time
pathD<-sprintf("G:/MastersWork/FieldStuff/%s/StreamTesting_%s_%s/",location,location,trial)
data = read.table(file=paste(pathD,sprintf("KineticsData_%s.csv",trial),sep=""),sep=",",header=TRUE)
data=subset(data,Mesocosm!="NA")##take out the na rows
data$Run=as.factor(data$Run) ##Make Runs column a factor (runs isn't actually a factor for my data, but the original code had this)
data$Mesocosm=as.factor(data$Mesocosm) ##Make mesocosms a factor
#data$Date.Time=strptime(data$Date.Time,"%Y-%m-%d %H:%M")
data$Date.Time=strptime(data$Date.Time,"%M/%d/%Y %H:%M") ##Convert dates to POSIXlt (date/time format)
 

me=subset(data,Mesocosm==12)
meso=c(8,9,10,11,12)
for (i in meso){ ##loop to make a smaller data set from total data point set
      subdata<-subset(data,Mesocosm==i)
      RandomSamp<-sample(1:nrow(subset(data,Mesocosm==i)),size=5)
      a=subdata[RandomSamp,]
      sort=order(a$X,decreasing=F)
      fg=a[sort,]
      assign(paste("Set",i,sep=""),fg)
}


d=as.vector(data$Mesocosm) ##vector for mesocosms column
ids=levels(as.factor(d))
n.ids=NROW(ids)
runs=levels(data$Run)
n.runs=NROW(runs)

nls.data=lapply(c(1:n.ids), function(id)
{
     # id=1
      df.id=subset(data,Mesocosm==ids[id])
      
      nls.data.runs=lapply(c(1:n.runs),function(r)
      {
           # r=1   
            f1<-function(C0,rho){  ##set the first order rate function
                  X<-C0*exp(-rho*t/depth)
                  return(X)}
            
            
            f2<-function(rho,alpha,C0){  ##set the efficiency loss function
                  X<-rho/depth*(alpha-1)*t + C0^(1-alpha)
                  X<-X^(1/(1-alpha))  
                  return(X)}
            
            f3<-function(C0,rho){ ##set the zero order rate function
                  X<-C0-(rho/depth)*t
                  return(X)}
            
            
            df.run=subset(df.id,Run==runs[r])
            t0=df.run$Date.Time[1]  #set the initial time stamp for this data series
            t=as.numeric((df.run$Date.Time-t0)/(3600*24)) #make an array where time is equal to the time elapsed from t0
            C=df.run[,"NO3"] ##array of concentration values
            depth=df.run[,"Depth"]/100 ##array of depth values for each mesocosm
            
            fit1=try ( nls(C ~ f1(C0,rho),
                           start = list(C0=max(C),rho = 0.1),
                           lower = list(C0=0,  rho = 0),
                           upper = list(C0=Inf,rho = Inf),
                           algorithm="port")
                       ,TRUE)
            if('try-error' %in% class(fit1)) {
                  co=c(rep("error",4))
                  rh=c(rep("error",4))
                  b1=data.frame(rbind(co,rh))
                  colnames(b1)=c("Estimate","Std. Error","t value","Pr(>|t|)")
            }else{
                  b1=as.data.frame(summary(fit1)$coefficients)
            }
            
            b1$fit="First"
            
            fit2=try ( nls(C ~ f2(rho,alpha,C0),
                           start = list(C0=max(C),rho = .01, alpha=.00001),
                           lower = list(C0=0,  rho = 0,  alpha=0),
                           upper = list(C0=10,rho = 2,alpha=.001),
                           algorithm="port"
            ),TRUE)
            
            if('try-error' %in% class(fit2)) {
                  co=c(rep("error",4))
                  rh=c(rep("error",4))
                  alph=c(rep("error",4))
                  b2=data.frame(rbind(rh,alph,co))
                  colnames(b2)=c("Estimate","Std. Error","t value","Pr(>|t|)")
            }else{
                  b2=as.data.frame(summary(fit2)$coefficients)
            }
            b2$fit="Second"
            
            fit3=try ( nls(C ~ f3(C0,rho),
                           start = list(C0=max(C),rho = .1),
                           lower = list(C0=0,  rho = -1),
                           upper = list(C0=10,rho = 4),
                           algorithm="port"),TRUE)
            
            if('try-error' %in% class(fit3)) {
                  co=c(rep("error",4))
                  rh=c(rep("error",4))
                  b3=data.frame(rbind(co,rh))
                  colnames(b3)=c("Estimate","Std. Error","t value","Pr(>|t|)")}else{
                        b3=as.data.frame(summary(fit3)$coefficients)
                  }
            b3$fit="Zero"
            
            fits=rbind(b1,b2,b3)
            fits$ID=ids[id]
            fits$Run=r
            rownames(fits)=c("C0","rho","C0_2","rho_2","alpha_2","C0_3","rho_3")
            
            return(fits)
      })
      
      nls.data.runs=do.call(rbind,nls.data.runs)
      return(nls.data.runs)
})

nls.data=do.call(rbind,nls.data)

colnames(nls.data)=c("Estimate","Std. Error", "t value", "Pr(>|t|)","Fit","Mesocosm","Run")

write.table(nls.data, file=paste(pathD,sprintf("KineticsResults_%s.csv",trial),sep=""),
            quote=FALSE,sep=",",row.names=TRUE,col.names=TRUE)

##To plot full kinetics data, with all values, functions, and lab values included
labdata = read.table(file=paste(pathD,sprintf("LabValuesOnly_%s.csv",trial),sep=""),sep=",",header=TRUE)
labdata$Date.Time=strptime(labdata$Date.Time,"%Y-%m-%d  %H:%M")
t0=you$Date.Time[1]
#t.0=difftime(lab$Date.Time,t0,units="hours")
see=layout(matrix(1:6,3,2))
layout.show(6)
meso=c(8,9,10,11,12)
col=c("red","blue","black","black","brown")
par(mar=c(5,5.5,2,.6))
sky=matrix(NA,5,7)
for (i in meso){
      #i=11
      hey=subset(nls.data,Mesocosm==i)
      you=subset(data,Mesocosm==i)
      lab=subset(labdata,Mesocosm==i)
      graph.time=as.POSIXlt(you$Date.Time)
      
      graphdate.time=c(graph.time[1],graph.time[1]+3600*6,graph.time[1]+3600*12,graph.time[1]+3600*18,graph.time[1]+3600*24)
      graphdate.time=format(graphdate.time,"%H:%M")
      graphhours=c(0,.25,0.5,0.75,1)
      t0=you$Date.Time[1]
      t.0=difftime(lab$Date.Time,t0,units="days")
      depth=you$Depth[1]/100
      if(hey[1,1]!='error'){C0=as.numeric(hey[1,1])}else{C0=0}
      if(hey[2,1]!='error'){rho=as.numeric(hey[2,1])}else{rho=0}
      if(hey[3,1]!='error'){C0_2=as.numeric(hey[3,1])}else{C0_2=0}
      if(hey[4,1]!='error'){rho_2=as.numeric(hey[4,1])}else{rho_2=0}
      if(hey[5,1]!='error'){alpha=as.numeric(hey[5,1])}else{alpha=0}
      if(hey[6,1]!='error'){C0_3=as.numeric(hey[6,1])}else{C0_3=0}
      if(hey[7,1]!='error'){rho_3=as.numeric(hey[7,1])}else{rho_3=0}
      x=t
      m=as.numeric(difftime(you$Date.Time,you$Date.Time[1],units="days"))
      zero=C0_3-(rho_3/depth)*m
      first=C0*exp(-rho*m/depth)
      second=(rho_2/depth*(alpha-1)*m + C0_2^(1-alpha))^(1/(1-alpha))
      #ylab="NO3 Conc."
      #xlab="Time"
      #xlim=c(0,max(m)+.05)
      #ylim=c(min(data$NO3)-.2,max(data$NO3)+1)
      #cex=1.8
     # lwd=2
      #text=1.8
      #plot(t.0,lab$NO3,pch=18,ylim=ylim,xlim=xlim,cex=cex,col=col[5],ylab=ylab,xlab=xlab,
      #     main=sprintf("Mesocosm %s",i),xaxt="n",cex.axis=cex,cex.lab=cex,cex.main=cex)
    #  par(new=TRUE)
    #  plot(m,first,"l",ylim=ylim,xlim=xlim,col=col[1],ylab="",xlab="",xaxt="n",yaxt="n",lwd=lwd)
    #  par(new=TRUE)
     # plot(m,second,"l",ylim=ylim,xlim=xlim,col=col[2],ylab="",xlab="",xaxt="n",yaxt="n",lwd=lwd)
     # par(new=TRUE)
     # plot(m,you$NO3,ylim=ylim,xlim=xlim,col=col[3],ylab="",xlab="",xaxt="n",yaxt="n",lwd=lwd,cex=1.4)
     # par(new=TRUE)
     # plot(m,zero,"l",ylim=ylim,xlim=xlim,col=col[4],ylab="",xlab="",xaxt="n",yaxt="n",lwd=lwd)
     # axis(side=1,at=graphhours,labels=graphdate.time,cex.axis=cex)
      
      if(is.finite(rho)){firstreg=lm(first~you$NO3)}else{firstreg=0}
      if(is.finite(rho_2)){secondreg=lm(second~you$NO3)}else{secondreg=0} 
      if(is.finite(rho_3)){zeroreg=lm(zero~you$NO3)}else{zeroreg=0}
      
      if(is.finite(rho_3)){zero.rsq=round(summary(zeroreg)$adj.r.squared,3)} else{zero.rsq=0}
      if(is.finite(rho)){first.rsq=round(summary(firstreg)$adj.r.squared,3)} else{first.rsq=0}
      if(is.finite(rho_2)){second.rsq=round(summary(secondreg)$adj.r.squared,3)} else{second.rsq=0}
      cnaught=expression(rho)
      
      firstrmse=sqrt(mean((firstreg$residuals)^2))
      zerormse=sqrt(mean((zeroreg$residuals)^2))
      #text(.45,ylim-.15,sprintf("1st Order =  %s", first.rsq),col="red",cex= text) 
     # text(.85,ylim-.15,sprintf("p=%.3f",rho),col="red",cex= text)
     # text(.45,ylim-.55,sprintf("Eff. Loss= %s",second.rsq),col="blue",cex= text)
     # text(.85,ylim-.55,sprintf("p= %.3f  a=%.3f",rho_2,alpha),col="blue",cex= text)
#text(.45,ylim-.95,sprintf("Zero Order= %s",zero.rsq),col="black",cex= text)
     # text(.85,ylim-.95,sprintf("p= %.3f",rho_3),col="black",cex= text)
      k=i-7
      sky[k,1]=rho_3
      sky[k,2]=zero.rsq
      sky[k,3]=zerormse
      sky[k,4]=rho
      sky[k,5]=first.rsq
      sky[k,6]=firstrmse
      sky[k,7]=trial
}
ace=as.data.frame(sky)
ace
plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
text(x = 0.5, y = 0.85, sprintf("Mesocosm Trial at %s \n on %s",location,trial),cex=2.2)
legend("bottomleft",c('First Order Model','Eff. Loss Model','Zero Order Model','PLSR-Corrected NO3','Lab Samples'),
       lty=c(1,1,1,NA,NA),pch=c(NA,NA,NA,1,18),bg='white',col=c('red','blue','black','black','brown'),ncol=2,
       x.intersp=.2,xjust=1,y.intersp=.5,text.width=.2,cex=1.5,pt.cex=c(1.2,1.2,1.2,2,2),lwd=lwd)

##Plot the residuals plots
see=layout(matrix(1:8,4,2))
layout.show(8)
meso=c(9,10,11,12)
col=c("red","blue","black","black","brown")
par(mar=c(5,5.5,2,.6))
for (i in meso){
      #i=10
      hey=subset(nls.data,Mesocosm==i)
      you=subset(data,Mesocosm==i)
      lab=subset(labdata,Mesocosm==i)
      graph.time=as.POSIXlt(you$Date.Time)
      
      graphdate.time=c(graph.time[1],graph.time[1]+3600*6,graph.time[1]+3600*12,graph.time[1]+3600*18,graph.time[1]+3600*24)
      graphdate.time=format(graphdate.time,"%H:%M")
      graphhours=c(0,.25,0.5,0.75,1)
      t0=you$Date.Time[1]
      t.0=difftime(lab$Date.Time,t0,units="days")
      depth=you$Depth[1]/100
      if(hey[1,1]!='error'){C0=as.numeric(hey[1,1])}else{C0=0}
      if(hey[2,1]!='error'){rho=as.numeric(hey[2,1])}else{rho=0}
      if(hey[3,1]!='error'){C0_2=as.numeric(hey[3,1])}else{C0_2=0}
      if(hey[4,1]!='error'){rho_2=as.numeric(hey[4,1])}else{rho_2=0}
      if(hey[5,1]!='error'){alpha=as.numeric(hey[5,1])}else{alpha=0}
      if(hey[6,1]!='error'){C0_3=as.numeric(hey[6,1])}else{C0_3=0}
      if(hey[7,1]!='error'){rho_3=as.numeric(hey[7,1])}else{rho_3=0}
      x=t
      m=as.numeric(difftime(you$Date.Time,you$Date.Time[1],units="days"))
      zero=C0_3-(rho_3/depth)*m
      first=C0*exp(-rho*m/depth)
      second=(rho_2/depth*(alpha-1)*m + C0_2^(1-alpha))^(1/(1-alpha))
      ylab="NO3 Conc."
      xlab="Time"
      xlim=c(0,max(m)+.05)
      ylim=c(min(data$NO3)-.2,max(data$NO3)+1)
      cex=1.8
      lwd=2
      text=1.8
      
      if(is.finite(rho)){firstreg=lm(you$NO3~first)}else{firstreg=0}
      if(is.finite(rho_2)){secondreg=lm(you$NO3~second)}else{secondreg=0} 
      if(is.finite(rho_3)){zeroreg=lm(you$NO3~zero)}else{zeroreg=0}
      f=you$NO3-first
      z=you$NO3-zero
      firstresid=resid(firstreg)
      zeroresid=resid(zeroreg)
      plot(m,firstresid,ylim=c(min(f)-.1,max(f)+.1),type="p",main=sprintf("Residuals for %s Mesocosm %s, First Order",trial,i),ylab="Observed-Predicted",
           cex.axis=1.8,cex.lab=1.8,cex=2,cex.main=1.8)
      abline(a=0,b=0)
      qqnorm(firstresid,cex.axis=1.8,cex.lab=1.8,cex=2,cex.main=1.8); qqline(firstresid,col="black")
      plot(m,zeroresid,ylim=c(min(z)-.1,max(z)+.1),type="p",main=sprintf("Residuals for %s Mesocosm %s, Zero Order",trial,i),ylab="Observed-Predicted",
           cex.axis=1.8,cex.lab=1.8,cex=2,cex.main=1.8)
      abline(a=0,b=0)
      qqnorm(zeroresid,cex.axis=1.8,cex.lab=1.8,cex=2,cex.main=1.8); qqline(zeroresid,col="black")
      
}
