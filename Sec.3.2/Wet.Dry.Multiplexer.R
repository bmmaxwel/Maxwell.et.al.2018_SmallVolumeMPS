data=read.table(file="G:/MastersWork/Technology Paper/DataForPaper/Bioreactor/Bioreactor.NO3.Injection_ForPaper.csv",
                sep=",",header=TRUE,skip=1)
data$Date.Time=strptime(data$Date.Time, "%Y.%m.%d  %H:%M:%S") 
data$Date.Time=as.POSIXct(data$Date.Time)
data=data[c(1:1638),c(1,3,6,10)]
data=data[-(which(is.na(data$Well))),]
data$Well=as.factor(data$Well)
data$NO3=as.numeric(as.character(data$NO3))
data$DOC=as.numeric(as.character(data$DOC))

###Calculate range of NO3 concentrations at inlet over stable period
inflow=subset(data,data$Well==1)
inflow=inflow[c(46:length(inflow$NO3)),]

##Adjust times for each well to reflect simultaenous pumping
for (i in 1:length(data$NO3)){
  if (data$Well[i]==2){
    data$Date.Time[i]=data$Date.Time[i]-200
  } else if (data$Well[i]==3){
    data$Date.Time[i]=data$Date.Time[i]-400
  } else if (data$Well[i]==4){
    data$Date.Time[i]=data$Date.Time[i]-600
  } else if (data$Well[i]==5){
    data$Date.Time[i]=data$Date.Time[i]-800
  } else if (data$Well[i]==6){
    data$Date.Time[i]=data$Date.Time[i]-1000
  } else if (data$Well[i]==7){
    data$Date.Time[i]=data$Date.Time[i]-1200
  } else if (data$Well[i]==8){
    data$Date.Time[i]=data$Date.Time[i]-1400
  } else {
  }
}

##Use new variable, and introduce a counter for cycles
same=data
k=0
for (i in 1:dim(same)[1]) {
  if(same$Well[i]==1){     ##Increase cycle count every time Valve 1 is reached
    k=k+1
  }
  else {
  }
  same$Cycle[i]=k
}

##Give names to each well, to make it easier to understand
for (i in 1:length(same$NO3)){
  if (same$Well[i]==1){
    same$Name[i]="D.In.Mid"
  } else if (same$Well[i]==2){
    same$Name[i]="S.In.Mid"
  } else if (same$Well[i]==3){
    same$Name[i]="D.Out.Mid"
  } else if (same$Well[i]==4){
    same$Name[i]="S.Out.Mid"
  } else if (same$Well[i]==5){
    same$Name[i]="S.Out.Side"
  } else if (same$Well[i]==6){
    same$Name[i]="D.Out.Side"
  } else if (same$Well[i]==7){
    same$Name[i]="S.In.Side"
  } else if (same$Well[i]==8){
    same$Name[i]="D.In.Side"
  } else if (same$Well[i]==9){
    same$Name[i]="Outlet"
  } else {
    same$Name[i]="Inlet"
  }
}

##Set Depth
for (i in 1:length(same$NO3)){
  if (same$Well[i]==1|same$Well[i]==8|same$Well[i]==3|same$Well[i]==6){
    same$Depth[i]="Deep"
  } else if (same$Well[i]==2|same$Well[i]==4|same$Well[i]==5|same$Well[i]==7){
    same$Depth[i]="Shallow"
  } else if(same$Well[i]==9){
    same$Depth[i]="Outlet"
  }  else {
  same$Depth[i]="Inlet"
  }
}

##Set Position
for (i in 1:length(same$NO3)){
  if (same$Well[i]==1|same$Well[i]==2|same$Well[i]==3|same$Well[i]==4){
    same$Position[i]="Center"
  } else if (same$Well[i]==5|same$Well[i]==6|same$Well[i]==7|same$Well[i]==8){
    same$Position[i]="Side"
  } else if(same$Well[i]==9){
    same$Position[i]="Outlet"
  }  else {
    same$Position[i]="Inlet"
  }
}
##Set Length
for (i in 1:length(same$NO3)){
  if (same$Well[i]==1|same$Well[i]==2|same$Well[i]==7|same$Well[i]==8){
    same$Length[i]="In"
  } else if (same$Well[i]==5|same$Well[i]==6|same$Well[i]==3|same$Well[i]==4){
    same$Length[i]="Out"
  } else if(same$Well[i]==9){
    same$Length[i]="Outlet"
  }  else {
    same$Length[i]="Inlet"
  }
}
same$Depth=as.factor(same$Depth)
same$Depth=factor(same$Depth,levels(same$Depth)[c(2,3,4,1)])
same$Position=as.factor(same$Position)
same$Position=factor(same$Position,levels(same$Position)[c(2,3,1,4)])
same$Length=as.factor(same$Length)
same$Length=factor(same$Length,levels(same$Length)[c(2,4,1,3)])
same$Time=(same$Date.Time-same$Date.Time[1])/3600
#cbbPalette=c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

require(ggplot2)
tiff(filename=paste("G:/MastersWork/Technology Paper/SourceContTesting/","bio1.tiff",sep=""),width = 8, height = 7,
     units = "in", pointsize = 12,
     compression = c("none"),
     bg = "white", res = 600,
     restoreConsole = TRUE)
a=expression(paste("NO" ["3"], " concentration", " (mg NO" ["3"], "-N L" ^"-1",")",sep=""))
##Plot inlet wells
near.inlet=subset(same,same$Well==1|same$Well==2|same$Well==7|same$Well==8|same$Well==10)
ggplot(near.inlet,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Depth,shape=Position),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Well Depth")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(0, 22))+
  annotate("text",x=10,y=10,label=c("Injection Start"),size=4.5,angle=90)+
  annotate("text",x=50,y=22,label=c("Inlet Wells"),size=6)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size=20),
        plot.title = element_text(hjust = 0.5))+
  labs(x="Time (h)",y=a)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
##Plot outlet wells
tiff(filename=paste("G:/MastersWork/Technology Paper/SourceContTesting/","bio2.tiff",sep=""),width = 8, height = 7,
     units = "in", pointsize = 12,
     compression = c("none"),
     bg = "white", res = 600,
     restoreConsole = TRUE)
near.outlet=subset(same,same$Well==3|same$Well==4|same$Well==5|same$Well==6|same$Well==9)
ggplot(near.outlet,aes(x=Time,y=NO3))+
      geom_point(aes(colour=Depth,shape=Position),size=3,stroke=2)+
      theme_bw()+
      scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Well Depth")+
      #values=c("#000000","#FF0000","#CCCCCC")
      scale_shape_manual(values=c(17,1 , 16))+
      geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
      scale_y_continuous(limits = c(0, 22))+
      annotate("text",x=10,y=10,label=c("Injection Start"),size=4.5,angle=90)+
      annotate("text",x=50,y=22,label=c("Outlet Wells"),size=6)+
      theme(text = element_text(size=20),
            axis.text.x = element_text(size=20),
            plot.title = element_text(hjust = 0.5))+
      labs(x="Time (h)",y=a)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
##Plot side wells
near.side=subset(same,same$Well==5|same$Well==6|same$Well==7|same$Well==8|same$Well==10)
ggplot(near.side,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Depth,shape=Length),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Well Depth")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(0, 20.5))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=20.5,label=c("Side Wells"),size=6)
##Plot mid wells
near.side=subset(same,same$Well==1|same$Well==2|same$Well==3|same$Well==4|same$Well==10)
ggplot(near.side,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Depth,shape=Length),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Well Depth")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(12, 20.5))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=20.5,label=c("Center Wells"),size=6)
##Plot shallow  wells
near.side=subset(same,same$Well==5|same$Well==4|same$Well==7|same$Well==2|same$Well==10)
ggplot(near.side,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Position,shape=Length),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Position")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(0, 20.5))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=20.5,label=c("Shallow Wells"),size=6)
##Plot deep  wells
near.side=subset(same,same$Well==1|same$Well==3|same$Well==6|same$Well==8|same$Well==10)
ggplot(near.side,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Position,shape=Length),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Position")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(0, 20.5))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=20.5,label=c("Deep Wells"),size=6)
##Plot inlet/outlet
in.out=subset(same,same$Well==9|same$Well==10)
ggplot(in.out,aes(x=Time,y=NO3))+
  geom_point(aes(colour=Position,shape=Length),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Position")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(15, 20.5))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=20.5,label=c("Deep Wells"),size=6)
##Plot NO3 & DOC
ggplot(near.outlet,aes(x=Time,y=DOC))+
  geom_point(aes(colour=Depth,shape=Position),size=3,stroke=2)+
  theme_bw()+
  scale_color_manual(values=c("#D55E00","#009E73","#0072B2"),name="Well Depth")+
  #values=c("#000000","#FF0000","#CCCCCC")
  scale_shape_manual(values=c(17,1 , 16))+
  geom_vline(xintercept=14.16,size=1.4,linetype="dashed")+
  scale_y_continuous(limits = c(0, 12))+
  annotate("text",x=11,y=10,label=c("Injection Start"),angle=90)+
  annotate("text",x=50,y=11,label=c("Outlet Wells"),size=6)+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size=20),
        plot.title = element_text(hjust = 0.5))+
  labs(x="Time (hr)",y="DOC (ppm)",title="Figure 5")
  


 vipe=subset(same,Position="Inlet")
 period=subset(same,Time>37)
# period=subset(same,NO3>1)
d.in.side=subset(period,Name=="D.In.Side")
d.in.mid=subset(period,Name=="D.In.Mid")
d.in.mid=d.in.mid[c(1:90),]
  diff=d.in.mid$NO3-d.in.side$NO3
s.in.side=subset(period,Name=="S.In.Side")
s.in.mid=subset(period,Name=="S.In.Mid")
diff1=s.in.side$NO3-s.in.mid$NO3
d.out.side=subset(period,Name=="D.Out.Side")
d.out.mid=subset(period,Name=="D.Out.Mid")
out=subset(period,Name=="Outlet")
diff3=d.out.mid$NO3-d.out.side$NO3
s.out.side=subset(period,Name=="S.Out.Side")
s.out.mid=subset(period,Name=="S.Out.Mid")
diff4=s.out.side$NO3-s.in.side$NO3
t.test(d.in.side$NO3,s.in.side$NO3)
t.test(d.out.side$NO3,s.out.side$NO3)
t.test(d.in.mid$NO3,s.in.mid$NO3)
t.test(d.out.mid$NO3,s.out.mid$NO3)
max(out$NO3)
bartlett.test(d.in.side$NO3,s.in.side$NO3)
min(out$NO3)