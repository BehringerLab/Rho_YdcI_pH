library(dplyr)
library(ggplot2)
library(cowplot)
library(data.table)
library(pals)

####Run1
ScopeData<-read.table("allCellsList.csv_Run1", sep=",",header=TRUE)
names(ScopeData)

ScopeMean<-ScopeData %>% group_by(Condition) %>% summarise(ratio.mean=median(ratio_1_over_2,na.rm=TRUE))
ScopeMean.tb<-as.data.frame(ScopeMean)
ScopeMean.tb

#pH for Scope Data run 1

ScopeData$pH_calc<-2.4041*log(ScopeData$ratio_1_over_2)+7.334


Scope_pH<-ScopeData %>% group_by(Condition) %>% summarise(ratio.mean=max(pH_calc,na.rm=TRUE))
as.data.frame(Scope_pH)


Lad1<-ScopeData$Condition %in% c( "L1-1","W1","Y1")
Lad2<-ScopeData$Condition %in% c( "L2-2","W2","Y2")
Lad4<-ScopeData$Condition %in% c( "L4","W4","Y4","L4v3")
Lad5<-ScopeData$Condition %in% c( "L5","W5","Y5")
Lad6<-ScopeData$Condition %in% c( "L6","W6v2","Y6")
Lad7<-ScopeData$Condition %in% c( "L7","W7v2","Y7")
Lad8<-ScopeData$Condition %in% c( "L8-2","W8","Y8","L8v2")

ScopeData$Actual_pH[Lad1] <- 6.06
ScopeData$Actual_pH[Lad2] <- 6.92
ScopeData$Actual_pH[Lad4] <- 7.72
ScopeData$Actual_pH[Lad5] <- 8.33
ScopeData$Actual_pH[Lad6] <- 8.69
ScopeData$Actual_pH[Lad7] <- 9.06
ScopeData$Actual_pH[Lad8] <- 9.59


Scope_pH<-ScopeData %>% group_by(Condition) %>% summarise(ratio.mean=max(pH_calc,na.rm=TRUE))
as.data.frame(Scope_pH)

Scope_Ladder<-ScopeData[which(ScopeData$Condition=="L1-1"|ScopeData$Condition=="L2-2"|ScopeData$Condition=="L4"|ScopeData$Condition=="L5"|ScopeData$Condition=="L6"|ScopeData$Condition=="L7"|ScopeData$Condition=="L8-2"),]
Scope_WT<-ScopeData[which(ScopeData$Condition=="W1"|ScopeData$Condition=="W2"|ScopeData$Condition=="W4"|ScopeData$Condition=="W5"|ScopeData$Condition=="W6v2"|ScopeData$Condition=="W7v2"|ScopeData$Condition=="W8"),]
Scope_ydcI<-ScopeData[which(ScopeData$Condition=="Y1"|ScopeData$Condition=="Y2"|ScopeData$Condition=="Y4"|ScopeData$Condition=="Y5"|ScopeData$Condition=="Y6"|ScopeData$Condition=="Y7"|ScopeData$Condition=="Y8"),]

###Plotting Run 1

ggplot(data=Scope_WT,aes(x=Actual_pH,y=pH_calc,col=pH_calc))+
  geom_jitter(alpha=0.3)+
  stat_summary(fun.y=median, geom="point", pch=3, color="black") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", color="black")+
  xlab("Environmental pH")+
  ylab("Intacellular pH (WT)")+
  theme_classic()+
  theme(legend.title = element_blank(),axis.title = element_text(face="bold",size=10),axis.text.x = element_text(angle=45,hjust=1))+
  scale_color_gradientn(colours=parula(1000), guide = "colourbar")+
  #scale_color_gradient(low="yellow",mid="green",high="blue", midpoint=8)+
  #scale_color_gradient(low="#AA0000",high="#00AAFF")+
  scale_y_continuous(limits=c(4,11))

ggplot(data=Scope_ydcI,aes(x=Actual_pH,y=pH_calc,col=Actual_pH))+
  geom_jitter(alpha=0.05)+
  stat_summary(fun.y=median, geom="point", pch=3) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar")+
  xlab("Environmental pH")+
  ylab("Intacellular pH (YdcI)")+
  theme_classic()+
  theme(legend.title = element_blank(),axis.title = element_text(face="bold",size=10),axis.text.x = element_text(angle=45,hjust=1))+
  scale_color_gradientn(colours=parula(1000), guide = "colourbar")+
  #scale_color_gradient(low="#AA0000",high="#00AAFF")+
  scale_y_continuous(limits=c(4,11))

######run 2
ScopeData2<-read.table("allCellsList_Run2.csv", sep=",",header=TRUE)
names(ScopeData2)

ScopeMean2<-ScopeData2 %>% group_by(Condition) %>% summarise(ratio.mean=median(ratio_1_over_2,na.rm=TRUE))
ScopeMean.tb2<-as.data.frame(ScopeMean2)
ScopeMean.tb2

#pH for Scope Data run 2

ScopeData2$pH_calc<-2.4041*log(ScopeData2$ratio_1_over_2)+7.334

Scope_pH2<-ScopeData2 %>% group_by(Condition) %>% summarise(ratio.mean=median(pH_calc,na.rm=TRUE))
as.data.frame(Scope_pH2)


Lad1<-ScopeData2$Condition %in% c( "L1","R1","RY1")
Lad2<-ScopeData2$Condition %in% c("R2","RY2")
Lad3<-ScopeData2$Condition %in% c("L3","R3","RY3")
Lad4<-ScopeData2$Condition %in% c("R4","RY4")
Lad5<-ScopeData2$Condition %in% c( "L5","R5","RY5")
Lad6<-ScopeData2$Condition %in% c("R6","RY6")
Lad7<-ScopeData2$Condition %in% c( "L7","R7","RY7")
Lad8<-ScopeData2$Condition %in% c( "L8","R8","RY8")

ScopeData2$Actual_pH[Lad1] <- 6.06
ScopeData2$Actual_pH[Lad2] <- 6.92
ScopeData2$Actual_pH[Lad3] <- 6.98
ScopeData2$Actual_pH[Lad4] <- 7.72
ScopeData2$Actual_pH[Lad5] <- 8.33
ScopeData2$Actual_pH[Lad6] <- 8.69
ScopeData2$Actual_pH[Lad7] <- 9.06
ScopeData2$Actual_pH[Lad8] <- 9.59

Scope_pH2<-ScopeData2 %>% group_by(Condition) %>% summarise(ratio.mean=max(pH_calc,na.rm=TRUE))
as.data.frame(Scope_pH2)

Scope_Ladder2<-ScopeData2[which(ScopeData2$Condition=="L1"|ScopeData2$Condition=="L3"|ScopeData2$Condition=="L5"|ScopeData2$Condition=="L7"|ScopeData2$Condition=="L8"),]
Scope_Rho<-ScopeData2[which(ScopeData2$Condition=="R1"|ScopeData2$Condition=="R2"|ScopeData2$Condition=="R3"|ScopeData2$Condition=="R4"|ScopeData2$Condition=="R5"|ScopeData2$Condition=="R6"|ScopeData2$Condition=="R7"|ScopeData2$Condition=="R8"),]
Scope_RY<-ScopeData2[which(ScopeData2$Condition=="RY1"|ScopeData2$Condition=="RY2"|ScopeData2$Condition=="RY3"|ScopeData2$Condition=="RY4"|ScopeData2$Condition=="RY5"|ScopeData2$Condition=="RY6"|ScopeData2$Condition=="RY7"|ScopeData2$Condition=="RY8"),]

###Run 3
ScopeData3<-read.table("allCellsList_Run3.csv", sep=",",header=TRUE)
names(ScopeData3)

ScopeMean3<-ScopeData3 %>% group_by(Condition) %>% summarise(ratio.mean=median(ratio_1_over_2,na.rm=TRUE))
ScopeMean3.tb<-as.data.frame(ScopeMean3)
ScopeMean3.tb

#pH for Scope Data run 3

ScopeData3$pH_calc<-3.169*log(ScopeData3$ratio_1_over_2)+7.0317

Scope_pH3<-ScopeData3 %>% group_by(Condition) %>% summarise(ratio.mean=median(pH_calc,na.rm=TRUE))
as.data.frame(Scope_pH3)


Lad1<-ScopeData3$Condition %in% c( "L1","R1","RY1","W1","Y1")
Lad2<-ScopeData3$Condition %in% c("L2","R2")
Lad4<-ScopeData3$Condition %in% c("L4","R4")
Lad5<-ScopeData3$Condition %in% c( "L5","R5")
Lad6<-ScopeData3$Condition %in% c("R6","L6")
Lad7<-ScopeData3$Condition %in% c( "L7","R7","RY7","W7","Y7")
Lad8<-ScopeData3$Condition %in% c( "L8","R8")
Lad9<-ScopeData3$Condition %in% c( "L9","R9","RY9","W9","Y9")
Lad10<-ScopeData3$Condition %in% c( "L10","R10","RY10","W10","Y10")

ScopeData3$Actual_pH[Lad1] <- 6.06
ScopeData3$Actual_pH[Lad2] <- 6.92
ScopeData3$Actual_pH[Lad9] <- 7.25
ScopeData3$Actual_pH[Lad10] <- 7.50
ScopeData3$Actual_pH[Lad4] <- 7.72
ScopeData3$Actual_pH[Lad5] <- 8.33
ScopeData3$Actual_pH[Lad6] <- 8.69
ScopeData3$Actual_pH[Lad7] <- 9.06
ScopeData3$Actual_pH[Lad8] <- 9.59


Scope_Ladder3<-ScopeData3[which(ScopeData3$Condition=="L1"|ScopeData3$Condition=="L2"|ScopeData3$Condition=="L4"|ScopeData3$Condition=="L5"|ScopeData3$Condition=="L6"|ScopeData3$Condition=="L7"|ScopeData3$Condition=="L8"|ScopeData3$Condition=="L9"|ScopeData3$Condition=="L10"),]
Scope_Rho3<-ScopeData3[which(ScopeData3$Condition=="R1"|ScopeData3$Condition=="R2"|ScopeData3$Condition=="R4"|ScopeData3$Condition=="R5"|ScopeData3$Condition=="R6"|ScopeData3$Condition=="R7"|ScopeData3$Condition=="R8"|ScopeData3$Condition=="R9"|ScopeData3$Condition=="R10"),]
Scope_RY3<-ScopeData3[which(ScopeData3$Condition=="RY1"|ScopeData3$Condition=="RY10"|ScopeData3$Condition=="RY7"|ScopeData3$Condition=="RY4"|ScopeData3$Condition=="RY9"),]
Scope_W3<-ScopeData3[which(ScopeData3$Condition=="W1"|ScopeData3$Condition=="W10"|ScopeData3$Condition=="W7"|ScopeData3$Condition=="W4"|ScopeData3$Condition=="W9"),]
Scope_Y3<-ScopeData3[which(ScopeData3$Condition=="Y1"|ScopeData3$Condition=="Y10"|ScopeData3$Condition=="Y7"|ScopeData3$Condition=="Y4"|ScopeData3$Condition=="Y9"),]

##### Combine Runs
Scope_RY3<-Scope_RY3[,c(1,2,3,4,5,6,7,8,9,10,12,13,14,15)]

W_bind<-rbind(Scope_WT,Scope_W3)
RY_bind<-rbind(Scope_RY,Scope_RY3)
Y_bind<-rbind(Scope_ydcI,Scope_Y3)
RY_bind_final<-RY_bind[which(RY_bind$Actual_pH!=6.92),]

W_bind$Background<-"WildType"
RY_bind$Background<-"RhoR109HYdcI"
Y_bind$Background<-"YdcI"
Scope_Rho3$Background<-"RhoR109H"
Scope_Ladder3$Background<-"Control"

W_bind_final<-W_bind[-c(11)]
Y_bind_final<-Y_bind[-c(11)]
Scope_Rho3_final<-Scope_Rho3[-c(11)]
Scope_Ladder3_final<-Scope_Ladder3[-c(11)]

AllScopeData<-rbind(W_bind_final,RY_bind,Y_bind_final,Scope_Rho3_final,Scope_Ladder3_final)
AllScopeDataforplot<-AllScopeData[which(AllScopeData$pH_calc>5.5),]

AllScopeDataforplot
AllScopeDataforplot$Background<-factor(AllScopeDataforplot$Background, levels=c("Control","WildType","RhoR109H","YdcI","RhoR109HYdcI"))


AllScopeDataPlot_withLegend<-ggplot(data=AllScopeDataforplot,aes(x=Actual_pH,y=pH_calc,col=pH_calc))+
  geom_jitter(alpha=0.3)+
  stat_summary(fun.y=mean, geom="point", pch=1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", color="black")+
  xlab("Environmental pH")+
  ylab("Intracellular pH")+
  theme_bw()+
  theme(legend.position="top",legend.title = element_text(face="bold"),axis.title = element_text(face="bold",size=10), panel.grid=element_blank(), strip.background = element_rect(fill="white"),strip.text = element_text(face="bold"),legend.background = element_rect(color="black"))+
  scale_color_gradientn(colours=parula(1000), guide = "colourbar",name="Intracellular pH")+
  #scale_color_gradient(low="#AA0000",high="#00AAFF")+
  scale_y_continuous(limits=c(6,11))+
  scale_x_continuous(limits=c(6.5,10))+
  facet_wrap(~Background,nrow=1,scales = "free")

AllScopeDataPlot<-ggplot(data=AllScopeDataforplot,aes(x=Actual_pH,y=pH_calc,col=pH_calc))+
  geom_jitter(alpha=0.3)+
  stat_summary(fun.y=mean, geom="point", pch=1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", color="black")+
  xlab("Environmental pH")+
  ylab("Intracellular pH")+
  theme_bw()+
  theme(legend.position="none",legend.title = element_text(face="bold"),axis.title = element_text(face="bold",size=10), panel.grid=element_blank(), strip.background = element_rect(fill="white"),strip.text = element_text(face="bold"),legend.background = element_rect(color="black"))+
  scale_color_gradientn(colours=parula(1000), guide = "colourbar",name="Intracellular pH")+
  #scale_color_gradient(low="#AA0000",high="#00AAFF")+
  scale_y_continuous(limits=c(6,11))+
  scale_x_continuous(limits=c(6.5,10))+
  facet_wrap(~Background,nrow=1,scales = "free")


####Range plot###
W_medians<-W_bind %>% group_by(Actual_pH) %>% summarise(pHmedian=median(pH_calc))
Y_medians<-Y_bind %>% group_by(Actual_pH) %>% summarise(pHmedian=median(pH_calc))
RY_medians<-RY_bind_final %>% group_by(Actual_pH) %>% summarise(pHmedian=median(pH_calc))
R_medians<-Scope_Rho3 %>% group_by(Actual_pH) %>% summarise(pHmedian=median(pH_calc))

W_medians$Strain<-"WT"
R_medians$Strain<-"Rho"
Y_medians$Strain<-"YdcI"
RY_medians$Strain<-"RY"

pHMedians<-rbind(W_medians,R_medians,Y_medians,RY_medians)
pHMedians_plotRange<-pHMedians[which(pHMedians$Actual_pH>6.5),]

pHMedians_minmax<-pHMedians_plotRange %>% group_by(Strain) %>% summarise(phMin=min(pHmedian), phMax=max(pHmedian))
pHMedians_minmax
pHMedians_minmax$Strain<-factor(pHMedians_minmax$Strain,levels=c("WT","Rho","YdcI","RY"))
pHMedians_minmax$header<-"header"
MedianPlot<-ggplot(pHMedians_minmax) + 
  geom_segment(aes(x=Strain, xend=Strain, y=phMin, yend=phMax,col=Strain), size=3) +
  scale_color_manual(values=c("#888888","#AA0000","#0000AA","#880088"))+
  ylab("pH Range")+
  xlab("Strains")+
  scale_y_continuous(limits=c(6,11))+
  #geom_hline(yintercept=mean((datap1$Maximo_horario + datap1$Promedio_diario)/2, na.rm=TRUE), color="red") +
  theme_bw()+
  theme(legend.position="none",legend.title = element_blank(),axis.title = element_text(face="bold",size=10),panel.grid = element_blank(),strip.background = element_rect(fill="white"),strip.text.x = element_text(face="bold"))+
  facet_wrap(~header)

pHi_Plot_andMedians<-plot_grid(AllScopeDataPlot,MedianPlot,nrow=1,rel_widths = c(5,1.15))
pHi_Plot_andMedians

LadderPlot<-ggplot(data=Scope_Ladder3,aes(x=Actual_pH,y=pH_calc,col=pH_calc))+
  geom_jitter(alpha=0.3)+
  stat_summary(fun.y=mean, geom="point", pch=1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", color="black")+
  xlab("Environmental pH")+
  ylab("Intacellular pH (Ladder Controls)")+
  theme_classic()+
  theme(legend.position="top",legend.background = element_rect(color="black"),axis.title = element_text(face="bold",size=10))+
  scale_color_gradient2(low="#3D2BAE",mid="#50AAD7",high="#F1E254", midpoint=8,name="Intracellular pH")+
  #scale_color_gradient(low="#AA0000",high="#00AAFF")+
  scale_y_continuous(limits=c(6,11))+
  scale_x_continuous(limits=c(6.5,10))
LadderPlot
