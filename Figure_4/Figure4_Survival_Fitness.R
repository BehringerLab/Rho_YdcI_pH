library(growthcurver)
library(ggplot2)
library(ggpattern)
library(ggpubr)
library(reshape2)
library(cowplot)
library(dbplyr)
library(scales)
library(magick)
library(pdftools)


##Code to analyze Survival Curves

survival_ecoli<-read.table("SurvivalCurve.txt",sep="\t",header=TRUE,check.names = FALSE)

survival_ecoli
survival_ecoli.melt<-reshape2::melt(survival_ecoli, id=c("Strain","Replicate","Day_1_CFU_Keep"))
survival_ecoli.melt.CFU<-survival_ecoli.melt[which(survival_ecoli.melt$variable=="0"|survival_ecoli.melt$variable=="1"|survival_ecoli.melt$variable=="2"|survival_ecoli.melt$variable=="3"|survival_ecoli.melt$variable=="4"|survival_ecoli.melt$variable=="5"|survival_ecoli.melt$variable=="6"|survival_ecoli.melt$variable=="7"|survival_ecoli.melt$variable=="8"|survival_ecoli.melt$variable=="9"|survival_ecoli.melt$variable=="10"|survival_ecoli.melt$variable=="11"|survival_ecoli.melt$variable=="12"|survival_ecoli.melt$variable=="13"|survival_ecoli.melt$variable=="14"|survival_ecoli.melt$variable=="21"),]
survival_ecoli.melt.CFU$Per_Survive<-survival_ecoli.melt.CFU$value/survival_ecoli.melt.CFU$Day_1_CFU_Keep
names(survival_ecoli.melt.CFU)<-c("Strain","Replicate","Day_1_CFU_Keep","Day","CFU","Per_Survive")
survival_ecoli.melt.CFU$Day<-as.character(survival_ecoli.melt.CFU$Day)
survival_ecoli.melt.CFU$Day<-as.numeric(survival_ecoli.melt.CFU$Day)
survival_ecoli.melt.CFU.WT<- survival_ecoli.melt.CFU[which(survival_ecoli.melt.CFU$Strain=="WT"),]
survival_ecoli.melt.CFU.Mut<- survival_ecoli.melt.CFU[which(survival_ecoli.melt.CFU$Strain!="WT"),]
survival_ecoli.melt.CFU.WT.Nostrain=subset(survival_ecoli.melt.CFU.WT, select = -c(Strain) )
survival_ecoli.melt.CFU$Strain<-factor(survival_ecoli.melt.CFU$Strain,levels=c("WT","Rho","ydcI","Rho_ydcI"),labels=c("WT",expression(paste(italic("rho")," R109H")),expression(paste(Delta,italic("ydcI"))),expression(paste(italic("rho")," R109H/",Delta,italic("ydcI")))))
survival_ecoli.melt.CFU$Strain

#Plots have been updated to have proper legend: Rho^R109H, delta ydcI italicized, etc.
survival_ecoli.melt.CFU
Plot_percent_survive<-ggplot(data=survival_ecoli.melt.CFU, aes(x=Day, y=Per_Survive, col=Strain, group=Strain))+
  geom_point(alpha=0.3)+
  stat_summary(fun.y=mean, geom="line", linetype="dashed")+
  stat_summary(fun.data="mean_se",geom="errorbar", size=1, width=0.2)+
  ylab(expression(paste(bold("Ratio CFU/mL"),~bold((T[x]/T[1])))))+
  xlab("Time (d)")+
  scale_color_manual(values =c("#888888","#DE2D26","#08306B","#660066"),name="Strain",labels=c("WT",expression("Rho"^"R109H"),expression(paste(Delta,italic("ydcI"))),expression(paste("Rho"^"R109H","/",Delta,italic("ydcI")))))+
  theme_classic()+ theme(axis.title = element_text(face="bold",size=8),legend.position=c(0.7,0.7))

Plot_percent_survive

Fig_4C_Survival_Plot<-ggplot()+
  geom_point(data=survival_ecoli.melt.CFU, aes(x=Day, y=CFU, col=Strain,group=Strain),alpha=0.3)+
  stat_summary(data=survival_ecoli.melt.CFU, aes(x=Day, y=CFU, col=Strain,group=Strain),fun.y=mean, geom="line", linetype="dashed")+
  stat_summary(data=survival_ecoli.melt.CFU, aes(x=Day, y=CFU, col=Strain,group=Strain),fun.data="mean_se",geom="errorbar", size=0.5, width=0.2)+
  scale_color_manual(values =c("#888888","#AA0000","#0000AA","#660066"),name="Strain")+
  scale_y_log10(label=scientific)+
  scale_x_continuous(limits=c(0,13))+
  ylab("CFU/mL")+
  xlab("Time (d)")+
  theme_bw()+theme(axis.title = element_text(face="bold",size=10), legend.position = "none",panel.grid = element_blank(), strip.background = element_rect(fill="white"))+facet_wrap(~Strain,nrow=4,labeller = label_parsed)


#Code to calculate pH dependent Fitness

PhPlot<-read.table("Comp_pH_mutants.txt", sep="\t",header=TRUE)
PhPlot
PhPlot$DayPair<-as.character(PhPlot$DayPair)
PhPlot$Comp2<-factor(PhPlot$Comp2,levels=c("Rho","ydcI","Rho_ydcI"))
PhPlot$pH<-factor(PhPlot$pH,levels=c("unbuffered","buffered_pH9"))

pH.labs <- c("Unbuffered", "Buffered pH9")
names(pH.labs) <- c("unbuffered", "buffered_pH9")


Figure4_B_pH_SelectionRate<-ggplot(data=PhPlot, aes(x=DayPair,y=s_Strain2, fill=Comp2,group=Comp2))+
  geom_point(aes(col=Comp2), position=position_dodge(width=1))+
  stat_summary(fun = "mean", geom = "col",position=position_dodge(width=1), color="black", alpha=0.5)+
  stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.2, position=position_dodge(width=1))+
  scale_color_manual(values=c("#AA0000","#0000AA","#660066"),name="Strain", breaks=c("Rho","ydcI","Rho_ydcI"),labels=c(expression("Rho"^"R109H"),expression(paste(Delta,italic("ydcI"))),expression(paste("Rho"^"R109H","/",Delta,italic("ydcI")))))+
  scale_fill_manual(values=c("#AA0000","#0000AA","#660066"),name="Strain", breaks=c("Rho","ydcI","Rho_ydcI"),labels=c(expression("Rho"^"R109H"),expression(paste(Delta,italic("ydcI"))),expression(paste("Rho"^"R109H","/",Delta,italic("ydcI")))))+
  xlab("Time (d)")+
  ylab("Selection Rate (s)")+
  theme_bw()+
  theme(strip.background = element_blank(), strip.text = element_text(face="bold"),panel.grid = element_blank(), legend.position = "none", axis.title = element_text(size=10,face="bold"), legend.background = element_rect(color="black"))+
  facet_wrap(~pH, labeller=labeller(pH=pH.labs))

FitSurvival<-ggdraw() + draw_image(magick::image_read_pdf("Survivalplot_AI_edit.pdf"))

plot_grid(FitSurvival, Figure4_Bottom, nrow=2)


####Code to plot Evolution Replay
Evo_replay<-read.table("EvolutionReplay.txt",sep="\t",header=TRUE)

Evo_replay
Evo_replay$InitialGenotype<-factor(Evo_replay$InitialGenotype,levels=c("WT","YdcI","Rho"))
#Evo_replay$FinalGenotype<-factor(Evo_replay$FinalGenotype,levels=c("WT","Rho","poly_YdcI","YdcI","YdcI_polyRY","RY"))
Evo_replay$FinalGenotype<-factor(Evo_replay$FinalGenotype,levels=c("RY","YdcI_polyRY","YdcI","poly_YdcI","Rho","WT"))

PatternPlot<-ggplot(Evo_replay, aes(x=InitialGenotype, y=Count,fill=FinalGenotype)) +
  geom_col_pattern(aes(fill=FinalGenotype, pattern=FinalGenotype, pattern_type=FinalGenotype,pattern_fill = FinalGenotype,pattern_density=FinalGenotype),colour='black',pattern="stripe",pattern_type="NA",alpha=0.5,pattern_alpha=0.4) +
  xlab("Intial Genotype")+
  ylab("Count after 100 days")+
  #scale_pattern_manual(values=c('stripe', 'stripe', 'stripe','stripe','stripe','stripe')) +
  #scale_pattern_type_manual(values=c(NA, NA,NA,NA,NA,NA))+
  scale_pattern_density_manual(values=c(0,0.5,0,0.5,0,0))+
  scale_fill_manual(values=c("#660066","#0000AA","#0000AA","#888888","#AA0000","#888888"))+
  scale_pattern_fill_manual(values=c("#660066", "#660066","#0000AA","#0000AA","#AA0000","#888888"))+
  theme_classic()+ theme(axis.title = element_text(face="bold",size=10),legend.position="right",legend.key.size = unit(0.7, 'cm'),legend.title = element_text(face="bold",size=8),legend.text = element_blank())


RightPlot<-plot_grid(PatternPlot,Figure4_B_pH_SelectionRate,nrow=2)

Figure4<-plot_grid(RightPlot,Fig_4C_Survival_Plot)


