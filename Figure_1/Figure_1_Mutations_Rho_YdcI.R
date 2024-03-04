library(ggplot2)
library(ggpubr)

Rho_YdcI.Simple<-read.table("Rho_YdcI_PopDynamicsMain.txt", sep= "\t", header=TRUE, check.names =FALSE)
Rho_YdcI.Simple
Rho_YdcI_Dynamics<-ggplot(Rho_YdcI.Simple,aes(x=Time,y=Count,linetype=Status,col=Locus))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits=c(0,900),breaks=c(0,90,200,300,400,500,600,700,800,900))+
  scale_color_manual(values=c("#AA0000","#0000AA"), labels=c(expression(italic("rho")),expression(italic("ydcI"))))+
  xlab("Time (d)")+
  ylab("# of Populations")+
  theme_classic()+theme(axis.title = element_text(face="bold",size=8), legend.background = element_rect(color="black"), legend.title = element_text(face="bold",size=8), legend.title.align=0.5,legend.position="top")
Rho_YdcI_Dynamics

RY_fixation<-read.table("RY_Dynamics_Fixation.txt",sep="\t",header=TRUE)

RY_fixation_summary<-RY_fixation%>%group_by(Locus,Fixation,Effect,Sample)%>% summarise(total=n())

compare_means(data=RY_fixation,Fixation~Locus,method="t.test")
RY_fixation

Fig1_bar<-ggplot(RY_fixation_summary, aes(x=Fixation, y=total, col=Locus)) +
  geom_col_pattern(aes(fill=Locus, pattern_type=Effect,pattern_fill=Effect,col=Locus),pattern="stripe",alpha=1,pattern_alpha=1,pattern_density=0.55) +
  xlab("Mutations fixed")+
  ylab("Time (d)")+
  scale_x_continuous(limits=c(0,900), breaks=c(0,300,600,900))+
  #scale_pattern_manual(values=c('stripe', 'stripe', 'stripe','stripe','stripe','stripe','stripe', 'stripe', 'stripe','stripe','stripe','stripe','stripe','stripe')) +
  scale_pattern_type_manual(values=c(NA, NA,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))+
  #scale_pattern_density_manual(values=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))+
  scale_fill_manual(values=c("#AA0000","#0000AA"))+
  scale_color_manual(values=c("#880000","#000088"))+
  scale_pattern_fill_manual(values=c("#A0649F", "#7EB456","#E28835","#888888","#888888","#888888","#888888","#E932E9","#7916EE","#E7752D","#FF0000","#2E6DE2","#69E3E3","#5ECF3C"))+
  theme_classic()+ theme(axis.title = element_text(face="bold",size=10),legend.position="none",legend.key.size = unit(0.7, 'cm'),legend.title = element_text(face="bold",size=8),legend.text = element_blank())+
  facet_wrap(~Locus, nrow=2,scales="free_x")

RY_fixation_summary

Fig1_ribbon<-ggplot(RY_fixation_summary, aes(x=Locus, group=Sample))+
  geom_ribbon(aes(ymin=Fixation-40,ymax=Fixation+40), color="black",fill="#880088")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(limits=c(0,900), breaks=c(0,300,600,900))+
  theme_classic()+
  coord_flip()
plot_grid(Fig1_bar,Fig1_ribbon, nrow=2,rel_heights=c(2,0.6), align="hv")
