if (!requireNamespace("devtools", quietly=TRUE))
  install.packages("devtools")

devtools::install_github("YuLab-SMU/ggmsa")
library(ggmsa)
library(msa)
library(pbase)
library(ggplot2)


setwd("/Users/behrinmg/Library/CloudStorage/Box-Box/Behringer_Lab_Box_Drive/Manuscripts/In_Progress/Rho_YdcI_pH/PaperSubmission/RScripts/Figure_5")
fas<-"WT_Rho_align_Final.fa"
RhoAA<-readAAStringSet(fas)
RhoAA
WT_RhoAlign<-ggmsa(RhoAA, start = 200, end = 240, char_width = 0.5, seq_name = T,color="Shapely_AA") + geom_seqlogo() 


RhoAA<-readAAStringSet(fas)
RhoAA
RhoAA_Align <- msa(RhoAA)
RhoAA_Align

Mutfas<-"Mut_Rho_align_Final.fa"

MutRhoAA<-readAAStringSet(Mutfas)
MutRhoAA
Mut_RhoAlign<-ggmsa(MutRhoAA,  start = 200, end = 240, char_width = 0.5, seq_name = T,color="Shapely_AA") + geom_seqlogo()

Allfas<-"Combined_Rho_align2.fa"

AllRhoAA<-readAAStringSet(Allfas)
AllRhoAA$`Query_10004 NP_462807.1_Salmonella_enterica_Typhimurium`


plot1<-ggmsa(AllRhoAA, start = 200, end = 250, char_width = 0.5, seq_name = T,color="Shapely_AA") + geom_seqlogo()

plot1+theme(axis.text.y = element_text(size=6,face="bold"))

plot_grid(WT_RhoAlign,Mut_RhoAlign, nrow=2,align = 'v', rel_heights = c(1,0.8))
