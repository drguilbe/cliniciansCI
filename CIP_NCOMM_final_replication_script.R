#Replication for Centola, Guilbeault et al. (2021) Nature Communications
#Collective Intelligence in Clinician Networks
#Douglas Guilbeault
#October 2021

rm(list=ls());gc(); options(max.print=999999)
library(dplyr); library(ggplot2); library(reshape2);library(rstatix);library(stargazer);library(sjPlot)
library(lmtest);library(finalfit);library(multiwayvcov);library(effects);library(data.table)
source("C:/Users/dougl/Desktop/Categories/code/Functions/general/01_general_functions_DG.R")
min_max_norm<-function(x){(x - min(x,na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x,na.rm=TRUE))}

#For Figures
savepath<-"C:/Users/dougl/Desktop/Research/NDG/CI/Physicians/CIP_BIAS/FINAL_CIP2_CODE/final_results/revised/"

###########
#Load Data#
###########
datapath='C:/Users/dougl/Desktop/Research/NDG/CI/Physicians/CIP_BIAS/Data/'
load(paste(datapath, "final_NCOMM_CIP_data.Rdata",sep=""))
final_control_data<-subset(all_data, experimental_cond=="Control")
final_net_data<-subset(all_data, experimental_cond=="Network")

###############
#Organize Data#
###############
all_data_agg<-all_data %>% 
  group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::summarise(error = mean(abserror, na.rm=T), 
                   accuracy_norm = mean(accuracy_norm, na.rm=T), 
                   prop_correct=sum(correct)/length(correct)) 

all_data_chg<-all_data_agg %>% group_by(experimental_cond, patient_demo, trial_id) %>% 
  dplyr::summarise(chg_accuracy = accuracy_norm[round=="Final"] - accuracy_norm[round=="Initial"], 
                   chg_correct = prop_correct[round=="Final"] - prop_correct[round=="Initial"])

#############
#Fig1AB Data#
#############
fig1AB_main<-all_data_agg %>% group_by(experimental_cond, patient_demo, round) %>% 
  dplyr::summarise(ci_low = t.test(accuracy_norm, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(accuracy_norm, conf.level = 0.95)$conf.int[2], 
                   accuracy_norm = mean(accuracy_norm, na.rm=T)) 

fig1AB_inset<-all_data_chg %>% group_by(experimental_cond, patient_demo) %>% 
  dplyr::summarise(ci_low = t.test(chg_accuracy, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(chg_accuracy, conf.level = 0.95)$conf.int[2], 
                   chg_accuracy = mean(chg_accuracy, na.rm=T)) 

#Plot Figure 1AB, main and inset
ggplot(subset(fig1AB_main, experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = accuracy_norm, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  #geom_errorbar(width = 0.05, size = 2, aes(colour = patient_demo)) + 
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Control Condition") + 
  ylab("Average Accuracy of Diagnostic Assessment") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=30), 
        axis.title=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.2,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0.77,0.925)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig1A.png', width=10, height=10,path = savepath)

ggplot(subset(fig1AB_main, experimental_cond %in% c("Network") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = accuracy_norm, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  #geom_errorbar(width = 0.05, size = 2, aes(colour = patient_demo)) + 
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Network Condition") + 
  ylab("Average Accuracy of Assessment") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=30), 
        axis.title=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.2,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0.77,0.925)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig1B.png', width=10, height=10,path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1AB_inset, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = chg_accuracy, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1AB_inset, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(all_data_chg, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + coord_cartesian(ylim=c(-0.05, 0.105)) + 
  scale_y_continuous(breaks=c(-0.03, 0, 0.03,0.06, 0.09, 0.12), 
                     labels = scales::percent_format(accuracy = 1))

#ggsave('fig1A_inset.png', width=10, height=10,  path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1AB_inset, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = chg_accuracy, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1AB_inset, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(all_data_chg, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + coord_cartesian(ylim=c(-0.05, 0.105)) + 
  scale_y_continuous(breaks=c(-0.03, 0, 0.03,0.06, 0.09, 0.12), 
                     labels = scales::percent_format(accuracy = 1))

#ggsave('fig1B_inset.png', width=10, height=10,  path = savepath)

#######################
#Figure 1AB Statistics#
#######################

#compare initial assessment accuracy
wilcox.test(subset(all_data_agg, patient_demo=="Black female")$error, 
            subset(all_data_agg, patient_demo!="Black female")$error)

#Control, evaluate change in assessment accuracy
wilcox.test(subset(all_data_chg, experimental_cond == "Control" & patient_demo=="white male")$chg_accuracy)
mean(subset(all_data_chg, experimental_cond == "Control" & patient_demo=="white male")$chg_accuracy)
wilcox.test(subset(all_data_chg, experimental_cond == "Control" & patient_demo!="white male")$chg_accuracy)
mean(subset(all_data_chg, experimental_cond == "Control" & patient_demo!="white male")$chg_accuracy)

#Network, evaluate change in assessment accuracy
wilcox.test(subset(all_data_chg, experimental_cond == "Network" & patient_demo=="white male")$chg_accuracy)
mean(subset(all_data_chg, experimental_cond == "Network" & patient_demo=="white male")$chg_accuracy)
wilcox.test(subset(all_data_chg, experimental_cond == "Network" & patient_demo!="white male")$chg_accuracy)
mean(subset(all_data_chg, experimental_cond == "Network" & patient_demo!="white male")$chg_accuracy)

###############
#Fig. 1CD Data#
###############
fig1CD_main<-all_data_agg %>% group_by(experimental_cond, patient_demo, round) %>% 
  dplyr::summarise(ci_low = t.test(prop_correct, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(prop_correct, conf.level = 0.95)$conf.int[2], 
                   prop_correct = mean(prop_correct, na.rm=T)) 

fig1CD_main$patient_cond<-paste(fig1CD_main$experimental_cond, fig1CD_main$patient_demo, sep="_")

fig1CD_inset<-all_data_chg %>% group_by(experimental_cond, patient_demo) %>% 
  dplyr::summarise(ci_low = t.test(chg_correct, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(chg_correct, conf.level = 0.95)$conf.int[2], 
                   chg_correct = mean(chg_correct, na.rm=T)) 

#Plot Figure 1CD, main and inset
ggplot(subset(fig1CD_main, experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = prop_correct, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  #geom_errorbar(width = 0.05, size = 2, aes(colour = patient_demo)) + 
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Control Condition") + 
  ylab("Percent Clinicians Recommending\nGuideline Treatment") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=30), 
        axis.title=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.2,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0.05,0.29))

#ggsave('fig1C.png', width=10, height=10,path = savepath)

ggplot(subset(fig1CD_main, experimental_cond %in% c("Network") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = prop_correct, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Network Condition") + 
  ylab("Percent Clinicians Recommending\nGuideline Treatment") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=30), 
        axis.title=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.2,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0.05,0.29)) 

#ggsave('fig1D.png', width=10, height=10,path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1CD_inset, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = chg_correct, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1CD_inset, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = chg_correct,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(all_data_chg, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = chg_correct,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-0.07, 0.31)) + 
  scale_y_continuous(breaks=c(-0.05, 0, 0.05,0.1, 0.15, 0.2, 0.25, 0.3), 
                     labels = scales::percent_format(accuracy = 1))

#ggsave('fig1C_cntrl_inset.png', width=10, height=10,  path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1CD_inset, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = chg_correct, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1CD_inset, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = chg_correct,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(all_data_chg, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = chg_correct,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-0.07, 0.31)) + 
  scale_y_continuous(breaks=c(-0.05, 0, 0.05,0.1, 0.15, 0.2, 0.25, 0.3), 
    labels = scales::percent_format(accuracy = 1))

#ggsave('fig1D_net_inset.png', width=10, height=10,  path = savepath)

#######################
#Figure 1CD Statistics#
#######################

#evaluate initial differences in treatment
sum(subset(all_data, patient_demo == "Black female" & round =="Initial")$correct)/length(subset(all_data, patient_demo == "Black female" & round =="Initial")$correct)
sum(subset(all_data, patient_demo == "white male" & round =="Initial")$correct)/length(subset(all_data, patient_demo == "white male" & round =="Initial")$correct)

wilcox.test(subset(all_data_agg, patient_demo=="Black female" & round == "Initial")$prop_correct, 
            subset(all_data_agg, patient_demo!="Black female" & round == "Initial")$prop_correct)

#Control, evaluate change in treatment accuracy
wilcox.test(subset(all_data_chg, experimental_cond == "Control" & patient_demo=="white male")$chg_correct)
mean(subset(all_data_chg, experimental_cond == "Control" & patient_demo=="white male")$chg_correct)
wilcox.test(subset(all_data_chg, experimental_cond == "Control" & patient_demo!="white male")$chg_correct)
mean(subset(all_data_chg, experimental_cond == "Control" & patient_demo!="white male")$chg_correct)

wilcox.test(subset(all_data_agg, patient_demo=="Black female" & round == "Final" & experimental_cond == "Control")$prop_correct, 
            subset(all_data_agg, patient_demo!="Black female" & round == "Final" & experimental_cond == "Control")$prop_correct, paired=T)

#Network, evaluate change in assessment accuracy
wilcox.test(subset(all_data_chg, experimental_cond == "Network" & patient_demo=="white male")$chg_correct)
mean(subset(all_data_chg, experimental_cond == "Network" & patient_demo=="white male")$chg_correct)

mean(subset(all_data_agg, patient_demo=="Black female" & experimental_cond == "Network" & round == "Initial")$prop_correct)
mean(subset(all_data_agg, patient_demo=="Black female" & experimental_cond == "Network" & round == "Final")$prop_correct)

wilcox.test(subset(all_data_chg, experimental_cond == "Network" & patient_demo!="white male")$chg_correct)
mean(subset(all_data_chg, experimental_cond == "Network" & patient_demo!="white male")$chg_correct)

wilcox.test(subset(all_data_agg, patient_demo=="Black female" & experimental_cond == "Network" & round == "Final")$prop_correct, 
            subset(all_data_agg, patient_demo!="Black female" & experimental_cond == "Network" & round == "Final")$prop_correct, paired=T)

###############
#Fig. 1EF Data#
###############
fig1EF_main_raw<-all_data %>% group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::summarise(propA = length(unique(clinician_id[treat_rec=="A"]))/length(unique(clinician_id)), 
                   propC = length(unique(clinician_id[treat_rec=="C"]))/length(unique(clinician_id)),
                   propB = length(unique(clinician_id[treat_rec=="B"]))/length(unique(clinician_id)),
                   propD = length(unique(clinician_id[treat_rec=="D"]))/length(unique(clinician_id)),
                   A_BC_ratio = propA/(propB + propC), 
                   A_C_ratio = propA/propC) 

fig1EF_main<-fig1EF_main_raw %>% group_by(experimental_cond, patient_demo, round) %>% 
  dplyr::summarise(cimin=t.test(A_C_ratio)$conf.int, 
                   cimax=t.test(A_C_ratio)$conf.int, 
                   avg.all_ratio = mean(A_C_ratio, rm.na=T))

fig1EF_chg<-fig1EF_main_raw %>% group_by(experimental_cond, patient_demo, trial_id) %>% 
  dplyr::summarise(chg_A_C_ratio = A_C_ratio[round=="Final"] - A_C_ratio[round=="Initial"])

fig1EF_inset<-fig1EF_chg %>% group_by(experimental_cond, patient_demo) %>% 
  dplyr::summarise(ci_low = t.test(chg_A_C_ratio, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(chg_A_C_ratio, conf.level = 0.95)$conf.int[2], 
                   chg_A_C_ratio = mean(chg_A_C_ratio, na.rm=T)) 

#Plot Figure 1EF, main and inset
ggplot(subset(fig1EF_main, experimental_cond %in% c("Control")), 
       aes(x = round, y = avg.all_ratio, fill = patient_demo, group=patient_demo, ymin=cimin, ymax=cimax)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Control Condition") + 
  ylab("Odds of Patient Receiving Unsafe\n Under-treatment vs. Guideline Treatment") +
  xlab("Recommendation") + 
  theme(axis.text=element_text(size=30), 
        axis.title.y=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.65,0.8), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0,5))+ 
  geom_hline(yintercept = 1, linetype="dotted", size=2)

#ggsave('fig1E.png', width=10, height=10,path = savepath)

ggplot(subset(fig1EF_main, experimental_cond %in% c("Network")), 
       aes(x = round, y = avg.all_ratio, fill = patient_demo, group=patient_demo, ymin=cimin, ymax=cimax)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Network Condition") + 
  ylab("Odds of Patient Receiving Unsafe\n Under-treatment vs. Guideline Treatment") +
  xlab("Recommendation") + 
  theme(axis.text=element_text(size=30), 
        axis.title.y=element_text(size=30), 
        axis.title.x=element_blank(),
        legend.position=c(0.65,0.8), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0,5)) + 
  geom_hline(yintercept = 1, linetype="dotted", size=2)

#ggsave('fig1F.png', width=10, height=10,path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1EF_inset, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = chg_A_C_ratio, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1EF_inset, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = chg_A_C_ratio,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(fig1EF_chg, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = chg_A_C_ratio,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Change") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-9,2.5))   

#ggsave('fig1E_cntrl_inset.png', width=10, height=10,  path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(fig1EF_inset, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = chg_A_C_ratio, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(fig1EF_inset, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = chg_A_C_ratio,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(fig1EF_chg, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = chg_A_C_ratio,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Change") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-9,2.5))  

#ggsave('fig1F_net_inset.png', width=10, height=10,  path = savepath)

#######################
#Figure 1EF Statistics#
#######################
WM_r1<-subset(fig1EF_main_raw, patient_demo=="white male" & round == "Initial")
WM_r1$ac_ratio_centered<-WM_r1$A_C_ratio-1
wilcox.test(WM_r1$ac_ratio_centered)

BF_r1<-subset(fig1EF_main_raw, patient_demo=="Black female" & round == "Initial")
BF_r1$ac_ratio_centered<-BF_r1$A_C_ratio-1
wilcox.test(BF_r1$ac_ratio_centered)

wilcox.test(WM_r1$ac_ratio_centered, BF_r1$ac_ratio_centered, paired=T)

wilcox.test(subset(fig1EF_chg, experimental_cond=="Control" & patient_demo=="white male")$chg_A_C_ratio)
wilcox.test(subset(fig1EF_chg, experimental_cond=="Network" & patient_demo=="white male")$chg_A_C_ratio)

wilcox.test(subset(fig1EF_chg, experimental_cond=="Control" & patient_demo=="Black female")$chg_A_C_ratio)
wilcox.test(subset(fig1EF_chg, experimental_cond=="Network" & patient_demo=="Black female")$chg_A_C_ratio)

wilcox.test(subset(fig1EF_main_raw, experimental_cond=="Network" & patient_demo=="white male" & round == "Final")$A_C_ratio, 
            subset(fig1EF_main_raw, experimental_cond=="Network" & patient_demo=="Black female" & round == "Final")$A_C_ratio)

#############
##Figure 2A##
#############
fig2A<-all_data %>% group_by(clinician_id) %>% subset(Round1 & Round3) %>% 
  subset(experimental_cond=="Network") %>% group_by(experimental_cond, patient_demo, trial_id, clinician_id) %>% 
  dplyr::summarise(round1_accuracy = accuracy_norm[round=="Initial"], 
                   revision = abs(numans[round=="Final"] - numans[round=="Initial"]), 
                   round1_error = abs(error[round=="Initial"]), 
                   round1_error_raw = error[round=="Initial"])

fig2A$Patient.Demographic<-as.factor(fig2A$patient_demo)
levels(fig2A$Patient.Demographic)<-c("Black female", "white male")

ggplot(fig2A, aes(x = round1_error, y = revision, group=1, color=Patient.Demographic)) + 
  geom_jitter(size=7, width=0.01, height=0.02, alpha=1, shape=16) +
  scale_color_manual(values=c("pink3", "lightskyblue2"))+
  geom_smooth(method='lm', formula= y~x, size=3, se=TRUE, color="black", alpha=0.3) +
  dougtheme_mod + 
  theme(plot.title = element_text(size = 30)) + 
  ylab("Magnitude of Revision") +
  xlab("Initial Error of Diagnostic Assessment\n(Pct. Points from Accurate Response)") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        axis.title.y=element_text(size=40),
        legend.position=c(0.82,0.12), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0,80)) 

#ggsave('fig2A.png', width=12, height=12,path = savepath)

####################
#Fig. 2A Statistics#
####################
cor.test(fig2A$round1_error, fig2A$revision, method="pearson")
fig2A$Magnitude.of.revision<-fig2A$revision
fig2A$Intial.Error.Diagnostic.Assessment<-fig2A$round1_error

#############
##Figure 2B##
#############
final_control_data_sub<-final_control_data %>% select(-Round1, -Round3)
Fig2B_data<-rbind(final_net_data, final_control_data_sub)
Fig2B_data$Prob.Correct.Treatment<-Fig2B_data$correct
Fig2B_data$raw_error<- Fig2B_data$numans - 16
Fig2B_data$abserror<- abs(Fig2B_data$raw_error)

Fig2B_data_chg<-Fig2B_data %>% group_by(trial_id, experimental_cond, patient_demo, clinician_id) %>% 
  dplyr::summarise(round1_cat = treat_rec[round=="Initial"], round3_cat = treat_rec[round=="Final"], 
                   ans1 = numans[round=="Initial"], ans3 = numans[round=="Final"], 
                   error1 = abserror[round=="Initial"], error3 = abserror[round=="Final"], 
                   chg_ans = ans3 - ans1, chg_error = error3 - error1)

Fig2B_data_chg_agg<-Fig2B_data_chg %>% group_by(trial_id, experimental_cond, patient_demo) %>% 
  dplyr::summarise(avg_chg_error = mean(chg_error, na.rm=T), 
                   P_correct1 = sum(round1_cat == "C")/length(round1_cat), 
                   P_correct3 = sum(round3_cat == "C")/length(round3_cat),
                   chg_treatment = P_correct3 - P_correct1)

Fig2B_data_final<-subset(Fig2B_data_chg, round1_cat != "C")
Fig2B_data_final$improve<-Fig2B_data_final$round3_cat == "C"
Fig2B_data_final$Prob.Improve.Treatment.Accuracy<-Fig2B_data_final$round3_cat == "C"
Fig2B_data_final$Improve.Diagnostic.Accuracy<-Fig2B_data_final$chg_error * -1
Fig2B_data_final$Patient.Type<-Fig2B_data_final$patient_demo
Fig2B_data_final$Patient.Type<-as.factor(Fig2B_data_final$Patient.Type)
levels(Fig2B_data_final$Patient.Type)<-c("Black.Female","White.Male")
Fig2B_data_final$Experimental.Condition<-Fig2B_data_final$experimental_cond
Fig2B_data_final$Experimental.Condition<-as.factor(Fig2B_data_final$Experimental.Condition)
levels(Fig2B_data_final$Experimental.Condition)<-c("Networks","Control")
Fig2B_data_final$Patient.Type<- factor(Fig2B_data_final$Patient.Type, levels=rev(c("Black.Female", "White.Male")))
Fig2B_data_final$Experimental.Condition<- factor(Fig2B_data_final$Experimental.Condition, levels=rev(c("Networks","Control")))

logit_main <- glm(Prob.Improve.Treatment.Accuracy ~ 
                    Improve.Diagnostic.Accuracy * Patient.Type * Experimental.Condition, 
                  data=Fig2B_data_final, family = "binomial")
summary(logit_main)
tab_model(logit_main)

all_effects_table<-allEffects(logit_main)
all_effects_table<-as.data.frame(all_effects_table)
all_effects_table<-all_effects_table$`Improve.Diagnostic.Accuracy:Patient.Type:Experimental.Condition`

ggplot(subset(all_effects_table, Experimental.Condition=="Networks"), 
       aes(x = Improve.Diagnostic.Accuracy, y = fit, group=Patient.Type, color=Patient.Type)) + 
  geom_errorbar(aes(ymin=fit - se, ymax=fit + se), width=0, size=1, alpha=0.8, position=position_dodge(3)) + dougtheme_mod + 
  geom_line(size=2, position=position_dodge(3)) + 
  geom_point(size=8, shape=16, position=position_dodge(3)) + 
  scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ylab("Probability of Improving \n Treatment Recommendation") +
  xlab("Change in Accuracy\n of Diagnostic Assessment") + 
  theme(axis.text.x=element_text(size=40),
        axis.text.y=element_text(size=40),
        axis.title=element_text(size=30), 
        axis.title.x=element_text(size=40),
        axis.title.y=element_text(size=40),
        legend.position=c(0.25,0.9), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=40)) +
  theme(legend.title=element_blank()) + 
  scale_x_continuous(breaks=c(-50,-25,0,25,50))

#ggsave('fig2B.png', width=12, height=12,path = savepath)

############
##Figure 3##
############

#org fig. 3 network data
Fig3_net<-subset(all_data, experimental_cond=="Network") %>% group_by(patient_demo, trial_id, round) %>% 
  dplyr::mutate(num_subjs=length(unique(clinician_id))) %>% group_by(patient_demo, trial_id, round, treat_rec) %>% 
  dplyr::summarise(num_subjs = unique(num_subjs)[1], prop = length(unique(clinician_id))/num_subjs ) %>% mutate(experimental_cond = "Network") %>% 
  filter(round != "Second")

all_cats<-c("A","B","C","D")
iids=unique(Fig3_net$trial_id)
net_to_add<-data.frame()

for(iid in iids){
  print(iid)
  
  iid_df_01<-subset(Fig3_net, trial_id==iid)
  
  for(iid_cond in c("white male", "Black female")){
    iid_df<-subset(iid_df_01, patient_demo==iid_cond)
    iid_patient = unique(iid_df$patient_demo)
    
    for(r in c("Initial","Final")){
      r_iid_df<-subset(iid_df, round == r)
      r_cats = as.character(unique(r_iid_df$treat_rec))
      missing <- all_cats[!all_cats %in% r_cats]

      if(length(missing)>0){
        for(miss_cat in missing){
          net_to_add<-rbind(net_to_add, data.frame(patient_demo=iid_patient, trial_id=iid, round=r,
                                           treat_rec=miss_cat, prop=0,num_subjs = NA,experimental_cond="Network"))
        }
      }
    }
  }
}

Fig3_net<-rbind(as.data.frame(Fig3_net), net_to_add)
Fig3_net_org<-Fig3_net %>% group_by(trial_id, patient_demo, round, treat_rec, experimental_cond) %>%
  dplyr::summarise(prop = mean(prop,na.rm=T))

#org fig. 3 control data
Fig3_control<-subset(all_data, experimental_cond=="Control") %>% group_by(patient_demo, trial_id, round) %>% 
  dplyr::mutate(num_subjs=length(unique(clinician_id))) %>% group_by(patient_demo, trial_id, round, treat_rec) %>% 
  dplyr::summarise(num_subjs = unique(num_subjs)[1], prop = length(unique(clinician_id))/num_subjs ) %>% mutate(experimental_cond = "Control") %>% 
  filter(round != 2)

all_cats<-c("A","B","C","D")
iids=unique(Fig3_control$trial_id)
control_to_add<-data.frame()

for(iid in iids){
  print(iid)
  iid_df_01<-subset(Fig3_control, trial_id==iid)
  
  for(iid_cond in c("white male", "Black female")){
    iid_df<-subset(iid_df_01, patient_demo==iid_cond)
    iid_patient = unique(iid_df$patient_demo)
    
    for(r in c("Initial","Final")){
      r_iid_df<-subset(iid_df, round == r)
      r_cats = as.character(unique(r_iid_df$treat_rec))
      missing <- all_cats[!all_cats %in% r_cats]
      
      if(length(missing)>0){
        for(miss_cat in missing){
          control_to_add<-rbind(control_to_add, 
                                data.frame(patient_demo=iid_patient, trial_id=iid, round=r,
                                           treat_rec=miss_cat, prop=0,num_subjs = NA, experimental_cond="Control"))
        }
      }
    }
  }
}

Fig3_control<-rbind(as.data.frame(Fig3_control), control_to_add)
Fig3_control_org<-Fig3_control %>% group_by(trial_id, patient_demo, round, treat_rec, experimental_cond) %>%
  dplyr::summarise(prop = mean(prop,na.rm=T))

#Combine
Fig3_final<-rbind(Fig3_control_org, Fig3_net_org)
Fig3_final$treat_rec<-as.factor(Fig3_final$treat_rec)
Fig3_final$experimental_cond<-as.factor(Fig3_final$experimental_cond)
Fig3_final<-subset(Fig3_final, round %in% c("Initial","Final"))
Fig3_final_agg<-Fig3_final %>% group_by(patient_demo, round, treat_rec, experimental_cond) %>%
  dplyr::summarise(prop=mean(prop))

#Fig3A
ggplot(subset(Fig3_final_agg, patient_demo == "Black female" & experimental_cond=="Control"), aes(x=treat_rec, y=prop, fill=factor(round))) + 
  geom_bar(stat="identity",position="dodge", color="black",size=3) +
  scale_fill_manual(values=c("lightpink", "pink4")) +
  dougtheme_mod + theme(plot.title = element_text(size = 30, hjust = 0.5),
                        axis.text.x = element_text(size=32),
                        axis.text.y = element_text(size=32),
                        axis.title.x = element_text(size=32),
                        axis.title.y = element_text(size=30),
                        strip.text.x = element_text(size = 26),
                        legend.position="none") +
  ggtitle("Control Condition (Black Female Patient)") + 
  labs(y="Fraction of Clinicians\n Providing Recommendation", x="Recommendation", linetype=NULL) + 
  coord_cartesian(ylim=c(0, 0.6)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig3A.png', width=10, height=10,  path = savepath)

#Fig3B
ggplot(subset(Fig3_final_agg, patient_demo == "Black female" & experimental_cond!="Control"), aes(x=treat_rec, y=prop, fill=factor(round))) + 
  geom_bar(stat="identity",position="dodge", color="black",size=3) +
  scale_fill_manual(values=c("pink1", "pink4")) +
  dougtheme_mod + theme(plot.title = element_text(size = 30, hjust = 0.5),
                        axis.text.x = element_text(size=32),
                        axis.text.y = element_text(size=32),
                        axis.title.x = element_text(size=32),
                        axis.title.y = element_text(size=30),
                        strip.text.x = element_text(size = 26),
                        legend.position="none") +
  ggtitle("Network Condition (Black Female Patient)") + 
  labs(y="Fraction of Clinicians\n Providing Recommendation", x="Recommendation", linetype=NULL) + 
  coord_cartesian(ylim=c(0, 0.6)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig3B.png', width=10, height=10,  path = savepath)

#Fig3C
ggplot(subset(Fig3_final_agg, patient_demo == "white male" & experimental_cond=="Control"), aes(x=treat_rec, y=prop, fill=factor(round))) + 
  geom_bar(stat="identity",position="dodge", color="black",size=3) +
  scale_fill_manual(values=c("lightblue2", "dodgerblue4")) +
  dougtheme_mod + theme(plot.title = element_text(size = 30, hjust = 0.5),
                        axis.text.x = element_text(size=32),
                        axis.text.y = element_text(size=32),
                        axis.title.x = element_text(size=32),
                        axis.title.y = element_text(size=30),
                        strip.text.x = element_text(size = 26),
                        legend.position="none") +
  ggtitle("Control Condition (White Male Patient)") + 
  labs(y="Fraction of Clinicians\n Providing Recommendation", x="Recommendation", linetype=NULL) + 
  coord_cartesian(ylim=c(0, 0.6)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig3C.png', width=10, height=10,  path = savepath)

#Fig3D
ggplot(subset(Fig3_final_agg, patient_demo == "white male" & experimental_cond!="Control"), aes(x=treat_rec, y=prop, fill=factor(round))) + 
  geom_bar(stat="identity",position="dodge", color="black",size=3) +
  scale_fill_manual(values=c("lightblue2", "dodgerblue4")) +
  dougtheme_mod + theme(plot.title = element_text(size = 30, hjust = 0.5),
                        axis.text.x = element_text(size=32),
                        axis.text.y = element_text(size=32),
                        axis.title.x = element_text(size=32),
                        axis.title.y = element_text(size=30),
                        strip.text.x = element_text(size = 26),
                        legend.position="none") +
  ggtitle("Network Condition (White Male Patient)") + 
  labs(y="Fraction of Clinicians\n Providing Recommendation", x="Recommendation", linetype=NULL) + 
  coord_cartesian(ylim=c(0, 0.6)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('fig3D.png', width=10, height=10,  path = savepath)

#####################
#Figure 3 Statistics#
#####################
mean(subset(Fig3_final_agg, treat_rec=="A" & round=="Initial" & patient_demo=="Black female")$prop)
mean(subset(Fig3_final_agg, treat_rec=="C" & round=="Initial" & patient_demo=="Black female")$prop)

mean(subset(Fig3_final_agg, treat_rec=="A" & round=="Initial" & patient_demo=="white male")$prop)
mean(subset(Fig3_final_agg, treat_rec=="C" & round=="Initial" & patient_demo=="white male")$prop)

sum(subset(all_data, patient_demo=="Black female" & round=="Initial")$treat_rec == "A")/nrow(subset(all_data, patient_demo=="Black female" & round=="Initial"))
sum(subset(all_data, patient_demo=="Black female" & round=="Initial")$treat_rec == "C")/nrow(subset(all_data, patient_demo=="Black female" & round=="Initial"))

sum(subset(all_data, patient_demo=="white male" & round=="Initial")$treat_rec == "A")/nrow(subset(all_data, patient_demo=="white male" & round=="Initial"))
sum(subset(all_data, patient_demo=="white male" & round=="Initial")$treat_rec == "C")/nrow(subset(all_data, patient_demo=="white male" & round=="Initial"))

Fig3_agg_AC_diff<-Fig3_final %>% group_by(trial_id, patient_demo, experimental_cond, round) %>% 
  dplyr::summarise(propAC_diff=prop[treat_rec=="A"]-prop[treat_rec=="C"])

wilcox.test(subset(Fig3_agg_AC_diff, patient_demo=="Black female" & round=="Initial")$propAC_diff, 
            subset(Fig3_agg_AC_diff, patient_demo=="white male" & round=="Initial")$propAC_diff)

wilcox.test(subset(Fig3_agg_AC_diff, patient_demo=="Black female" & round=="Initial")$propAC_diff)
mean(subset(Fig3_agg_AC_diff, patient_demo=="Black female" & round=="Initial")$propAC_diff)
wilcox.test(subset(Fig3_agg_AC_diff, patient_demo=="white male" & round=="Initial")$propAC_diff)
mean(subset(Fig3_agg_AC_diff, patient_demo=="white male" & round=="Initial")$propAC_diff)

Fig3_agg_AC_diff_control<-subset(Fig3_agg_AC_diff, experimental_cond=="Control")
Fig3_agg_AC_diff_network<-subset(Fig3_agg_AC_diff, experimental_cond=="Network")
Fig3_agg_AC_diff_network_WM<-subset(Fig3_agg_AC_diff_network, patient_demo=="white male")
Fig3_agg_AC_diff_network_WM$trial_id<-as.numeric(as.factor(Fig3_agg_AC_diff_network_WM$trial_id))
Fig3_agg_AC_diff_network_BF<-subset(Fig3_agg_AC_diff_network, patient_demo=="Black female")
Fig3_agg_AC_diff_network_BF$trial_id<-as.numeric(as.factor(Fig3_agg_AC_diff_network_BF$trial_id))

Fig3_agg_AC_diff_final<-rbind(Fig3_agg_AC_diff_control, Fig3_agg_AC_diff_network_WM, Fig3_agg_AC_diff_network_BF)

Fig3_ineq_stat<-Fig3_agg_AC_diff_final %>% group_by(trial_id, round, experimental_cond) %>% 
  dplyr::summarise(inequity = propAC_diff[patient_demo=="Black female"] - propAC_diff[patient_demo=="white male"])

Fig3_ineq_chg<-Fig3_ineq_stat %>% group_by(trial_id, experimental_cond) %>% 
  dplyr::summarise(ineq_chg=inequity[round=="Initial"] - inequity[round=="Final"])

wilcox.test(subset(Fig3_ineq_chg, experimental_cond == "Control")$ineq_chg)
mean(subset(Fig3_ineq_stat, experimental_cond == "Control" & round=="Final")$inequity)
wilcox.test(subset(Fig3_ineq_stat, experimental_cond == "Control" & round=="Final")$inequity)

wilcox.test(subset(Fig3_agg_AC_diff, patient_demo=="Black female" & round=="Final" & experimental_cond=="Control")$propAC_diff, 
            subset(Fig3_agg_AC_diff, patient_demo=="white male" & round=="Final" & experimental_cond=="Control")$propAC_diff, paired=T)

wilcox.test(subset(Fig3_ineq_chg, experimental_cond == "Network")$ineq_chg)
mean(subset(Fig3_ineq_chg, experimental_cond == "Network")$ineq_chg)
mean(subset(Fig3_ineq_stat, experimental_cond == "Network" & round=="Final")$inequity)
wilcox.test(subset(Fig3_ineq_stat, experimental_cond == "Network" & round=="Final")$inequity)

wilcox.test(subset(Fig3_agg_AC_diff, patient_demo=="Black female" & round=="Final" & experimental_cond=="Network")$propAC_diff, 
            subset(Fig3_agg_AC_diff, patient_demo=="white male" & round=="Final" & experimental_cond=="Network")$propAC_diff)

#across both patients, overall change in option A
chg_by_treatment<-Fig3_final %>% group_by(trial_id, patient_demo, experimental_cond, treat_rec) %>% 
  dplyr::summarise(chg_prop=prop[round=="Final"] - prop[round=="Initial"])

mean(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "A")$chg_prop)
mean(subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "A")$chg_prop)

wilcox.test(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "A")$chg_prop, 
            subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "A")$chg_prop, paired=T)

mean(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "B")$chg_prop)
mean(subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "B")$chg_prop)

wilcox.test(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "B")$chg_prop, 
            subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "B")$chg_prop, paired=T)

mean(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "D")$chg_prop)
mean(subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "D")$chg_prop)

wilcox.test(subset(chg_by_treatment, experimental_cond=="Network" & treat_rec == "D")$chg_prop, 
            subset(chg_by_treatment, experimental_cond=="Control" & treat_rec == "D")$chg_prop, paired=T)

############################
###Supplementary Appendix###
############################

########
#TABLES#
########

#Formatting
all_data$Patient.Demographic<-all_data$patient_demo
all_data$Patient.Demographic = relevel(all_data$Patient.Demographic, ref=2)
all_data$Initial.Diagnostic.Assessment<-all_data$numans
all_data$Experimental.Condition<-all_data$experimental_cond
all_data$Prob.Correct.Treatment<-all_data$correct

general_stats<-all_data %>% group_by(experimental_cond, patient_demo, trial_id, round, treat_rec) %>% 
  dplyr::summarise(num_subjects = length(unique(clinician_id))) %>% 
  dplyr::group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::mutate(frac_subjects = num_subjects/sum(num_subjects))

#Show Tables#
TableS2<-subset(general_stats, experimental_cond=="Control")
TableS3<-subset(general_stats, experimental_cond=="Network")
TableS4<-TableS2 %>% group_by(experimental_cond, patient_demo, round, treat_rec) %>% dplyr::summarise(frac_subjects = mean(frac_subjects))
TableS5<-TableS3 %>% group_by(experimental_cond, patient_demo, round, treat_rec) %>% dplyr::summarise(frac_subjects = mean(frac_subjects))

#TableS6
all_data$Patient.Demographic = relevel(all_data$Patient.Demographic, ref=2)

TableS6<-lm(Initial.Diagnostic.Assessment ~ Patient.Demographic + Experimental.Condition + 
              Patient.Demographic * Experimental.Condition, data = subset(all_data, round=="Initial"))

tab_model(TableS6, show.se=TRUE)

#TableS7
TableS7 <- glm(Prob.Correct.Treatment ~ Initial.Diagnostic.Assessment + Patient.Demographic + Experimental.Condition, data=subset(all_data, round=="Initial"), family = "binomial")
tab_model(TableS7, show.se=TRUE)

#TableS8
all_data$Prob.Unsafe.Undertreatment<-all_data$treat_rec == "A"
TableS8 <- glm(Prob.Unsafe.Undertreatment ~ Initial.Diagnostic.Assessment + Patient.Demographic + Experimental.Condition, data=subset(all_data, round=="Initial"), family = "binomial")
tab_model(TableS8, show.se=TRUE)

#TableS9
Fig2B_data_final$Patient.Demographic<-Fig2B_data_final$Patient.Type
Table_S9 <- glm(Prob.Improve.Treatment.Accuracy ~ Improve.Diagnostic.Accuracy * Experimental.Condition + Patient.Demographic * Experimental.Condition, 
                data=Fig2B_data_final, family = "binomial")
tab_model(Table_S9, show.se=TRUE)

#TableS10
Table_S10 <- glm(Prob.Improve.Treatment.Accuracy ~ Improve.Diagnostic.Accuracy + Patient.Demographic, 
                data=subset(Fig2B_data_final, round1_cat == "A" & experimental_cond=="Network"), family = "binomial")
tab_model(Table_S10, show.se=TRUE)

#TableS11
all_data$Guideline.Recommended.Treatment<-all_data$correct
all_data$Round.of.Recommendation<-all_data$round

Table_s11_control <- glm(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Patient.Demographic, 
  data=subset(all_data, experimental_cond=="Control"), family = "binomial")
tab_model(Table_s11_control, show.se=TRUE)

Table_s11_control_clust <- miceadds::glm.cluster(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Patient.Demographic, 
  data=subset(all_data, experimental_cond=="Control"), family = "binomial",
  cluster="trial_id")
summary(Table_s11_control_clust)

Table_s11_net <- glm(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Patient.Demographic, 
  data=subset(all_data, experimental_cond=="Network"), family = "binomial")
tab_model(Table_s11_net, show.se=TRUE)

Table_s11_net_clust <- miceadds::glm.cluster(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Patient.Demographic, 
  data=subset(all_data, experimental_cond=="Network"),
  cluster="trial_id")
summary(Table_s11_net)

#TableS12
all_data$Experimental.Condition = relevel(all_data$Experimental.Condition, ref=2)
all_data$Patient.Demographic = relevel(all_data$Patient.Demographic, ref=2)

Table_s12 <- glm(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Experimental.Condition * Patient.Demographic, 
  data=all_data, family = "binomial")
tab_model(Table_s12, show.se=TRUE)

Table_s12_clust <- miceadds::glm.cluster(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Experimental.Condition * Patient.Demographic, 
  data=all_data,
  cluster="trial_id")
summary(Table_s12_clust)

Table_s12_noint <- glm(
  formula = Guideline.Recommended.Treatment ~ Round.of.Recommendation + Experimental.Condition + Patient.Demographic, 
  data=all_data, family = "binomial")
tab_model(Table_s12_noint, show.se=TRUE)

anova.res<-anova(Table_s12, Table_s12_noint, test="Chisq")

#TableS13
all_data$Unsafe.Undertreatment<-all_data$treat_rec=="A"
all_data$Undertreatment<-all_data$treat_rec=="B"
all_data$Overtreatment<-all_data$treat_rec=="D"
  
Table_s13a <- glm(
  formula = Unsafe.Undertreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data, family = "binomial")
tab_model(Table_s13a, show.se=TRUE)

Table_s13a_clust <- miceadds::glm.cluster(
  formula = Unsafe.Undertreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data,
  cluster="trial_id")
summary(Table_s13a_clust)

Table_s13b <- glm(
  formula = Undertreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data, family = "binomial")
tab_model(Table_s13b, show.se=TRUE)

Table_s13b_clust <- miceadds::glm.cluster(
  formula = Undertreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data,
  cluster="trial_id")
summary(Table_s13b_clust)

Table_s13d <- glm(
  formula = Overtreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data, family = "binomial")
tab_model(Table_s13d, show.se=TRUE)

Table_s13d_clust <- miceadds::glm.cluster(
  formula = Overtreatment ~ Patient.Demographic + Round.of.Recommendation * Experimental.Condition, 
  data=all_data,
  cluster="trial_id")
summary(Table_s13d_clust)

#TableS14
Table_s14<-lm(Magnitude.of.revision ~ Intial.Error.Diagnostic.Assessment + Patient.Demographic, data=fig2A)
tab_model(Table_s14, show.se=TRUE)

Table_s14_clust <- miceadds::lm.cluster(
  formula = revision ~ round1_error + Patient.Demographic, data=fig2A,
  cluster="trial_id")
summary(Table_s14_clust)

#######################
#Supplementary Figures#
#######################

###########
#Figure S8#
###########
rev_coeff <- lm(Initial.Diagnostic.Assessment ~  Patient.Demographic *  Experimental.Condition, data=subset(all_data, round=="Initial"))
all_effects_df<-allEffects(rev_coeff)
all_effects_df<-data.frame(all_effects_df$`Patient.Demographic:Experimental.Condition`)

ggplot(all_effects_df, aes(x = Experimental.Condition, y = fit, group=Patient.Demographic, color=Patient.Demographic)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, size=1, alpha=0.8, position=position_dodge(1)) + dougtheme_mod + 
  geom_line(size=2, position=position_dodge(1)) + geom_point(size=8, shape=16, position=position_dodge(1)) + 
  scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Initial Diagnostic Assessment") +
  theme(axis.text.x=element_text(size=40),axis.text.y=element_text(size=40),
        axis.title=element_text(size=30), axis.title.x=element_blank(),
        axis.title.y=element_text(size=40),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30), 
        strip.text.x = element_text(size = 40, color = "black", face = "italic")) +
  theme(legend.title=element_blank()) + facet_wrap(~Patient.Demographic)

#ggsave('Fig_S8.png', width=20, height=10,path = savepath)

###########
#Figure S9#
###########
continuum_truths<-seq(12, 17, by=c(1))
FigS9<-data.frame()

for(truth in continuum_truths){
  t_all_data<-all_data
  t_all_data$numtruth<-truth
  t_all_data$abserror<-abs(t_all_data$numans - t_all_data$numtruth)
  t_all_data$accuracy<-t_all_data$abserror * -1
  t_all_data$accuracy_norm<-min_max_norm(t_all_data$accuracy)
  t_all_data_agg<-subset(t_all_data, patient_demo %in% c("Black female", "white male")) %>% 
    group_by(experimental_cond, patient_demo, trial_id, round) %>% 
    dplyr::summarise(error = mean(abserror, na.rm=T), accuracy_norm = mean(accuracy_norm, na.rm=T)) 
  
  t_all_data_chg<-t_all_data_agg %>% group_by(experimental_cond, patient_demo, trial_id) %>% 
    dplyr::summarise(chg_error = error[round=="Final"] - error[round=="Initial"], 
                     chg_accuracy = accuracy_norm[round=="Final"] - accuracy_norm[round=="Initial"])
  
  control_avg<-mean(subset(t_all_data_chg, experimental_cond != "Network")$chg_accuracy, na.rm=T)
  control_cilow<-t.test(subset(t_all_data_chg, experimental_cond != "Network")$chg_accuracy)$conf.int[1]
  control_cihi<-t.test(subset(t_all_data_chg, experimental_cond != "Network")$chg_accuracy)$conf.int[2]
  network_avg<-mean(subset(t_all_data_chg, experimental_cond == "Network")$chg_accuracy, na.rm=T)
  network_cilow<-t.test(subset(t_all_data_chg, experimental_cond == "Network")$chg_accuracy)$conf.int[1]
  network_cihi<-t.test(subset(t_all_data_chg, experimental_cond == "Network")$chg_accuracy)$conf.int[2]
  
  t_all_data_chg_agg<-t_all_data_chg %>% group_by(experimental_cond, patient_demo) %>%
    dplyr::summarise(mean_chg = mean(chg_accuracy, na.rm=T), 
                     cilow = t.test(chg_accuracy)$conf.int[1], 
                     cihi= t.test(chg_accuracy)$conf.int[2]) %>% mutate(truth=truth)
  FigS9<-rbind(FigS9,t_all_data_chg_agg)
}

FigS9$condition_full<-paste(FigS9$experimental_cond, FigS9$patient_demo, sep="_")
FigS9$condition_full<-as.factor(FigS9$condition_full)
FigS9$truth<-as.numeric(as.character(FigS9$truth))

ggplot(FigS9,  aes(x = truth, y = mean_chg, color = condition_full, group=condition_full, 
                   shape = experimental_cond, ymin=cilow, ymax=cihi)) + theme_bw()+ dougtheme + 
  geom_point(position = position_dodge(0.8),size = 5) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 0.5) + 
  scale_color_manual(values=c("pink3", "lightskyblue2", "pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  xlab("Accurate Diagnostic Assessment") +
  ylab("Change in Accuracy of\nDiagnostic Assessment\n(Initial to Final Round)") +
  theme(axis.text=element_text(size=35), axis.title.x=element_text(size=35), 
        axis.title.y=element_text(size=35),axis.text.x=element_text(size=35),
        legend.position="none",legend.text=element_text(size=30)) +
  scale_y_continuous(breaks=c(-0.02,0,0.02,0.04,0.06,0.08), labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks=continuum_truths) + 
  coord_cartesian(ylim=c(-0.035,0.09)) + 
  geom_hline(yintercept = 0) 

#ggsave('FigS9.png', width=10, height=10,  path = savepath)

############
#Figure S10#
############
all_data[all_data$numans<12 & !is.na(all_data$numans),]$abserror<-abs(all_data[all_data$numans<12 & !is.na(all_data$numans),]$numans - 12)
all_data[all_data$numans>17 & !is.na(all_data$numans),]$abserror<-abs(all_data[all_data$numans>17 & !is.na(all_data$numans),]$numans - 17)
all_data[all_data$numans<=17 & all_data$numans>=12 & !is.na(all_data$numans),]$abserror<-0
all_data$accuracy<-all_data$abserror * -1
all_data$accuracy_norm<-min_max_norm(all_data$accuracy)

figS10_raw<-subset(all_data, patient_demo %in% c("Black female", "white male")) %>% 
  group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::summarise(error = mean(abserror, na.rm=T), accuracy_norm = mean(accuracy_norm, na.rm=T)) 

figS10<-figS10_raw%>% group_by(experimental_cond, patient_demo, round) %>% 
  dplyr::summarise(ci_low = t.test(accuracy_norm, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(accuracy_norm, conf.level = 0.95)$conf.int[2], 
                   accuracy_norm = mean(accuracy_norm, na.rm=T))

ggplot(subset(figS10, experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = accuracy_norm, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 40)) + ggtitle("Control Condition") + 
  ylab("Average Accuracy of\nDiagnostic Assessment") +xlab("Assessment") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        legend.position=c(0.24,0.9), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0.83,0.95)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('FigS10A.png', width=10, height=10,path = savepath)

ggplot(subset(figS10, !experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = accuracy_norm, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 40)) + 
  ggtitle("Network Condition") + 
  ylab("Average Accuracy of\nDiagnostic Assessment") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        legend.position=c(0.24,0.1), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0.83,0.95)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('FigS10B.png', width=10, height=10,path = savepath)

#FigS10 Insets
figS10_inset_raw<-figS10_raw %>% group_by(experimental_cond, patient_demo, trial_id) %>% 
  dplyr::summarise(chg_error = error[round=="Final"] - error[round=="Initial"], 
                   chg_accuracy = accuracy_norm[round=="Final"] - accuracy_norm[round=="Initial"])

figS10_inset<-figS10_inset_raw %>% group_by(experimental_cond, patient_demo) %>%
  dplyr::summarise(ci_low = t.test(chg_accuracy)$conf.int[1], 
                   ci_hi= t.test(chg_accuracy)$conf.int[2], 
                   chg_accuracy = mean(chg_accuracy, na.rm=T)) 

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(figS10_inset, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = chg_accuracy, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(figS10_inset, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(figS10_inset_raw, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + coord_cartesian(ylim=c(-0.06, 0.105)) + 
  scale_y_continuous(breaks=c(-0.03, 0, 0.03,0.06, 0.09, 0.12), 
                     labels = scales::percent_format(accuracy = 1))

#ggsave('FigS10A_inset.png', width=10, height=10,  path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(figS10_inset, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = chg_accuracy, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(figS10_inset, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(figS10_inset_raw, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = chg_accuracy,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + coord_cartesian(ylim=c(-0.06, 0.105)) + 
  scale_y_continuous(breaks=c(-0.03, 0, 0.03,0.06, 0.09, 0.12), 
                     labels = scales::percent_format(accuracy = 1))

#ggsave('FigS10B_inset.png', width=10, height=10,  path = savepath)

#Figure S11
all_data$acceptable_treat<-all_data$treat_rec %in% c("B","C")

###
FigS11_raw<-subset(all_data, patient_demo %in% c("Black female", "white male")) %>% 
  group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::summarise(error = mean(abserror, na.rm=T), 
                   accuracy_norm = mean(accuracy_norm, na.rm=T), 
                   prop_correct=sum(acceptable_treat)/length(acceptable_treat)) 
FigS11_raw$patient_cond<-paste(FigS11_raw$experimental_cond, FigS11_raw$patient_demo, sep="_")

FigS11<- FigS11_raw %>% group_by(experimental_cond, patient_demo, round, patient_cond) %>% 
  dplyr::summarise(ci_low = t.test(prop_correct, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(prop_correct, conf.level = 0.95)$conf.int[2], 
                   prop_correct = mean(prop_correct, na.rm=T))

FigS11_insets_raw<-FigS11_raw %>% group_by(experimental_cond, patient_demo, trial_id, patient_cond) %>% 
  dplyr::summarise(chg_error = error[round=="Final"] - error[round=="Initial"], 
                   chg_accuracy = accuracy_norm[round=="Final"] - accuracy_norm[round=="Initial"], 
                   chg_correct = prop_correct[round=="Final"] - prop_correct[round=="Initial"]) 

FigS11_insets<- FigS11_insets_raw %>% group_by(experimental_cond, patient_demo,patient_cond) %>% 
  dplyr::summarise(ci_low = t.test(chg_correct, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(chg_correct, conf.level = 0.95)$conf.int[2], 
                   chg_correct = mean(chg_correct, na.rm=T))

######
ggplot(subset(FigS11, experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final") ), 
       aes(x = round, y = prop_correct, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  #geom_errorbar(width = 0.05, size = 2, aes(colour = patient_demo)) + 
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 40)) + 
  ggtitle("Control Condition") + 
  ylab("% Clinicians Providing Acceptable\nTreatment Recommendations (B or C)") +
  xlab("Assessment") + 
  theme(axis.text=element_text(size=40), 
        axis.title=element_text(size=40), 
        axis.title.x=element_text(size=40),
        legend.position=c(0.24,0.9), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0.57, 0.85))

#ggsave('figS11A.png', width=10, height=10,path = savepath)

ggplot(subset(FigS11, !experimental_cond %in% c("Control") & round %in% c("Initial", "Second", "Final")), 
       aes(x = round, y = prop_correct, fill = patient_demo, group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) + geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 40)) + ggtitle("Network Condition") + 
  ylab("% Clinicians Providing Acceptable\nTreatment Recommendations (B or C)") + xlab("Assessment") + 
  theme(axis.text=element_text(size=40), axis.title=element_text(size=40), axis.title.x=element_text(size=40),
        legend.position=c(0.24,0.9), plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim=c(0.57, 0.85))

#ggsave('figS11B.png', width=10, height=10,path = savepath)

#Fig S11 Insets
ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(FigS11_insets, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = chg_correct, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(FigS11_insets, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = chg_correct,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(FigS11_insets_raw, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = chg_correct,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-0.15, 0.35)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('FigS11A_inset.png', width=10, height=10,path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(FigS11_insets, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = chg_correct, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(FigS11_insets, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = chg_correct,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(FigS11_insets_raw, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = chg_correct,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-0.15, 0.35)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ggsave('FigS11B_inset.png', width=10, height=10,path = savepath)

############
#Figure S12#
############
FigS12_raw<-all_data %>% group_by(experimental_cond, patient_demo, trial_id, round) %>% 
  dplyr::summarise(propA = length(unique(clinician_id[treat_rec=="A"]))/length(unique(clinician_id)), 
                   propC = length(unique(clinician_id[treat_rec=="C"]))/length(unique(clinician_id)),
                   propB = length(unique(clinician_id[treat_rec=="B"]))/length(unique(clinician_id)),
                   propD = length(unique(clinician_id[treat_rec=="D"]))/length(unique(clinician_id)),
                   A_BC_ratio = propA/(propB + propC), 
                   A_C_ratio = propA/propC) 

FigS12<-FigS12_raw %>% group_by(experimental_cond, patient_demo, round) %>% 
  dplyr::summarise(cimin=t.test(A_BC_ratio)$conf.int, 
                   cimax=t.test(A_BC_ratio)$conf.int, 
                   avg.all_ratio = mean(A_BC_ratio, rm.na=T))

FigS12_inset_raw<-FigS12_raw %>% group_by(experimental_cond, patient_demo, trial_id) %>% 
  dplyr::summarise(A_BC_ratio_chg = A_BC_ratio[round=="Final"] - A_BC_ratio[round=="Initial"])

FigS12_inset<-FigS12_inset_raw %>% group_by(experimental_cond, patient_demo) %>% 
  dplyr::summarise(ci_low = t.test(A_BC_ratio_chg, conf.level = 0.95)$conf.int[1], 
                   ci_hi = t.test(A_BC_ratio_chg, conf.level = 0.95)$conf.int[2], 
                   A_BC_ratio_chg = mean(A_BC_ratio_chg, na.rm=T))

#FigS12A
ggplot(subset(FigS12, experimental_cond %in% c("Control")), 
       aes(x = round, y = avg.all_ratio, fill = patient_demo, group=patient_demo, ymin=cimin, ymax=cimax)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Control Condition") + 
  ylab("Odds of Patient Receiving Unsafe (A) vs.\nGuideline-Recommended Treatment (B&C)") +
  xlab("Recommendation") + 
  theme(axis.text=element_text(size=30), 
        axis.title.y=element_text(size=30), 
        axis.title.x=element_text(size=30), 
        legend.position=c(0.24,0.2), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0,0.6))

#ggsave('FigS12A.png', width=10, height=10,path = savepath)

ggplot(subset(FigS12, !experimental_cond %in% c("Control")), 
       aes(x = round, y = avg.all_ratio, fill = patient_demo, group=patient_demo, ymin=cimin, ymax=cimax)) + 
  geom_point(size=8, aes(colour = patient_demo), shape = 16) +
  geom_line(size=4, aes(colour = patient_demo))+
  dougtheme_mod + scale_colour_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + 
  ggtitle("Network Condition") + 
  ylab("Odds of Patient Receiving Unsafe (A) vs.\nGuideline-Recommended Treatment (B&C)") +
  xlab("Recommendation") + 
  theme(axis.text=element_text(size=30), 
        axis.title.y=element_text(size=30), 
        axis.title.x=element_text(size=30), 
        legend.position=c(0.24,0.2), 
        plot.title = element_text(hjust = 0.5), 
        legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(0,0.6))

#ggsave('FigS12B.png', width=10, height=10,path = savepath)

#Fig S12 insets
ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(FigS12_inset, experimental_cond %in% c("Control") ), 
           aes(x = patient_demo, y = A_BC_ratio_chg, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(FigS12_inset, experimental_cond %in% c("Control") ), 
                aes(x = patient_demo, y = A_BC_ratio_chg,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(FigS12_inset_raw, experimental_cond %in% c("Control") ), 
             aes(x = patient_demo, y = A_BC_ratio_chg,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-1.2, 0.45))

#ggsave('FigS12A_inset.png', width=10, height=10,path = savepath)

ggplot() + dougtheme + 
  geom_bar(position = "dodge", stat="identity", width=0.8, color = "black", size = 2, 
           data = subset(FigS12_inset, experimental_cond %in% c("Network") ), 
           aes(x = patient_demo, y = A_BC_ratio_chg, fill = patient_demo, group=patient_demo)) + 
  geom_errorbar(width = 0.3, position = position_dodge(0.8), size = 3, 
                data = subset(FigS12_inset, experimental_cond %in% c("Network") ), 
                aes(x = patient_demo, y = A_BC_ratio_chg,  group=patient_demo, ymin=ci_low, ymax=ci_hi)) + 
  geom_point(position = position_dodge(0.8), size = 12, 
             data = subset(FigS12_inset_raw, experimental_cond %in% c("Network") ), 
             aes(x = patient_demo, y = A_BC_ratio_chg,  group=patient_demo, 
                 fill=patient_demo),color="black", pch=21, alpha=0.7, stroke = 2) + 
  dougtheme + scale_fill_manual(values=c("pink3", "lightskyblue2")) + 
  theme(plot.title = element_text(size = 30)) + ylab("Total Improvement") +
  theme(axis.text=element_text(size=55), axis.title=element_text(size=60), 
        axis.text.x=element_blank(),legend.position="none", 
        plot.title = element_text(hjust = 0.5), legend.text=element_text(size=30)) +
  theme(legend.title=element_blank()) + 
  coord_cartesian(ylim=c(-1.2, 0.45))

#ggsave('FigS12B_inset.png', width=10, height=10,path = savepath)
