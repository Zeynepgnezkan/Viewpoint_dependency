# Zeynep G. Özkan, Ayşe C. Şimşek, Tolgahan Aydın, 2022

rm(list= ls())


######        Main raw data column explanation:       #######

# Participant:            participant number
# Condition:              order type of the videos
# Perspective:            perspectives
# Activity:               action type made by actors in videos
# Actor:                  actor group
# Shot_Type:              type of shots
# movie_resp.keys:        button pressed by the participants to answer video memory part
# movie_resp.corr:        participants accuracy in video memory part
# movie_resp.rt:          participants reaction time in video memory part
# trials.thisIndex:       number of shot
# corrAnsw:               correct answers in video memory part
# corrAnsIM:              correct answers in image memory part
# IM_key.keys:            button pressed by the participants to answer image memory part
# IM_key.corr:            participants accuracy in image memory part
# IM_key.rt:              participants reaction time in image memory part


######        Variable names explanation:             #######
# Note: Video memory and image memory, which were decided as analysis names at the beginning 
# of the article, were later changed to shot memory and spatial orientation memory respectively.

# dprimes:               data frame for d prime scores, criterion c scores, z-scores, means, standard deviations, standart errors for video memory data
# VDdprime:              2*2 mixed design analysis of variance for d prime scores
# VDrt:                  2*2 mixed design analysis of variance for video memory reaction time
# partialE_cond_vd_d:    partial eta square for condition variable on video memory d prime scores
# partialE_Pers_vd_d:    partial eta square for perspective variable on video memory d prime scores
# partialE_inter_vd_d:   partial eta square for interaction on video memory d prime scores
# partialE_cond_vd_rt:   partial eta square for condition variable on video memory reaction times
# partialE_Pers_vd_rt:   partial eta square for perspective variable on video memory reaction times
# partialE_inter_vd_rt:  partial eta square for interaction on video memory reaction times
# Image_M:               data frame for means, standard deviations, standard errors for image memory data
# AccIM:                 2*2 mixed design analysis of variance for image memory accuracies
# partialE_cond_acc_IM:  partial eta square for condition variable on image memory accuracies
# partialE_Pers_acc_IM:  partial eta square for perseption variable on image memory accuracies
# partialE_inter_acc_IM: partial eta square for interaction on image memory accuracies
# IMrt:                  2*2 mixed design analysis of variance for image memory reaction times
# partialE_cond_rt_IM:   partial eta square for condition variable on image memory reaction times
# partialE_Pers_rt_IM:   partial eta square for perseption variable on image memory reaction times
# partialE_inter_rt_IM:  partial eta square for interaction on image memory reaction times
# Awareness:             data frame for awareness in post questionnaire 


# load/ install required packages:
packages= c("tidyverse", "ggpubr", "ez", "ggplot2", "readxl","stringr","readr","rstatix","qqplotr",
            "DescTools") 

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

## Please note, Since the z-score of the 16th participant's reaction time in two perspectives was higher 
## than 3.92, it was excluded from the data as outlier.

####    Video Memory    ####


source("prec.R")
raw_data = preproc(data_dir = "Results")

save(raw_data, file= "raw_data.Rda")

#or
load("raw_data.Rda")

# n_hit/n_miss/n_fa/n_cr & d'primes

dprimes <- raw_data %>% group_by(Participant,Perspective,Condition) %>% summarise(
                                                                        n_hit = sum(movie_resp.corr == 1 & Shot_Type == "test shot"),
                                                                        n_miss = sum(movie_resp.corr == 0 & Shot_Type == "test shot"),
                                                                        n_fa = sum(movie_resp.corr == 0 & Shot_Type == "distractor shot"),
                                                                        n_cr = sum(movie_resp.corr == 1 & Shot_Type == "distractor shot")
                                                                        ) %>% 
  group_by(Participant,Perspective) %>% 
  mutate(hitrate = (n_hit+0.5)/((n_hit+0.5)+(n_miss+0.5)), farate = (n_fa+0.5)/((n_fa+0.5)+(n_cr+0.5))) %>% 
  mutate(zscoreHit = qnorm(hitrate), zscoreFA = qnorm(farate)) %>% 
  mutate(dprime = zscoreHit - zscoreFA) %>% 
  mutate(CriterionC = -0.5*(zscoreHit+zscoreFA))

dprimes1 <- raw_data %>% group_by(Participant,Perspective) %>% summarise(meanrt = mean(movie_resp.rt))
dprimes <- dprimes %>% left_join(dprimes1,by=c("Participant","Perspective"))

# Contrasting
dprimes$Condition <- as.factor(dprimes$Condition)
levels(dprimes$Condition)
contrasts(dprimes$Condition)

dprimes$Perspective <- factor(dprimes$Perspective,levels= c("Single", "Shot-Reverse-Shot"))
levels(dprimes$Perspective)
contrasts(dprimes$Perspective)

#### ANOVAs ####

# D prime
VDdprime<-ezANOVA(data = dprimes,
        dv = dprime,
        wid=Participant,
        within = Perspective,
        between = Condition,
        type = 3,
        detailed = TRUE,
        return_aov = TRUE); VDdprime

# Partial Eta Squares
partialE_cond_vd_d <- VDdprime$ANOVA$SSn[2] / (VDdprime$ANOVA$SSd[2] + VDdprime$ANOVA$SSn[2])
partialE_Pers_vd_d <- VDdprime$ANOVA$SSn[3] / (VDdprime$ANOVA$SSd[3] + VDdprime$ANOVA$SSn[3])
partialE_inter_vd_d <- VDdprime$ANOVA$SSn[4] / (VDdprime$ANOVA$SSd[4] + VDdprime$ANOVA$SSn[4])

# RT
VDrt<-ezANOVA(data = dprimes,
           dv = meanrt,
           wid=Participant,
           within = Perspective,
           between = Condition,
           type = 3,
           detailed = TRUE,
           return_aov = TRUE);VDrt

# Partial Eta Squares
partialE_cond_vd_rt <- VDrt$ANOVA$SSn[2] / (VDrt$ANOVA$SSd[2] + VDrt$ANOVA$SSn[2])
partialE_Pers_vd_rt <- VDrt$ANOVA$SSn[3] / (VDrt$ANOVA$SSd[3] + VDrt$ANOVA$SSn[3])
partialE_inter_vd_rt <- VDrt$ANOVA$SSn[4] / (VDrt$ANOVA$SSd[4] + VDrt$ANOVA$SSn[4])



####    Image Memory    ####

source("precIM.R")
raw_dataIM = precIM(data_dir = "Results")

save(raw_dataIM, file= "raw_dataIM.Rda")


# or
load("raw_dataIM.Rda")

 
# Accuracy Calculation
Image_M <- raw_dataIM %>% group_by(Participant,Perspective,Condition,Gender,Age) %>% summarise(accuracy_M = mean(IM_key.corr),
                                                                          rt_M = mean(IM_key.rt),
                                                                          rt_sd = sd(IM_key.rt)
                                                                          )

# Contrasting
Image_M$Condition <- as.factor(Image_M$Condition)
levels(Image_M$Condition)
contrasts(Image_M$Condition)

Image_M$Perspective <- factor(Image_M$Perspective, levels= c("Single", "Shot-Reverse-Shot"))
levels(Image_M$Perspective)
contrasts(Image_M$Perspective)

#### ANOVAs ####

# Accuracy 
AccIM<-ezANOVA(data=Image_M,
        dv = accuracy_M,
        wid=Participant,
        within = Perspective,
        between = Condition,
        detailed = TRUE,
        return_aov = TRUE,
        type=3)

# Partial Eta Squares
partialE_cond_acc_IM <- AccIM$ANOVA$SSn[2] / (AccIM$ANOVA$SSd[2] + AccIM$ANOVA$SSn[2])
partialE_Pers_acc_IM <- AccIM$ANOVA$SSn[3] / (AccIM$ANOVA$SSd[3] + AccIM$ANOVA$SSn[3])
partialE_inter_acc_IM <- AccIM$ANOVA$SSn[4] / (AccIM$ANOVA$SSd[4] + AccIM$ANOVA$SSn[4])

# RT
IMrt<-ezANOVA(data=Image_M,
        dv = rt_M,
        wid=Participant,
        within = Perspective,
        between = Condition,
        detailed = TRUE,
        return_aov = TRUE,
        type=3)

# Partial Eta Squares
partialE_cond_rt_IM <- IMrt$ANOVA$SSn[2] / (IMrt$ANOVA$SSd[2] + IMrt$ANOVA$SSn[2])
partialE_Pers_rt_IM <- IMrt$ANOVA$SSn[3] / (IMrt$ANOVA$SSd[3] + IMrt$ANOVA$SSn[3])
partialE_inter_rt_IM <- IMrt$ANOVA$SSn[4] / (IMrt$ANOVA$SSd[4] + IMrt$ANOVA$SSn[4])


#### Post-experiment Questions ####

Awareness <- read_excel("Awareness.xlsx")
View(Awareness)

Awareness <- Awareness %>% group_by(AwarenessType,Question) %>% mutate(means = (sum(First,na.rm = TRUE)/62)*100)

#### Visualizations ####

dprimes <- dprimes %>% group_by(Perspective,Condition) %>% mutate(meand = mean(dprime),
                                                                   sd = sd(dprime),
                                                                   n = n())
                                                                
dprimes$se = dprimes$sd/sqrt(dprimes$n)

Image_M <- Image_M %>% group_by(Perspective,Condition) %>% mutate( accuracy_M_M = mean(accuracy_M),
                                                         sdAcc = sd(accuracy_M),
                                                         n = n(),
                                                         rt_M_M = mean(rt_M),
                                                         sdRT_sd = sd(rt_M))
Image_M$se = Image_M$sdAcc/sqrt(Image_M$n)


# D'scores
bargraph_dprime = ggplot(dprimes %>% group_by(Perspective,Condition), aes(y=dprime, x=Perspective, fill = Condition, ymax= meand + se, ymin= meand - se))+
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position=position_dodge(0.7),
               width = 0.6) + 
  geom_errorbar(position = position_dodge(0.7), 
                width = 0.2)+
  scale_fill_manual(name="Order", values = c("#666666", "#999999"),
                    labels = c("Canonical", "Scrambled")) + 
  xlab("Perspective") +
  ylab("D prime scores") + 
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA),
        legend.position = "top",
        #legend.position = c(0.7,0.86),
        legend.box = "vertical",
        aspect.ratio = 1) +
  coord_cartesian(ylim = range(0:2.5)) +
  scale_y_continuous(expand = c(0,0.005))

bargraph_dprime<-bargraph_dprime +
  geom_line(data = tibble(x=c(1,2), y = c(1.75,1.75)), aes(x=x,y=y), 
           inherit.aes = F)+
  geom_text(data = tibble(x=1.5, y = 1.8), aes(x=x,y=y), 
            inherit.aes = F,label = "* * *",size = 5)+
  geom_line(data = tibble(x=c(1,1), y = c(1.75,1.65)), aes(x=x,y=y), 
            inherit.aes = F)+
  geom_line(data = tibble(x=c(2,2), y = c(1.75,1.65)), aes(x=x,y=y), 
            inherit.aes = F);bargraph_dprime

# Image Memory
bargraph_image_m = ggplot(Image_M %>% group_by(Perspective,Condition), aes(y=accuracy_M, x=Perspective, fill = Condition,ymax= accuracy_M_M + se, ymin= accuracy_M_M - se))+
  coord_cartesian(ylim = c(0.5,1))+
  stat_summary(fun = mean,
               geom = "bar",
               position=position_dodge(0.7),
               width = 0.6) +
  geom_errorbar(position = position_dodge(0.7),
                width = 0.2)+
  scale_fill_manual(name="Order", values = c("#666666", "#999999"),
                    labels = c("Canonical", "Scrambled")) + 
  xlab("Perspective") +
  ylab("% of Accuracy") + 
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA),
        #legend.position = c(0.7,0.86),
        legend.position = "top",
        legend.box = "vertical",
        aspect.ratio = 1)+
  scale_y_continuous(expand = c(0,0.004),);bargraph_image_m 

bargraph_image_m <- bargraph_image_m + 
  geom_line(data = tibble(x=c(1,2), y = c(0.97,0.97)), aes(x=x,y=y), 
            inherit.aes = F)+
  geom_text(data = tibble(x=1.5, y = 0.974), aes(x=x,y=y), 
            inherit.aes = F,label = "* * *",size = 5)+
  geom_line(data = tibble(x=c(1,1), y = c(0.97,0.955)), aes(x=x,y=y), 
            inherit.aes = F)+
  geom_line(data = tibble(x=c(2,2), y = c(0.97,0.955)), aes(x=x,y=y), 
            inherit.aes = F);bargraph_image_m

# Awareness
bargraph_awareness = ggplot(Awareness %>% group_by(AwarenessType,Question), aes(y=means, x=AwarenessType, fill = as.factor(Question)))+
  geom_bar(position=position_dodge(1), stat="identity",width = 0.9)+
  scale_fill_manual(name="Question", values = c("#999999", "#666666","#333333"),
                    labels = c("first", "second","third")) + 
  xlab("Awareness Type") +
  ylab("% of Participants") + 
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA),
        #legend.position = c(0.8,0.8),
        legend.position = "top",
        aspect.ratio = 1) + 
  coord_cartesian(ylim = range(0:100)) +
  scale_y_continuous(expand = c(0,0.4))+
  geom_text(aes(label=round(means,2)), position=position_dodge(width=1), vjust=2,col = "white");bargraph_awareness

all_figures <- ggarrange(bargraph_dprime, bargraph_image_m,bargraph_awareness, 
                         labels = c("A)", "B)","C)"),
                         ncol = 3, nrow = 1);all_figures

ggsave(bargraph_dprime, file="Figures/Fig3.png",height = 120 ,width = 110,units = "mm", dpi=300) 
ggsave(bargraph_image_m, file="Figures/Fig4.png",height = 120 ,width = 110,units = "mm", dpi=300) 
ggsave(bargraph_awareness, file="Figures/Fig5.png",height = 120 ,width = 110,units = "mm", dpi=300) 

