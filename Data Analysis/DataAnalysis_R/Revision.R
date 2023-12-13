# Zeynep G. Özkan, Ayşe C. Şimşek, Tolgahan Aydın, 2023

rm(list= ls())

# This R script contains descriptive analysis performed at the request of reviewers.

# Note: Video memory and image memory, which were decided as analysis names at the beginning 
# of the article, were later changed to shot memory and spatial orientation memory respectively.

# load/ install required packages:
packages= c("tidyverse", "ggpubr", "ez", "ggplot2", "readxl","readr","rstatix","qqplotr",
            "DescTools") 

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

load("raw_data.Rda")
load("raw_dataIM.Rda")

#### Gender & Age
dprimes_gender <- raw_data %>% group_by(Participant,Gender,Age) %>% summarise(
  n_hit = sum(movie_resp.corr == 1 & Shot_Type == "test shot"),
  n_miss = sum(movie_resp.corr == 0 & Shot_Type == "test shot"),
  n_fa = sum(movie_resp.corr == 0 & Shot_Type == "distractor shot"),
  n_cr = sum(movie_resp.corr == 1 & Shot_Type == "distractor shot")) %>% 
  group_by(Participant) %>% 
  mutate(hitrate = (n_hit+0.5)/((n_hit+0.5)+(n_miss+0.5)), farate = (n_fa+0.5)/((n_fa+0.5)+(n_cr+0.5))) %>% 
  mutate(zscoreHit = qnorm(hitrate), zscoreFA = qnorm(farate)) %>% 
  mutate(dprime = zscoreHit - zscoreFA) %>% 
  mutate(CriterionC = -0.5*(zscoreHit+zscoreFA))

dprimes_gender1 <- raw_data %>% group_by(Participant) %>% summarise(meanrt = mean(movie_resp.rt))
dprimes_gender <- dprimes_gender %>% left_join(dprimes_gender1,by=c("Participant"))

#### Assumption check ####
dprimes_gender$Gender <- ifelse(dprimes_gender$Gender == "Kadın","Female","Male")

dprimes_gender$Gender <- as.factor(dprimes_gender$Gender)
levels(dprimes_gender$Gender)
contrasts(dprimes_gender$Gender)
##### Mean RT #####

# Outlier 
dprimes_gender %>%
  group_by(Gender) %>%
  identify_outliers(meanrt)

# Shapiro wilk test
dprimes_gender %>% group_by(Participant) %>%
  group_by(Gender) %>%
  summarise(`W Stat` = shapiro.test(meanrt)$statistic,
            p.value = shapiro.test(meanrt)$p.value)

# QQ plot 
qq1 <- ggplot(data = dprimes_gender %>% group_by(Participant), mapping = aes(sample = meanrt, color = Gender, fill = Gender)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Gender, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# Equal variances
resRT <- var.test(meanrt ~ Gender, data = dprimes_gender)
resRT

##### d primes #####

# Outlier 
dprimes_gender %>%
  group_by(Gender) %>%
  identify_outliers(dprime)

# Shapiro wilk test
dprimes_gender %>% group_by(Participant) %>%
  group_by(Gender) %>%
  summarise(`W Stat` = shapiro.test(dprime)$statistic,
            p.value = shapiro.test(dprime)$p.value)

# QQ plot 
qq2 <- ggplot(data = dprimes_gender %>% group_by(Participant), mapping = aes(sample = dprime, color = Gender, fill = Gender)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Gender, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# Equal variances
res_d <- var.test(dprime ~ Gender, data = dprimes_gender)
res_d

##### Image memory accuracy #####

Image_M_gender <- raw_dataIM %>% group_by(Participant,Gender,Age) %>% summarise(accuracy_M = mean(IM_key.corr),
                                                                                               rt_M = mean(IM_key.rt),
                                                                                               rt_sd = sd(IM_key.rt))

Image_M_gender$Gender <- ifelse(Image_M_gender$Gender == "Kadın","Female","Male")
Image_M_gender$Gender <- as.factor(Image_M_gender$Gender)
levels(Image_M_gender$Gender)
contrasts(Image_M_gender$Gender)

# Outlier 
Image_M_gender %>%
  group_by(Gender) %>%
  identify_outliers(accuracy_M)

# Shapiro wilk test
Image_M_gender %>% group_by(Participant) %>%
  group_by(Gender) %>%
  summarise(`W Stat` = shapiro.test(accuracy_M)$statistic,
            p.value = shapiro.test(accuracy_M)$p.value)

# QQ plot 
qq3 <- ggplot(data = Image_M_gender %>% group_by(Participant), mapping = aes(sample = accuracy_M, color = Gender, fill = Gender)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Gender, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# Equal variances
res_Acc <- var.test(accuracy_M ~ Gender, data = Image_M_gender)
res_Acc

##### Image memory RT #####

# Outlier 
Image_M_gender %>%
  group_by(Gender) %>%
  identify_outliers(rt_M)

# Shapiro wilk test
Image_M_gender %>% group_by(Participant) %>%
  group_by(Gender) %>%
  summarise(`W Stat` = shapiro.test(rt_M)$statistic,
            p.value = shapiro.test(rt_M)$p.value)

# QQ plot 
qq4 <- ggplot(data = Image_M_gender %>% group_by(Participant), mapping = aes(sample = rt_M, color = Gender, fill = Gender)) +
  stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot") +
  stat_qq_line(identity=TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ Gender, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()

# Equal variances
res_rtİm <- var.test(rt_M ~ Gender, data = Image_M_gender)
res_rtİm


all_qqs <- ggarrange(qq1, qq2, qq3, qq4 + rremove("x.text"), 
                         labels = c("A)", "B)","C)","D)"),
                         ncol = 2, nrow = 2, legend= "none");all_qqs

#### Anaylsis ####

##### Mann-Whitney U tests and t-test #####

H1<-wilcox.test(meanrt ~ Gender, data=dprimes_gender, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H1)

H2<-wilcox.test(dprime ~ Gender, data=dprimes_gender, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H2)

H3<-wilcox.test(accuracy_M ~ Gender, data=Image_M_gender, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H3)

H4<-wilcox.test(rt_M ~ Gender, data=Image_M_gender, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(H4)

## Age correlations

# video memory
# d primes

cor.test(dprimes_gender$dprime, dprimes_gender$Age)

# meanrt

cor.test(dprimes_gender$meanrt, dprimes_gender$Age)

# Image memory
# accuracy

cor.test(Image_M_gender$accuracy_M, Image_M_gender$Age)

# rt

cor.test(Image_M_gender$rt_M, Image_M_gender$Age)

# Activity

dprimes_act <- raw_data %>% group_by(Participant,Perspective,Condition,Activity) %>% summarise(
  n_hit = sum(movie_resp.corr == 1 & Shot_Type == "test shot"),
  n_miss = sum(movie_resp.corr == 0 & Shot_Type == "test shot"),
  n_fa = sum(movie_resp.corr == 0 & Shot_Type == "distractor shot"),
  n_cr = sum(movie_resp.corr == 1 & Shot_Type == "distractor shot")
) %>% 
  group_by(Participant,Activity) %>% 
  mutate(hitrate = (n_hit+0.5)/((n_hit+0.5)+(n_miss+0.5)), farate = (n_fa+0.5)/((n_fa+0.5)+(n_cr+0.5))) %>% 
  mutate(zscoreHit = qnorm(hitrate), zscoreFA = qnorm(farate)) %>% 
  mutate(dprime = zscoreHit - zscoreFA) %>% 
  mutate(CriterionC = -0.5*(zscoreHit+zscoreFA))

dprimes1 <- raw_data %>% group_by(Participant,Perspective,Condition,Activity) %>% summarise(meanrt = mean(movie_resp.rt))
dprimes_act <- dprimes_act %>% left_join(dprimes1,by=c("Participant","Perspective","Condition","Activity"))


Image_Mact <- raw_dataIM %>% group_by(Participant,Perspective,Condition,Gender,Age,Activity) %>% summarise(accuracy_M = mean(IM_key.corr),
                                                                                                                 rt_M = mean(IM_key.rt),
                                                                                                                 rt_sd = sd(IM_key.rt))

## One way ANOVAs ##

Image_Mact$Activity <- as.factor(Image_Mact$Activity)
Image_Mact$Participant <- as.factor(Image_Mact$Participant)
levels(Image_Mact$Activity)
contrasts(Image_Mact$Activity)

## Image memory
# accuracy

model<-ezANOVA(data=Image_Mact,
              dv = accuracy_M,
              wid=Participant,
              within = Activity,
              detailed = TRUE,
              return_aov = TRUE,
              type=3);model
# RT

model<-ezANOVA(data=Image_Mact,
               dv = rt_M,
               wid=Participant,
               within = Activity,
               detailed = TRUE,
               return_aov = TRUE,
               type=3);model

## Video memory
#dprime

dprimes_act$Activity <- as.factor(dprimes_act$Activity)
levels(dprimes_act$Activity)
contrasts(dprimes_act$Activity)

model<-ezANOVA(data=dprimes_act,
               dv = dprime,
               wid=Participant,
               within = Activity,
               detailed = TRUE,
               return_aov = TRUE,
               type=3);model

# RT
model<-ezANOVA(data=dprimes_act,
               dv = meanrt,
               wid=Participant,
               within = Activity,
               detailed = TRUE,
               return_aov = TRUE,
               type=3);model


#### Descriptive group sizes

sample_size <- raw_dataIM %>% group_by(Condition) %>% summarise(num_participants = n_distinct(Participant))







