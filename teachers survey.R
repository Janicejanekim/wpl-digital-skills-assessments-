suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(mudata2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library("rstudioapi"))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(janitor))

setwd(dirname(getActiveDocumentContext()$path))

###
skills_assessment<-read_excel("Digital Literacy Skills Assessment v4.xlsx",sheet = "Digital Literacy Skills Assessm")

skills_assessment<-skills_assessment%>%
  rename("role"="(Role) What is your role?","Gender"="(Gender) What is your gender?","school"="(School) What is the name of your school ?" ,
         "survey"="(Survey) How are you completing this survey? (If you choose printed posters the large images may be hidden)",
         "own devices"= "(Own devices) What devices do you or your family own?",
         "oftenly use Devices"="(Devices) How often do you use digital devices?","online"="(Online) How often do you use the Internet?",
         "Reason for buying device"= "(Device) If you bought a new mobile phone or tablet - what would be the most important reason you would buy it?",
         "use for internet data"="(Data) If you had alot of data- what would you prefer to spend it on?",
         "Barriers"="(Barriers) What are the two biggest problems stopping you from using online content?",
         "Devices"= "(Devices) DEVICES: In using computers, phones, or tablets,  I am a digital:",
         "Devices barriers"="(Devices barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?",
         "work productivity"="(Work) PRODUCTIVITY In my work and daily productivity tasks, I am a digital:",
         "work Barriers"="(Work- barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?",
         "APPs"= "(Apps) APPS: In using digital apps, I am a digital:","APPs barriers"="(Apps - barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?",
         "Entertainment"="(Entertainment) ENTERTAINMENT: In using digital entertainment, I am a digital:",
         "Entertainment barriers"="(Entertainment - barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?",
         "communities"="(Communities) COMMUNITIES: In online communities, I am a digital:","communities barriers"= "(Communities- barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?",
         "Education"="(Education) EDUCATION: In education, I am a digital:",
         "Education barriers"="(Education - barriers) What would you like to learn next, and what problems or barriers stop you from improving your digtial skills in this area?","feedback"= "(Comments) Please provide additional feedback about this survey.")


skills_assessment<-skills_assessment%>%
  mutate(school=case_when(school=="Badbado Abe Centre"~"Badbado Abe Centre",school == "BADBADO ABE CENTRE"~"Badbado Abe Centre",school == "Barbado Abe centre"~"Badbado Abe Centre",school=="Badbaado ABE centre"~"Badbado Abe Centre",
                          school=="Badbado"~"Badbado Abe Centre",school=="Badbaado ABE"~"Badbado Abe Centre",school=="Badbadho"~"Badbado Abe Centre",school=="Badbado ABE centre"~"Badbado Abe Centre",school=="Libyan ABE centre"~"Liban Abe centre",
                          school=="Badbado ABE center"~"Badbado Abe Centre",school=="Liban Abe  Centre"~"Liban Abe centre",school=="Liban Abe centre"~"Liban Abe centre",school=="Liban"~"Liban Abe centre",school=="Liban ABE center"~"Liban Abe centre",
                          school=="Badbad ABE"~"Badbado Abe Centre",school=="Badbado ABE Centre"~"Badbado Abe Centre",school=="Unity ABE centre"~"Unity Abe centre"))
###Gender
Gender<-skills_assessment%>%
  select(Gender,school,survey,`own devices`)%>%
  group_by(Gender)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))
ggplot(Gender,aes(x = Gender,y = freq,fill = Gender,label=scales::percent(freq))) +
  geom_bar(position="dodge",stat="identity",width = 0.3) +
  scale_fill_manual(values = c("lightsalmon2","darkturquoise"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "gender",
       title = "Percentage of respondents by gender")+
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 20),
        axis.text.x = element_text(color = "black", size = 10,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 10,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))
ggsave("gender.png")

###school by gender
school<-skills_assessment%>%
  select(Gender,school,survey,`own devices`)%>%
  group_by(school)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))
ggplot(school,aes(x = school,y = freq,fill = school,label=scales::percent(freq))) +
  geom_bar(position="dodge",stat="identity",width = 0.3) +
  scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "school",
       title = "Percentage of schools which participated")+
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 20),
        axis.text.x = element_text(color = "black", size = 10,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 15,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 15,face = "bold"),
        legend.text = element_text(color = "black", size = 10,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5))
ggsave("school.png")
#####
Gender_school1<-skills_assessment%>%
  select(Gender,school,survey,`own devices`)%>%
  tabyl(school,Gender)

####Gender-school
   Gender_school<-skills_assessment%>%
   select(Gender,school,survey,`own devices`)%>%
   group_by(Gender,school)%>%
   summarise(cnt = n())%>%
   mutate(freq = round(cnt / sum(21),2)) %>% 
   arrange(desc(freq))
  
   
   ggplot(Gender_school,aes(x = school,y = freq,fill = school,label=scales::percent(freq))) +
     geom_bar(position="dodge",stat="identity",width = 0.3) +
     scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2"))+
     scale_y_continuous(labels = scales::percent,
                        breaks = scales::pretty_breaks(n = 7))+
     facet_wrap(~Gender)+
     geom_text(nudge_y= .01,
               color="black",
               size = 4,
               fontface="bold")+
     labs(x = "school",
          title = "Percentage number of teachers grouped by Gender and school")+
     theme(panel.spacing = unit(2, "lines"),
           strip.text.x = element_text(size = 12),
           axis.text.x = element_blank(),
           axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
           axis.title.x = element_text(colour="black", size = 15,face = "bold"),
           axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
           legend.title = element_text(color = "black", size = 10,face = "bold"),
           legend.text = element_text(color = "black", size = 10,face = "bold"),
           plot.title = element_text(face = "bold",hjust = 0.5,size = 9))
ggsave("gender_school.png")

####survey
survey<-skills_assessment%>%
  select(Gender,school,survey,`own devices`)%>%
  group_by(survey)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))

ggplot(survey,aes(x = survey,y = freq,fill = survey,label=scales::percent(freq))) +
  geom_bar(position="dodge",stat="identity",width = 0.3) +
  scale_fill_manual(values = c("chartreuse2","mediumseagreen"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "How they finished the survey",
       title = "Percentage of teachers on how they finished the survey")+
  theme(legend.position = "right",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 10,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 7,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.4,size = 9))
ggsave("survey.png")
  
  ###devices owned

Gender_devices<-skills_assessment%>%
  select(Gender,school,survey,`own devices`)%>%
  tabyl(`own devices`,Gender)%>%
  adorn_title()

#####


# 
# wpl<-skills_assessment%>%
#   select(Gender,school,survey,`own devices`,`oftenly use Devices`,online)%>%
#   pivot_longer(!c(Gender,school), names_to = "reason_cycling", values_to = "Responses")%>%
#   group_by(`own devices`)%>%
#   summarise(cnt = n())%>%
#   mutate(freq = round(cnt / sum(21),2)) %>% 
#   arrange(desc(freq))

####
  Devices<-skills_assessment%>%
    select(Gender,school,survey,`own devices`)%>%
    group_by(`own devices`)%>%
    summarise(cnt = n())%>%
    mutate(freq = round(cnt / sum(21),2)) %>% 
    arrange(desc(freq))
  ggplot(Devices,aes(x = `own devices`,y = freq,fill = `own devices`,label=scales::percent(freq)))+
    geom_bar(position="dodge",stat="identity",width = 0.4) +
    scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                                 "darkturquoise","darkgoldenrod2"))+
    scale_y_continuous(labels = scales::percent,
                       breaks = scales::pretty_breaks(n = 7))+
    geom_text(nudge_y= .01,
              color="black",
              size = 4,
              fontface="bold")+
    labs(x = "Devices",
         title = "percentages of devices owned")+
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"),
          strip.text.x = element_text(size = 10),
          axis.text.x = element_text(color = "black", size = 10,angle = 90,face = "bold"),
          axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
          axis.title.x = element_text(colour="black", size = 10,face = "bold"),
          axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
          legend.title = element_text(color = "black", size = 5,face = "bold"),
          legend.text = element_text(color = "black", size = 7,face = "bold"),
          plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
ggsave("devices.png")
#####

####
Devices_use<-skills_assessment%>%
  select(Gender,school,survey,`own devices`,`oftenly use Devices`,online,`Reason for buying device`)%>%
  group_by(`own devices`,online)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))

ggplot(Devices_use,aes(x = `own devices`,y = freq,fill = `own devices`,
                       label=scales::percent(freq)))+
  geom_bar(position="dodge",stat="identity",width = 0.4)+
  scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                               "darkturquoise","darkgoldenrod2"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  facet_wrap(~online)+
  labs(x = "Devices",
       title = "percentages of devices owned grouped by usage ")+
  theme(legend.position = "right",
        strip.text.x = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 5,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
####
reason_buying_device<-skills_assessment%>%
  select(Gender,`own devices`,`oftenly use Devices`,online,`Reason for buying device`,`use for internet data`)%>%
  group_by(`use for internet data`)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))


ggplot(reason_buying_device,aes(x =`use for internet data`,y = freq,fill = `use for internet data`,
                       label=scales::percent(freq)))+
  geom_bar(position="dodge",stat="identity",width = 0.4)+
  scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                               "darkturquoise","darkgoldenrod2"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "Devices",
       title = "Usage for internet")+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 10),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(color = "black", size = 7,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 5,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
ggsave("usage.png")
#####
library(RColorBrewer)
data<-skills_assessment%>%
  select(Gender,`own devices`,`oftenly use Devices`,online,`Reason for buying device`,`use for internet data`)%>%
  group_by(`use for internet data`)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))
ggplot(data,aes(x =`use for internet data`,y = freq,fill = `use for internet data`,
                                label=scales::percent(freq)))+
  geom_bar(position="dodge",stat="identity",width = 0.4)+
  scale_fill_manual(values = c("mediumseagreen","chartreuse2","chocolate2",
                               "darkturquoise","darkgoldenrod2"))+
scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "Devices",
       title = "Usage for internet")+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 10),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(color = "black", size = 7,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 5,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
ggsave("data.png")
#####

# 
# Barriers<-skills_assessment%>%
#   select(Barriers)%>%
#   mutate(Barriers,paying_for_internet=case_when(Barriers=="Paying for Internet is too much money for me"~1,Barriers=="Not enough power (charge)\r\nPaying for Internet is too much money for me"~1))
#   summarise(cnt = n())%>%
#   mutate(freq = round(cnt / sum(21),2)) %>% 
#   arrange(desc(freq))%>%
#   ungroup()

####
digital<-skills_assessment%>%
  select(Gender,Devices,`Devices barriers`)%>%
  group_by(Gender,Devices)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))


ggplot(digital,aes(x=Devices,y = freq,fill = Devices,
                label=scales::percent(freq)))+
  geom_bar(position="dodge",stat="identity",width = 0.4)+
  scale_fill_manual(values = c("mediumseagreen","chartreuse2",
                               "darkturquoise"))+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 7))+
  facet_wrap(~Gender)+
  geom_text(nudge_y= .01,
            color="black",
            size = 4,
            fontface="bold")+
  labs(x = "Devices",
       title = "percentage of teachers on their level on using devices by gender")+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 10),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(color = "black", size = 7,angle = 0,face = "bold"),
        axis.text.y = element_text(color="black", size = 10, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 15,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 5,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
ggsave("digital.png")  
####  
Work<-skills_assessment%>%
  select(Gender,Devices,`work productivity`,APPs,Entertainment,communities,Education)%>%
  pivot_longer(!c(Gender), names_to = "categories",values_to = "levels")%>%
  group_by(categories,levels)%>%
  summarise(cnt = n())%>%
  mutate(freq = round(cnt / sum(21),2)) %>% 
  arrange(desc(freq))

  

ggplot(Work,aes(x=levels,y = freq,fill = levels,
                   label=scales::percent(freq)))+
  geom_bar(stat="identity",width = 0.5)+
  display.brewer.pal(n = 6, name = 'PuBu')+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~categories)+
  geom_text(nudge_y= .01,
            color="black",
            size = 2.5,
            fontface="bold")+
  labs(x = "levels",
       title = "percentage of teachers level as i am digital in different categories")+
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 9),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="black", size = 6, angle = 0,face = "bold"),
        axis.title.x = element_text(colour="black", size = 10,face = "bold"),
        axis.title.y = element_text(colour="black", size = 10,face = "bold",hjust = 0.5),
        legend.title = element_text(color = "black", size = 10,face = "bold"),
        legend.text = element_text(color = "black", size = 7,face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 10))
ggsave("digital.png")


