# MSE Problem Framing 
# Literature Review Data Analysis
# Create figures
# 8/20/2019, JWC

##### Meta Data and setup #####
# Description: Import and analyze MSE documentation from database
# Value: Create figures to communicate study results

##### load libraries #####
library(tidyverse) # upgrade to base R
library(readxl) # read in excel files
library(shiny) # Shiny App
library(shinythemes) # web themes for Shiny
library(DT) # Data Table
library(purrr) # for loop alternative
library(ggmap) # map plotting
library(mapdata) # basemap generation
library(odbc) # database connection 
library(RMySQL) # MySQL scripting in R
library(DBI) # database interface
library(dbplyr) # database interface for plyr
library(kableExtra) # extra formating of the tables
library(here) # file directory assistance

###### To open a connection to the database: #####
# Copy into command prompt:
## cloud_sql_proxy -instances=alpine-tracker-230222:us-east1:mse-review=tcp:3306

# ##### Connect to database and get data #####
# # Create connection function
# getSqlConnection <- function(){
#   cn <-
#     dbConnect(
#       RMySQL::MySQL(),
#       dbname = 'MSEreview',
#       host = '127.0.0.1',
#       port=3306,
#       username = 'root',
#       password = '2u8aNDdur1aFdb8z')
#   return(cn)
# }
# # Open connection
# con <- getSqlConnection()
# 
# # load("C:/Users/jcummings/OneDrive - UMASS Dartmouth/SMAST/Research/MSE Problem Framing/MSEreview/MSEreview.RData")
# 
# # Obtain data tables
# study<-dbReadTable(con,"tblStudy")
# # study<-read_xlsx("DB files - Excel/tblStudy.xlsx")
# mgmt<-dbReadTable(con,"tblStudyManagement")
# # mgmt<-read_xlsx("DB files - Excel/tblStudyManagementTools.xlsx")
# # mgmt$fkStudyID<-parse_integer(mgmt$fkStudyID)
# obj<-dbReadTable(con,"tblStudyObjectives")
# # obj<-read_xlsx("DB files - Excel/tblStudyObjectives.xlsx")
# # obj$fkStudyID<-parse_integer(obj$fkStudyID)
# fields<-dbReadTable(con,"tblStudyFields")
# # fields<-read_xlsx("DB files - Excel/tblStudyFields.xlsx")
# WOS<-dbReadTable(con,"tblWOS")
# 
# # To close the database connection
# dbDisconnect(con)

load(here("data/MSEreview.RData"))

##### Edit and join tables #####
# Edit tables for joining
load(here("data/MSEreview.RData"))

study<-study %>% 
  unite(Citation,c(Authors,YearPub),sep=" ",remove=F)

mgmt.join<-mgmt %>% 
  group_by(fkStudyID) %>%
  summarise(ManagementType = toString(sort(unique(MPManagementTool))),
            AlternativesEvaluated = toString(sort(unique(MPAlternativesEvaluated))))

obj.join<-obj %>%
  group_by(fkStudyID) %>%
  summarise(ObjectiveCategories = toString(sort(unique(ObjCategory))))

# Join tables
data<-full_join(study,mgmt.join,by=c("ID"="fkStudyID"))
data<-full_join(data,obj.join,by=c("ID"="fkStudyID"))

# Move comment column to the end
data<-data %>%
  select(-'Comments', 'Comments')

# Get columns whose width needs editing
targets<-match(c("FullCitation","Comments"),names(data))

#####-- Data analysis --#####
# Get columns for study summary
# Filter data by study type
data_pub<-filter(data,RandomSample==TRUE)
data_CC<-filter(data,str_detect(Drivers,"Climate Change")&UseInPublication==TRUE)

data_pub.join<-data_pub %>%
  select(ID,Citation)
data_CC.join<-data_CC %>%
  select(ID,Citation)

obj.data_pub<-left_join(data_pub.join,obj,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))
obj.data_CC<-left_join(data_CC.join,obj,by=c("ID"="fkStudyID"))%>%
  select(-c("ID","ID.y"))

alt.data_pub<-left_join(data_pub.join,mgmt.join,by=c("ID"="fkStudyID"))%>%
  select(-c("ID"))
alt.data_CC<-left_join(data_CC.join,mgmt.join,by=c("ID"="fkStudyID"))%>%
  select(-c("ID"))

data_pub<-data_pub %>%
  select(-c("ID"))
data_CC<-data_CC %>%
  select(-c("ID"))

# Get columns whose width needs editing
targets<-match(c("FullCitation","Comments"),names(data))

#####-- Filter Data by analysis --#####
# Get columns for study summary
summary.col<-c("Citation",
               "Species",
               "Location",
               "System")
# Get columns for study drivers and problem
prob.col<-c("Citation",
            "ProblemDefinition",
            "Drivers",
            "ConsequencePrediction",
            "TradeOffMethod_Exp",
            "TradeOffMethod_Sub",
            "Decision")
# Get columns for frequency analysis
freq.col<-c("ProcessExplicit",
            "ProblemDefinitionExplicit",
            "ObjectivesExplicit",
            "AlternativesExplicit",
            "TradeOffsExplicit",
            "DecisionExplicit",
            "ResultsAdopted")
freq2.col<-c("RoleSpecification",
             "OpenMeetings")
# Get columns for participant analysis
part.col<-c("Leader",
            "Participants",
            "ObjElicitationSource_Exp",
            "ProcedureElicitation_Exp",
            "ObjElicitationSource_Sub",
            "ProcedureElicitation_Sub")
obj.col<-c("ObjType",
           "ObjCategory",
           "ObjDirection",
           "ObjScale")
alt.col<-c("ManagementType",
           "AlternativesEvaluated")
# Get columns for map
map.col<-c("Latitude",
           "Longitude",
           "Citation",
           "Drivers")

# Select data summary columns
summary.data_pub<-data_pub %>%
  select(all_of(summary.col))
summary.data_CC<-data_CC %>%
  select(all_of(summary.col))

# Frequency of method
n_mse<-nrow(data)
n_pub<-nrow(data_pub)
n_obj_pub<-nrow(obj.data_pub)
n_CC<-nrow(data_CC)
n_obj_CC<-nrow(obj.data_CC)

freq.data_pub<-data_pub %>%
  select(all_of(freq.col)) %>%
  rename("Process"="ProcessExplicit",
         "Problem"="ProblemDefinitionExplicit",
         "Objectives"="ObjectivesExplicit",
         "Alternatives"="AlternativesExplicit",
         "Tradeoffs"="TradeOffsExplicit",
         "Decision"="DecisionExplicit",
         "Results Adopted"="ResultsAdopted") %>%
  summarise_all(sum)%>%
  gather(Explicit) %>%
  mutate(Percent=value/n_pub*100) %>%
  mutate(Explicit=factor(Explicit,levels=
                           c("Process","Problem","Objectives","Alternatives","Tradeoffs","Decision","Results Adopted"))) %>%
  rename("Number"="value")

freq.data_CC<-data_CC %>%
  select(all_of(freq.col)) %>%
  rename("Process"="ProcessExplicit",
         "Problem"="ProblemDefinitionExplicit",
         "Objectives"="ObjectivesExplicit",
         "Alternatives"="AlternativesExplicit",
         "Tradeoffs"="TradeOffsExplicit",
         "Decision"="DecisionExplicit",
         "Results Adopted"="ResultsAdopted") %>%
  summarise_all(sum)%>%
  gather(Explicit) %>%
  mutate(Percent=value/n_CC*100) %>%
  mutate(Explicit=factor(Explicit,levels=
                           c("Process","Problem","Objectives","Alternatives","Tradeoffs","Decision","Results Adopted"))) %>%
  rename("Number"="value")

freq2.data_pub<-data_pub %>%
  select(all_of(freq2.col)) %>%
  rename("Roles"="RoleSpecification",
         "Open Meetings"="OpenMeetings") %>%
  summarise_all(sum) %>%
  gather(Explicit) %>%
  mutate(Percent=value/n_pub*100) %>%
  mutate(Explicit=factor(Explicit,levels=
                           c("Roles","Open Meetings"))) %>%
  rename("Number"="value")

# Who participates
part.data_pub<-data_pub %>%
  select(all_of(part.col)) %>%
  rename("Process"="Leader",
         "Doc Objectives"="ObjElicitationSource_Exp",
         "Doc Alternatives"="ProcedureElicitation_Exp",
         "Sub Objectives"="ObjElicitationSource_Sub",
         "Sub Alternatives"="ProcedureElicitation_Sub")

part.data_pub<- part.data_pub %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table)


part.data_pub<-plyr::ldply(part.data_pub,data.frame)
colnames(part.data_pub)<-c("Stage","Participants","Number")

neworder <- c("Process","Participants","Doc Objectives",
              "Sub Objectives","Doc Alternatives",
              "Sub Alternatives")
newlabels <- c("Process","Participants","Explicit Objectives Process",
               "Subjective Objectives Process","Explicit Alternatives Process",
               "Subjective Alternatives Process")

part.data_pub <- part.data_pub %>%
  mutate(Percent=Number/n_pub*100) %>%
  mutate(Stage=factor(Stage,levels=neworder,labels=newlabels)) %>%
  ungroup() %>%
  # 2. Arrange by
  #   i.  facet group =Stage
  #   ii. bar height
  arrange(Stage, Percent) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number())

part.data_CC<-data_CC %>%
  select(all_of(part.col)) %>%
  rename("Process"="Leader",
         "Doc Objectives"="ObjElicitationSource_Exp",
         "Doc Alternatives"="ProcedureElicitation_Exp",
         "Sub Objectives"="ObjElicitationSource_Sub",
         "Sub Alternatives"="ProcedureElicitation_Sub")

part.data_CC<- part.data_CC %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table)


part.data_CC<-plyr::ldply(part.data_CC,data.frame)
colnames(part.data_CC)<-c("Stage","Participants","Number")

part.data_CC <- part.data_CC %>%
  mutate(Percent=Number/n_CC*100) %>%
  mutate(Stage=factor(Stage,levels=neworder,labels=newlabels)) %>%
  ungroup() %>%
  # 2. Arrange by
  #   i.  facet group =Stage
  #   ii. bar height
  arrange(Stage, Percent) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number())

part.dataTable_pub<-part.data_pub %>%
  as_tibble() %>%
  select(Stage,Participants,Number) %>%
  rename("Participant Group"="Participants") %>%
  spread(Stage,Number,fill=0)

part.dataTable_CC<-part.data_CC %>%
  as_tibble() %>%
  select(Stage,Participants,Number) %>%
  rename("Participant Group"="Participants") %>%
  spread(Stage,Number,fill=0)

# What drivers are considered
drive.data_pub<-data_pub %>%
  select(Drivers) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Driver"="Var1","Frequency"="Freq") %>%
  mutate(Percent=Frequency/n_pub*100) %>%
  arrange(desc(Frequency))

drive.data_CC<-data_CC %>%
  select(Drivers) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Driver"="Var1","Frequency"="Freq") %>%
  mutate(Percent=Frequency/n_CC*100) %>%
  arrange(desc(Frequency))

# What objectives categories were considered
objcat.data_pub<-data_pub %>%
  select(ObjectiveCategories) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Objective Category"="Var1","Frequency"="Freq") %>%
  mutate(Percent=Frequency/n_pub*100) %>%
  arrange(desc(Frequency))

objcat.data_CC<-data_CC %>%
  select(ObjectiveCategories) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Objective Category"="Var1","Frequency"="Freq") %>%
  mutate(Percent=Frequency/n_CC*100) %>%
  arrange(desc(Frequency))

# How were objectives defined
obj.data_pub<-obj.data_pub %>%
  select(all_of(obj.col)) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame)

obj.data_CC<-obj.data_CC %>%
  select(all_of(obj.col)) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame)

colnames(obj.data_pub)<-colnames(obj.data_CC)<-c("Objective","Type","Number")

neworder <- c("ObjCategory","ObjType","ObjDirection","ObjScale")
newlabels <- c("Category","Type","Direction","Scale")

# What Objective types are considered
obj.dataTable_pub <- obj.data_pub %>%
  mutate(Percent=round(Number/n_obj_pub*100,0)) %>%
  mutate('Per MSE'=round(Number/n_pub,2)) %>%
  mutate(Objective=factor(Objective,levels=neworder,labels=newlabels)) %>%
  arrange(Objective,desc(Number))

obj.dataTable_CC <- obj.data_CC %>%
  mutate(Percent=round(Number/n_obj_CC*100,0)) %>%
  mutate('Per MSE'=round(Number/n_CC,2)) %>%
  mutate(Objective=factor(Objective,levels=neworder,labels=newlabels)) %>%
  arrange(Objective,desc(Number))

# What Alternative types are considered
altcat.data_pub<-alt.data_pub %>%
  select(ManagementType) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Management Type"="Var1","Number"="Freq") %>%
  mutate(Percent=round(Number/n_pub*100,0)) %>%
  mutate('Per MSE'=round(Number/n_pub,2)) %>%
  arrange(desc(Number))

altcat.data_CC<-alt.data_CC %>%
  select(ManagementType) %>%
  purrr::map(~ strsplit(as.character(.),split=", ")) %>%
  purrr::map(unlist) %>%
  purrr::map(table) %>%
  plyr::ldply(data.frame) %>%
  select(Var1,Freq) %>%
  rename("Management Type"="Var1","Number"="Freq") %>%
  mutate(Percent=round(Number/n_CC*100,0)) %>%
  mutate('Per MSE'=round(Number/n_CC,2)) %>%
  arrange(desc(Number))

# Where MSEs have occured
map.data<-rbind(data_pub,data_CC) %>%
  select(all_of(map.col)) %>%
  mutate(Drivers=str_extract(Drivers, "Climate Change")) %>%
  mutate(Drivers=replace_na(Drivers, "Random Sample"))

# label MSEs articles by analysis for ploting
freq.data_pub<-mutate(freq.data_pub,Analysis="Random Sample")
freq.data_CC<-mutate(freq.data_CC,Analysis="Climate Change")
freq.data<-rbind(freq.data_pub,freq.data_CC)
part.data_pub<-mutate(part.data_pub,Analysis="Random Sample")
part.data_CC<-mutate(part.data_CC,Analysis="Climate Change")
part.data<-rbind(part.data_pub,part.data_CC) %>% 
  arrange(Stage, Percent) %>% 
  mutate(order = row_number())
# part.data$order<-c(1,2,3,3,4,4,5,6,7,8,6,9,10,8,9,11,12,10,13,12,13,14,14,15,16,17,18,19,
#                    20,16,17,18,20,21,22,23,24,25,21,23,26,25,26,27,27,28,29,30,31,29,32,
#                    30,33,32,33,34,35,36,37,38,39,39)
drive.data_pub<-mutate(drive.data_pub,Analysis="Random Sample") %>% 
  mutate(Percent=round(Percent,0)) %>% 
  select(Analysis,Driver,Percent,Frequency)
drive.data_CC<-mutate(drive.data_CC,Analysis="Climate Change")%>% 
  mutate(Percent=round(Percent,0)) %>% 
  select(Analysis,Driver,Percent,Frequency)
drive.data<-rbind(drive.data_pub,drive.data_CC) %>% 
  arrange(Percent) %>%
  mutate(order = row_number())
drive.data$order<-c(6,5,4,3,2,1,8,7,12,9,3,15,14,13,12,11,10,14,16,15,
                    16,6)
# Wrangle objective category data by Random Sample, climate change, and combined
objcat.data_pub<-mutate(objcat.data_pub,Analysis="Random Sample") %>% 
  mutate(Percent=round(Percent,0)) %>% 
  select(Analysis,'Objective Category',Percent,Frequency)
objcat.data_CC<-mutate(objcat.data_CC,Analysis="Climate Change")%>% 
  mutate(Percent=round(Percent,0)) %>% 
  select(Analysis,'Objective Category',Percent,Frequency)
objcat.data<-rbind(objcat.data_pub,objcat.data_CC) %>% 
  rename(OC='Objective Category') %>% 
  arrange(OC) %>% 
  rename('Objective Category'=OC)

# Wrangle objective data by Random Sample, climate change, and combined
obj.dataTable_pub<-mutate(obj.dataTable_pub,Analysis="Random Sample") %>% 
  mutate(Percent=round(Percent,0))
obj.dataTable_CC<-mutate(obj.dataTable_CC,Analysis="Climate Change")%>% 
  mutate(Percent=round(Percent,0))
obj.dataTable<-rbind(obj.dataTable_pub,obj.dataTable_CC) %>% 
  filter(Objective=="Category") %>%
  group_by(Analysis) %>% 
  summarise('AVG'=sum(Number)) %>% 
  mutate(count=c(11,30)) %>% 
  mutate('Per MSE'=round(AVG/count,1)) %>% 
  mutate('Type'=c("Objectives","Objectives"))

altcat.data_pub<-mutate(altcat.data_pub,Analysis="Random Sample") %>% 
  mutate(Percent=round(Percent,0))
altcat.data_CC<-mutate(altcat.data_CC,Analysis="Climate Change")%>% 
  mutate(Percent=round(Percent,0))
altcat.data<-rbind(altcat.data_pub,altcat.data_CC) %>%
  group_by(Analysis) %>% 
  summarise('AVG'=sum(Number)) %>% 
  mutate(count=c(11,30)) %>% 
  mutate('Per MSE'=round(AVG/count,1)) %>% 
  mutate('Type'=c("Alternatives","Alternatives"))

per.MSE<-obj.dataTable %>% 
  mutate(order=c(2,1)) %>% 
  arrange(order) %>% 
  select(Analysis,'Per MSE')

# Get map background for plotting the map
world <- borders("world", colour="gray85", fill="gray96", alpha=0.75) # create a layer of borders

# plot MSEs on map
MSE.map<-ggplot(data=map.data,aes(x=Longitude, y=Latitude,color=Drivers)) + world +
  geom_point(size=2.5) + geom_point(size = 2.5, colour = "gray55", shape = 1) +
  scale_color_manual(values=c("gray10","gray55"))+theme_void() +
  theme(legend.position = c(0.15, 0.25))

# plot explicit documentation of steps or components of MSE processes 
# Publication MSEs
Freq.plot_reviewed<-ggplot(freq.data_pub,aes(Explicit,Percent))+
    geom_col()+geom_vline(xintercept=3.5,linetype="dashed")+
    coord_flip()+scale_y_continuous(limits=c(0,100),expand = c(0,0))+xlab(NULL)+
    scale_x_discrete(
      limits=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
               "Alternatives","Objectives","Problem","Process"), 
      labels=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
               "Alternatives","Objectives","Problem","Process"))

#Climate Change MSEs
Freq.plot_CC<-ggplot(freq.data_CC,aes(Explicit,Percent))+
  geom_col()+geom_vline(xintercept=3.5,linetype="dashed")+
  coord_flip()+scale_y_continuous(limits=c(0,100),expand = c(0,0))+xlab(NULL)+
  scale_x_discrete(
    limits=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process"), 
    labels=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process"))

Freq.plot_both<-ggplot(freq.data,aes(Explicit,Percent,fill=Analysis))+
  geom_col(position="dodge")+geom_vline(xintercept=3.5,linetype="dashed")+
  coord_flip()+scale_fill_manual(values=c("#FF0033","#0000FF")) +
  scale_y_continuous(limits=c(0,100),expand = c(0,0))+xlab(NULL)+
  scale_x_discrete(
    limits=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process"), 
    labels=c("Adopted","Open Meetings","Roles","Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process")) +
  guides(fill = guide_legend(reverse=T)) + theme_bw()

Freq.plot_pub<-ggplot(freq.data_pub,aes(Explicit,Percent))+
  geom_col()+
  coord_flip() +
  scale_y_continuous(limits=c(0,100),expand = c(0,0))+xlab(NULL)+
  scale_x_discrete(
    limits=c("Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process"),
    labels=c("Decision","Tradeoffs",
             "Alternatives","Objectives","Problem","Process")) + theme_bw()

Part.plot_reviewed<-ggplot(part.data_pub,aes(x=order,y=Percent)) +
    facet_wrap(~Stage,scale="free",ncol=2) + geom_col() +  
    scale_x_continuous(breaks = part.data_pub$order,
      labels = part.data_pub$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
    ylab("Percent")+coord_flip()

Part.plot_CC<-ggplot(part.data_CC,aes(x=order,y=Percent)) +
  facet_wrap(~Stage,scale="free",ncol=2) + geom_col() +  
  scale_x_continuous(breaks = part.data_CC$order,
                     labels = part.data_CC$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  ylab("Percent")+coord_flip()

Part.plot_all<-ggplot(part.data,aes(x=order,y=Percent,fill=Analysis)) +
  facet_wrap(~Stage,scale="free",ncol=2) + geom_col(position="dodge") +  
  scale_fill_manual(values=c("#FF0033","#0000FF"))+
  scale_x_continuous(breaks = part.data$order,
                     labels = part.data$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  ylab("Percent")+xlab(NULL)+coord_flip() + 
  guides(fill = guide_legend(reverse=T)) + theme_bw() +
  theme(legend.position = c(0.875,0.085),legend.title = element_blank())

Part.plot_pub<-ggplot(part.data_pub,aes(x=order,y=Percent)) +
  facet_wrap(~Stage,scale="free",ncol=2) + geom_col() +
  scale_fill_manual(values=c("#66CCCC","#006666")) +
  scale_x_continuous(breaks = part.data_pub$order,
                     labels = part.data_pub$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  ylab("Percent")+xlab(NULL)+coord_flip() + theme_bw()

part.data_graphicalAbstract<-part.data_pub %>% 
  mutate(stage_f=factor(Stage,levels=c('Process','Explicit Objectives Process','Explicit Alternatives Process',
                                       'Participants','Subjective Objectives Process',
                                       'Subjective Alternatives Process'))) %>% 
  arrange(stage_f)

Part.plot_GraphicalAbstract<-ggplot(part.data_graphicalAbstract,aes(x=order,y=Percent)) +
  facet_wrap(~stage_f,scale="free",ncol=3) + geom_col() +
  scale_fill_manual(values=c("#66CCCC","#006666")) +
  scale_x_continuous(breaks = part.data_graphicalAbstract$order,
                     labels = part.data_graphicalAbstract$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  ylab("Percent")+xlab(NULL)+coord_flip(clip="on") + theme_bw() + theme(plot.margin = margin(0, 0.5, 0, 0.25, "cm"))

ggplot(part.data_pub,aes(x=order,y=Percent)) +
  facet_wrap(~Stage,scale="free",ncol=2) + geom_col() +
  scale_fill_manual(values=c("#66CCCC","#006666")) +
  scale_x_continuous(breaks = part.data_pub$order,
                     labels = part.data_pub$Participants)+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  ylab("Percent")+xlab(NULL)+coord_flip() + theme_bw()

as_tibble(summary.data_pub)
as_tibble(summary.data_CC)

#Show poster results
n_mse
n_CC
MSE.map
Freq.plot
Part.plot
as_tibble(drive.data_pub)
as_tibble(drive.data_CC)
as_tibble(objcat.data_pub)
as_tibble(objcat.data_CC)
as_tibble(obj.dataTable_pub)
as_tibble(obj.dataTable_CC)
as_tibble(altcat.data_pub)
as_tibble(altcat.data_CC)

# devtools::install_github("odeleongt/postr")
# library(postr)
# 
# devtools::install_github("brentthorne/posterdown")
# library(posterdown)
# 
# ##### Example SQL update and select syntax #####
# dbExecute(con, "UPDATE tblStudyObjectives 
#   SET ObjDirection = 'Constraint', ObjMetric = 'SB(y)/SB(0)'
#   WHERE ID = 51")
# 
# dbExecute(con, "DELETE FROM tblStudyManagement")
# # 
# dbGetQuery(con, "SELECT * FROM tblStudyObjectives WHERE ID = 51")
# 
# obj.dataTable_pub<-mutate(obj.dataTable_pub,Analysis="Reviewed") %>% 
#   mutate(Percent=round(Percent,0))
# obj.dataTable_CC<-mutate(obj.dataTable_CC,Analysis="Climate Change")%>% 
#   mutate(Percent=round(Percent,0))
# obj.dataTable<-rbind(obj.dataTable_pub,obj.dataTable_CC) %>% 
#   filter(Objective=="Category") %>%
#   group_by(Analysis) %>% 
#   summarise('AVG'=sum(Number)) %>% 
#   mutate(count=c(11,30)) %>% 
#   mutate('Per MSE'=AVG/count)
# 
# alt.dataTable_pub<-mutate(alt.dataTable_pub,Analysis="Reviewed") %>% 
#   mutate(Percent=round(Percent,0))
# alt.dataTable_CC<-mutate(alt.dataTable_CC,Analysis="Climate Change")%>% 
#   mutate(Percent=round(Percent,0))
# alt.dataTable<-rbind(alt.dataTable_pub,alt.dataTable_CC) %>% 
#   filter(Objective=="Category") %>%
#   group_by(Analysis) %>% 
#   summarise('AVG'=sum(Number)) %>% 
#   mutate(count=c(11,30)) %>% 
#   mutate('Per MSE'=AVG/count)

#Analyze the year that MSEs and the random sample of MSEs were published

# Rename columns, filter to only MSEs, and select year published and whether the publication was in the 
# random sample
SampleCheck<-tblWOS %>%
  rename(IsMSE="Is MSE?") %>% 
  filter(IsMSE==TRUE&YearPub<2019) %>%
  mutate(IncludeInPub = replace(IncludeInPub, IncludeInPub == TRUE, "Selected")) %>% 
  mutate(IncludeInPub = replace(IncludeInPub, IncludeInPub == FALSE, "Not Selected")) %>% 
  select(YearPub,IncludeInPub)
# Plot result as a histogram
ggplot(SampleCheck,aes(x=YearPub,color=IncludeInPub,fill=IncludeInPub))+geom_histogram(binwidth = 1)+
  scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+theme_bw()+
  labs(col="Random Sample",fill="Random Sample",x="Publication Year")+theme(legend.position = c(0.15, 0.85))

# # make a table of the result
# SampleCount<-SampleCheck %>% 
#   group_by(YearPub,IncludeInPub) %>% 
#   count() %>% 
#   pivot_wider(names_from = IncludeInPub,values_from = n) %>% 
#   rename(Selected="TRUE",
#          NotSelected="FALSE") %>%
#   mutate_at(vars(Selected,NotSelected),~replace_na(., 0)) %>% 
#   mutate(percent=Selected/(Selected+NotSelected)*100) %>% 
#   pivot_longer(cols = c(Selected,NotSelected),names_to = "RandomSample") %>% 
#   arrange(value)
# # Plot result as area
# ggplot(SampleCount,aes(x=YearPub,y=value,color=RandomSample,fill=RandomSample))+geom_area()+geom_point(position = "stack")+
#   scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+theme_bw()+
#   labs(y="Publication Count",x="Year Published")

# Wrangle data
SampleJournals<-tblWOS %>%
  rename(IsMSE="Is MSE?") %>% 
  filter(IsMSE==TRUE) %>% 
  select(Journal,IncludeInPub) %>%
  group_by(Journal,IncludeInPub) %>% 
  count() %>%
  arrange(-n) %>% 
  ungroup() %>% 
  mutate(IncludeInPub = replace(IncludeInPub, IncludeInPub == TRUE, "Selected")) %>% 
  mutate(IncludeInPub = replace(IncludeInPub, IncludeInPub == FALSE, "Not Selected")) %>% 
  drop_na()
# Plot result for all journals
ggplot(SampleJournals,aes(x=reorder(Journal, n),y=n,color=IncludeInPub,fill=IncludeInPub))+geom_col()+
  scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+theme_bw()+
  labs(col="Random Sample",fill="Random Sample",y="Publication Count")+coord_flip()+
  theme(legend.position = c(0.75, 0.125),axis.title.y=element_blank())
# remove journals with less than 2 MSEs
SampleJournalsFiltered<-SampleJournals %>% 
  pivot_wider(names_from = IncludeInPub,values_from = n) %>%
  mutate_at(vars(Selected,NotSelected),~replace_na(., 0)) %>% 
  mutate(Total=Selected+NotSelected) %>% 
  mutate(percent=Selected/(Selected+NotSelected)*100) %>% 
  filter(Total>1) %>% 
  pivot_longer(cols = c("NotSelected","Selected"),names_to = "IncludeInPub",values_to = "n") %>% 
  select(Journal,IncludeInPub,n)
# Plot result
ggplot(SampleJournalsFiltered,aes(x=reorder(Journal, n),y=n,color=IncludeInPub,fill=IncludeInPub))+geom_col()+
  scale_color_grey()+scale_fill_manual(values=c("gray35","gray50"))+theme_bw()+
  labs(col="Random Sample",fill="Random Sample")+coord_flip()+
  theme(legend.position = c(0.75, 0.125),axis.title.y=element_blank())+
  labs(y="Publication Count")

# Examining the years and journals of climate change MSE publications
SampleClimate<-data_CC %>%
  select(YearPub) %>% 
  group_by(YearPub) %>% 
  count() %>% 
  ungroup() %>% 
  add_row(YearPub=c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008",
                    "2012","2014","2015"),n=0)
ggplot(SampleClimate,aes(x=YearPub,y=n))+geom_col()+
  scale_color_grey()+theme_bw()+
  labs(x="Publication Year")
SampleClimateJournal<-data_CC %>%
  select(Journal) %>% 
  group_by(Journal) %>% 
  count() %>% 
  arrange(-n)
ggplot(SampleClimateJournal,aes(x=reorder(Journal,n),y=n))+geom_col()+  scale_color_grey()+theme_bw()+  coord_flip()+
  theme(axis.title.y=element_blank())+  labs(y="Publication Count")

