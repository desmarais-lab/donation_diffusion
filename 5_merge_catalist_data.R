library(boxr) 
library(tidyverse)
library(yaml)
library(plyr)
library(stringr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)


#config = yaml.load_file('0_config.yml')
config = yaml.load_file('~/donation_diffusion/0_config.yml')
#LOCAL_DATA = config$LOCAL_DATA
LOCAL_DATA = "~/Box Sync/Strategic_Donors/final_paper_data/"

#if(!is.null(LOCAL_DATA)) {
#    random_reply = read_csv(paste0(LOCAL_DATA, 
#                                   'catalist_random_donors_ideology.csv')) 
#    active_reply = read_csv(paste0(LOCAL_DATA, 
#                                   'catalist_active_donor_ideology.csv')) 
#}



#Read in vertex level data
if(!is.null(LOCAL_DATA)) {
  new_vlc = read_csv(paste0(LOCAL_DATA, 'VLC_16.csv'))
} else {
  box_auth()
  new_vlc = box_read_csv(file_id = '302126365533')
}


#read in data files from Catalist (matched to active donor list):
#ideology, income, and partisanship (models)
#Age, race, PID (revealed) 

if(!is.null(LOCAL_DATA)) {
  catalist_ideology = read_csv(paste0(LOCAL_DATA, 'catalist-active-Ideology.csv'))
  catalist_income = read_csv(paste0(LOCAL_DATA, 'catalist-active-incomemodel.csv'))
  catalist_partisanship = read_csv(paste0(LOCAL_DATA, 'catalist-active-Partisanship.csv'))
  catalist_age = read_csv(paste0(LOCAL_DATA, 'catalist-active-Age.csv'))
  catalist_race = read_csv(paste0(LOCAL_DATA, 'catalist-active-Race.csv'))
} else {
  box_auth()
  catalist_ideology = box_read_csv(file_id = '308272085168')
  catalist_income = box_read_csv(file_id = '308271872207')
  catalist_partisanship = box_read_csv(file_id = '308274474344')
  catalist_age = box_read_csv(file_id = '308273996144')
  catalist_race = box_read_csv(file_id = '308274760111')
}

#combine into single file for active donors
active_catalist = Reduce(function(x, y) merge(x, y, all=TRUE), list(catalist_ideology,catalist_income,catalist_partisanship,
                                                                    catalist_age,catalist_race))
active_catalist$donor_type = "active"

#reformat actor ID:
active_catalist$ID = paste0(substr(active_catalist$ID,1,(nchar(active_catalist$ID)-6)),"_",substr(active_catalist$ID,(nchar(active_catalist$ID)-4),nchar(active_catalist)))
active_catalist$ID = sub(' ', ', ', active_catalist$ID)
#fix idiosynchratic issues with names
active_catalist$ID <- mapvalues(active_catalist$ID, from=c("HEMINGWAY, FEUER ANNE_33157","POWELL, JOBS LAURENE_94301","VAN, HEUVELEN ROBERT_49024","VAN, SCOYOC H STEWART_22066"), 
                                to=c("POWELL JOBS, LAURENE_94301", "VAN SCOYOC, H STEWART_22066" , "VAN HEUVELEN, ROBERT_49024", "FEUER, ANNE HEMINGWAY_33157"))

  
#read in data files from Catalist (random sample), reformat to match active donor files
#(Note: the active donor sample was pulled from Catalist through a university subscription, which
#was not renewed. As a result, we had to purchase the random donor sample data directly from Catalist,
#and the file they sent contained all data in a single csv file. Here we reformat to match the 
#active_catalist file and combine):
random_catalist = read.delim(paste0(LOCAL_DATA,"catalist-random-sample-data.txt"),header=T)
random_catalist = random_catalist[,c(4,52,54,53,20,22,3)]
colnames(random_catalist)<-names(active_catalist)

#place age, ideology and partisanship models into bins to match the active donor sample data
#age
rplc<-c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","Older\nthan 95")
random_catalist$Age<-cut(random_catalist$Age,c(18,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,120))
tb<-as.data.frame(table(random_catalist$Age))
random_catalist$Age<-as.character(mapvalues(random_catalist$Age,from=tb$Var1,to=rplc))

#income
random_catalist$incomemodel <-  str_replace_all(random_catalist$incomemodel,",","")

#ideology
rplc<-c("0.0–5.0","5.01–10.0","10.01–15.0","15.01–20.0","20.01–25.0","25.01–30.0","30.01–35.0","35.01–40.0","40.01–45.0","45.01–50.0",
        "50.01–55.0","55.01–60.0","60.01–65.0","65.01–70.0", "70.01–75.0","75.01–80.0","80.01–85.0","85.01–90.0","90.01–95.0","95.01–100.0")
random_catalist$Ideology<-cut(random_catalist$Ideology,c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
tb<-as.data.frame(table(random_catalist$Ideology))
random_catalist$Ideology<-mapvalues(random_catalist$Ideology,from=tb$Var1,to=rplc)

#partisanship
rplc <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
random_catalist$Partisanship<-cut(random_catalist$Partisanship,c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
tb2<-as.data.frame(table(random_catalist$Partisanship))
random_catalist$Partisanship<-mapvalues(random_catalist$Partisanship,from=tb2$Var1,to=rplc)


#format active donor file 
#age:
active_catalist$Age[active_catalist$Age %in% c("95–99","115–119")]<-"Older\nthan 95"
active_catalist$Age<-str_replace_all(active_catalist$Age,"–","-")

#partisanship
#convert from category to numeric (0 = strong R, 20 = strong D)
active_catalist$Partisanship<-factor(active_catalist$Partisanship)
active_catalist$Partisanship<-as.integer(active_catalist$Partisanship)

#income
active_catalist$incomemodel<-factor(substr(active_catalist$incomemodel,4,nchar(active_catalist$incomemodel)))
active_catalist$incomemodel <- factor(active_catalist$incomemodel, levels=c("Less than $20000","$20000 - $30000","$30000 - $50000",
                                      "$50000 - $75000","$75000 - $100000","$100000 - $150000","Greater than $150000"))


#JOIN ACTIVE AND RANDOM SAMPLES:
catalist_full <- rbind(active_catalist,random_catalist)
colnames(catalist_full)[1]<-"Actor_ID"


#merge in to full vlc file
table(catalist_full$Actor_ID %in% new_vlc$Actor_ID)
new_vlc_catalist = left_join(new_vlc,catalist_full)


#check if any donors became "active" after full OS data downloaded:
if(!is.null(LOCAL_DATA)) {
  active_donor_list = read_csv(paste0(LOCAL_DATA, 'data_for_netinf_threshold_8.csv'))
} else {
  box_auth()
  active_donor_list = box_read_csv(file_id = '302844881600')}

new_vlc_catalist$donor_type = ifelse(new_vlc_catalist$Actor_ID %in% active_donor_list$Donor_ID & new_vlc_catalist$Ent_Typ=="IND","active",new_vlc_catalist$donor_type)



###########
#add percent of donations to democrats (to validate Catalist PID model)
##########
#read in edge list
if(!is.null(LOCAL_DATA)) {
  el = read_csv(paste0(LOCAL_DATA, 'EL_16.csv'))
} else {
  box_auth()
  el = box_read_csv(file_id = '50855821402')}


#only donations to candidates, from individuals
el<-subset(el,el$Recip_Tp=="CAND")
el<-subset(el,el$Donor_Tp == "IND")

#aggregate amount by individual
t<-as.data.frame(aggregate(as.numeric(as.character(el$Amt))~el$Donor_ID,FUN=sum))
new_vlc_catalist$total_donated<-as.numeric(t[,2][match(new_vlc_catalist$Actor_ID,t[,1])])

#aggregate amount by party
el$cand_pty<-as.character(new_vlc_catalist$Party_PAC_Type[match(el$Recip_ID,new_vlc_catalist$Actor_ID)])
tt<-as.data.frame(aggregate(as.numeric(as.character(el$Amt))~el$Donor_ID + el$cand_pty,FUN=sum))

#subset to Dems
tt<-subset(tt,tt[,2]=="D")

#match to x, if not there then 0
new_vlc_catalist$tot_dem<-ifelse(new_vlc_catalist$Actor_ID %in% tt[,1], as.numeric(as.character(tt[,3][match(new_vlc_catalist$Actor_ID,tt[,1])])),0)
new_vlc_catalist$tot_dem <- ifelse(new_vlc_catalist$Ent_Typ=="IND",new_vlc_catalist$tot_dem,NA)
new_vlc_catalist$pct_dem<- (new_vlc_catalist$tot_dem/new_vlc_catalist$total_donated)*100


##############
#LOCATION DATA
##############
#read in original individual file
if(!is.null(LOCAL_DATA)) {
  inds = fread(paste0(LOCAL_DATA, 'indivs16.txt'))
} else {
  box_auth()
  inds = box_read(file_id = '302063495065',fread=T)
}

#remove columns with commas
inds<-inds[,c(seq(2,16,by=2),17,seq(20,36,by=2),40,42)]
colnames(inds)<-c("Year","Record_ID","ID","Name","RID","Employer","Employer_Parent","Industry_Ideo","Date_Amt",
         "City","State","Zip","R_Type","Tran_Type","Cmte_ID","Other_ID","Gender","Micro","Employer","Source")

#ind ID
inds$ind_ID<-paste(inds$Name,inds$Zip,sep="_")
inds<-subset(inds,!duplicated(inds$ind_ID))

#ind state, city, zip
new_vlc_catalist$ind_state<-as.character(inds$State[match(new_vlc_catalist$Actor_ID,inds$ind_ID)])
new_vlc_catalist$ind_city<-as.character(inds$City[match(new_vlc_catalist$Actor_ID,inds$ind_ID)])
new_vlc_catalist$ind_zip<-as.character(inds$Zip[match(new_vlc_catalist$Actor_ID,inds$ind_ID)])

#add leading zeros to zip
new_vlc_catalist$ind_zip<-sprintf("%05s",new_vlc_catalist$ind_zip)

#Congressional District - data from HUD/census: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
#use RES_RATIO to determine probability that zip to cd is correct
if(!is.null(LOCAL_DATA)) {
  cds = read.csv(paste0(LOCAL_DATA, 'zip2cd.csv'),header=T,colClasses = "character")
} else {
   box_auth()
  cds = box_read_csv(file_id = '308335502405')}


#focus on zip associated with maximum RES_RATIO
cd2 = aggregate(RES_RATIO ~ ZIP + CD, cds, max)

new_vlc_catalist$ind_cd<-as.character(cd2$CD[match(new_vlc_catalist$ind_zip,cd2$ZIP)])

#only cd (not state fips code)
new_vlc_catalist$ind_cd<-sprintf("%04s",new_vlc_catalist$ind_cd)
new_vlc_catalist$ind_cd<-substr(new_vlc_catalist$ind_cd,3,4)

#add res ratio
new_vlc_catalist$res_ratio<-as.character(cd2$RES_RATIO[match(new_vlc_catalist$ind_zip,cd2$ZIP)])


#ind_state, ind_city, ind_zip, ind_cd, res_ratio, pac_state, activist, age, 
#hhwealth, ideology, income, partisanship, pid, race, pct_dem
new_vlc_catalist = new_vlc_catalist[,c(1:9,19:23,13,10:12,14,18,16:17,15)]

#reformat column order:


#rename:
colnames(new_vlc_catalist)<-c("Yr","Actor_ID", "Assoc_Cand_ID","Name","Party_PAC_Type",
                              "District","Incum" ,"Winner","Ent_Typ","ind_state", "ind_city",
                              "ind_zip", "ind_cd", "res_ratio","age", "ideology", "income", 
                              "partisanship", "race", "pct_dem","total_donated","total_to_dems","donor_type")


if(!is.null(LOCAL_DATA)) {
  write_csv(new_vlc_catalist, path = paste0(LOCAL_DATA, 'VLC_16_full.csv'))
} else{
  box_write(new_vlc_catalist, 'VLC_16_full.csv', dir_id = '50855821402')
}


rm(list=ls())


#####################
#DEMOGRAPHICS PLOTS#
####################
if(!is.null(LOCAL_DATA)) {
  vlc = read_csv(paste0(LOCAL_DATA, 'VLC_16_full.csv'))
} else {
  box_auth()
  vlc = box_read(file_id = '50855821402')
}

#only those for whom we have Catalist data
full_samp = subset(vlc,!is.na(vlc$race))


#AGE
age<-as.data.frame(prop.table(table(full_samp$age,full_samp$donor_type),margin=2))
age<-subset(age,age$Var1!="")
colnames(age)<-c("Age","Type","Percentage")

a<-ggplot(data=age,aes(x=Age,y=(Percentage*100)))+geom_bar(aes(fill=Type),position="dodge",stat="identity")+
  xlab("")+ylab("Percentage of\nDonor Category")+ggtitle("(a) Donor Age Distributions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.background = element_rect(fill = 'white', colour = 'grey'))+ 
  guides(fill=guide_legend(title="Donor Type"))+
  scale_fill_grey(start=0,end=0.7)

#INCOME
inc<-as.data.frame(prop.table(table(full_samp$income,full_samp$donor_type),margin=2))
inc<-subset(inc,inc$Var1!="")
colnames(inc)<-c("Income","Type","Percentage")
inc$Income<-as.character(inc$Income)
inc$Income<-str_replace_all(inc$Income," - "," -\n")
inc$Income <- str_replace_all(inc$Income," than "," than\n")
inc$Income <- factor(inc$Income, levels=c("Less than\n$20000","$20000 -\n$30000","$30000 -\n$50000",
                                          "$50000 -\n$75000","$75000 -\n$100000","$100000 -\n$150000","Greater than\n$150000"))


b<-ggplot(data=inc,aes(x=Income,y=(Percentage*100)))+geom_bar(aes(fill=Type),position="dodge",stat="identity")+
  xlab("")+ylab("")+ggtitle("(b) Donor Income Distributions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.background = element_rect(fill = 'white', colour = 'grey'))+ 
  guides(fill=guide_legend(title="Donor Type"))+
  scale_fill_grey(start=0,end=0.7)


#PARTISANSHIP
parti<-as.data.frame(prop.table(table(full_samp$partisanship,full_samp$donor_type),margin=2))
colnames(parti)<-c("Partisanship","Type","Percentage")

c<-ggplot(data=parti,aes(x=Partisanship,y=(Percentage*100)))+geom_bar(aes(fill=Type),position="dodge",stat="identity")+
  xlab("")+ylab("Percentage of\nDonor Category")+ggtitle("(c) Donor Partisanship Distributions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.background = element_rect(fill = 'white', colour = 'grey'))+ 
  guides(fill=guide_legend(title="Donor Type"))+
  scale_fill_grey(start=0,end=0.7)+
  scale_x_discrete(breaks=c(1,5,10,15,20),labels=c("Strong\nRepublican","Republican","Independent","Democrat","Strong\nDemocrat"))


#IDEOLOGY
ideo<-as.data.frame(prop.table(table(full_samp$ideology,full_samp$donor_type),margin=2))
ideo<-subset(ideo,ideo$Var1!="")
colnames(ideo)<-c("Ideology","Type","Percentage")

d<-ggplot(data=ideo,aes(x=Ideology,y=(Percentage*100)))+geom_bar(aes(fill=Type),position="dodge",stat="identity")+
  xlab("")+ylab("")+ggtitle("(d) Donor Ideology Distributions")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.background = element_rect(fill = 'white', colour = 'grey'))+ 
  guides(fill=guide_legend(title="Donor Type"))+
  scale_fill_grey(start=0,end=0.7)+
  scale_x_discrete(breaks=c("0.0–5.0","25.01–30.0","50.01–55.0","75.01–80.0","95.01–100.0"),labels=c("Very\nConservative","Conservative","Moderate","Liberal","Very\nLiberal"))



#RACE (do not plot)
prop.table(table(tolower(full_samp$race), full_samp$type),margin=2)*100



#shared legend function:
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x) x + theme(legend.position="none")), ncol=2,nrow=2)),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

pdf(".../sample_demos_plot_all_donor_categories.pdf",height=8,width=8.5,onefile = F)
grid_arrange_shared_legend(a,b,c,d)
dev.off()


