library(stringr)
library(data.table)
library(bit64)

##################
## Cand/PAC Info - create vertex level data
#################
yr<-"16"
  
  #CANDS
  cands<-read.delim(paste("../data/cands",yr,".txt",sep=""),sep="|",header=F,quote="\n")
  #rm columns with ,
  cands<-cands[,c(seq(2,24,by=2))]
  #name
  nms_c<-c("Year","ID","CID","Name","Party","District","Current_Dist","Active","Active2","Incum_Won","Party_Won","NoPacs")
  colnames(cands)<-nms_c
  
  cands$won<-substr(cands$Party_Won,2,2)
  
  #subset to include year, cmte ID, cand ID, name, party, district, incumbency/winner
  cands<-cands[,c(1:6,10,13)]
  
  #party - change all 3rd party to 3
  cands$Party[!cands$Party %in% c("D","R","I")]<-"3"
  
  #Supplement missing incumbency data
  yy<-read.delim(paste("../data/cn",yr,"_fec.txt",sep=""),sep="|",quote="\n",header=F)
  
  cands$Incum_Won[!cands$Incum_Won %in% c("C","I","O")]<-NA
  cands$Incum_Won<-ifelse(is.na(cands$Incum_Won),as.character(yy$V8[match(cands$ID,yy$V1)]),as.character(cands$Incum_Won))
  
  
  #PACS
  setClass("num.with.commas")
  setAs("character", "num.with.commas", 
        function(from) as.character(gsub(",", "", from) ) )
  
  cmtes<-read.delim(paste("../data/cmtes",yr,".txt",sep=""),sep=",",header=F,quote="\n",fill=T,
                    colClasses = "num.with.commas")

  #deal with misaligned rows
  for(i in 1:nrow(cmtes)){
    if(nchar(cmtes[i,1])<6){
      cmtes[i-1,]<-cmtes[(i-1),c(1:4,6,8,9,10,10,12,13,14,10,10)]
    }}
  
  #remove pipes
  for(i in 1:ncol(cmtes)){
    cmtes[,i]<-gsub("[^[:alnum:]]","",cmtes[,i])}
  
  #name
  nms_cm<-c("Year","ID","Name","Affiliate","Parent_Org","RID","R_Type","CID","Party","Industry","Source","Multi_Industry","Foreign","Active")
  colnames(cmtes)<-nms_cm
  
  #remove overflow rows (rows with extraneous data from rows that were too long)
  cmtes<-subset(cmtes,as.character(substr(cmtes$Year,3,4))==yr)
  
  #subset to include year, cmte ID, name, cand ID (or cmte ID if not cand), party/pac type
  cmtes<-cmtes[,c(1:3,8,10)]
  
  #FORMAT CANDS AND CMTES FOR RBIND
  #year, cmte ID, cand ID (if applicable), name, party/pac type, district (cands),incumbency (cands),entity type
  #CANDS
  cands<-merge(cands,cmtes,by.x="ID",by.y="CID",all.x=T) #add candidate cmtes
  cands<-subset(cands,cands$ID!=",")
  cands<-cands[,c(2,3,1,4:8)]
  cands[,9]<-"CAND"
  
  #if cand has no cmte, assign cand id for actor id
  cands$ID<-ifelse(is.na(cands$ID),as.character(cands$CID),as.character(cands$ID))
  
  #CMTES
  cmtes[,6]<-NA
  cmtes[,7]<-"PAC"
  cmtes<-cmtes[,c(1:2,4,3,5,6,6,6,7)]
  cmtes[,5]<-substr(cmtes[,5],1,1)
  
  nms<-c("Yr","Actor_ID","Assoc_Cand_ID","Name","Party_PAC_Type","District","Incum","Winner","Ent_Typ")
  colnames(cands)<-nms
  colnames(cmtes)<-nms

  ####add committee info to missing info
  cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type,1,1) %in% c("A","B","C","D","E","F","G","H","K","M","T")]<-"B"
  cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type,1,1)=="J"]<-"I"
  cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type,1,1)=="L"]<-"L"
  cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type,1,1)=="X"]<-"O"
  cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type,1,1) %in% c("P","S","Y","Z","R")]<-"P"
  
  #augment missing data with FEC data
  y<-read.delim(paste("../data/cm",yr,"_fec.txt",sep=""),sep="|",quote="\n",header=F)
  y$V13<-as.character(y$V13)
  y$V13[y$V13 %in% c("B","T","V","W","C")]<-"B"
  y$V13[y$V13=="M"]<-"I"
  
  cmtes$Party_PAC_Type<-ifelse(cmtes$Party_PAC_Type=="",as.character(y$V13[match(cmtes$Actor_ID,y$V1)]),cmtes$Party_PAC_Type)
  
  write.csv(cands,paste("../data/Cands_",yr,".csv",sep=""),row.names=F)
  write.csv(cmtes,paste("../data/Cmtes_",yr,".csv",sep=""),row.names=F)
  
  ##############
  #Add individuals
  inds<-fread(paste("../data/indivs",yr,".txt",sep=""),header=F,sep="|",quote="\n",fill = T)

  #remove columns with commas
  inds<-inds[,c(seq(2,16,by=2),17,seq(20,36,by=2),40,42)]
  
  #names
  nms_i<-c("Year","Record_ID","ID","Name","RID","Employer","Employer_Parent","Industry_Ideo","Date_Amt",
           "City","State","Zip","R_Type","Tran_Type","Cmte_ID","Other_ID","Gender","Micro","Employer","Source")
  colnames(inds)<-nms_i
  
  #create id (name_zip):
  inds$Donor_ID<-paste(inds$Name,inds$Zip,sep="_")
  
  ##format (same as cands and cmtes) - year, cmte/donor ID, cand ID (if applicable), name, party/pac type, district (cands),incumbency (cands),entity type
  inds$N<-NA
  inds$Ent_Typ<-"IND"
  inds<-inds[,c(1,21,22,4,22,22,22,22,23)]
  colnames(inds)<-nms
  
  write.csv(inds,paste("../data/Indivs_",yr,".csv",sep=""),row.names=F)
  
  
  #########################
  #CREATE VLC LIST, REMOVE DUPLICATES, WRITE
  vlc<-rbind(cands,cmtes,inds)
  vlc<-subset(vlc,!duplicated(vlc$Actor_ID))
  
  write.csv(vlc,paste("../data/VLC_",yr,".csv",sep=""),row.names=F)

  rm(list=ls())



######################################################
###### CREATE EDGE LIST FROM CONTRIBUTION FILES 
######################################################
yr<-"16"

  #read vertex data
  vlc<-fread(paste("../data/VLC_",yr,".csv",sep=""))
  
  #Read Indiv file
  ###########
  inds<-fread(paste("../data/indivs",yr,".txt",sep=""),header=F,sep="|",quote="\n",fill = T)
  
  #remove columns with commas
  inds<-inds[,c(seq(2,16,by=2),17,seq(20,36,by=2),40,42)]
  
  #names
  nms_i<-c("Year","Record_ID","ID","Name","RID","Employer","Employer_Parent","Industry_Ideo","Date_Amt",
           "City","State","Zip","R_Type","Tran_Type","Cmte_ID","Other_ID","Gender","Micro","Employer","Source")
  colnames(inds)<-nms_i
  
  #separate date from amt (Note: original text file read in does not separate date from amount contributed)
  dt<-unlist(strsplit(inds$Date_Amt,","))
  m<-matrix(dt,ncol=3,byrow=T)
  inds$Date<-m[,2]
  inds$Amt<-m[,3]
  
  #create id:
  inds$Donor_ID<-paste(inds$Name,inds$Zip,sep="_")
  inds$Ent_Typ<-"IND"
  
  #add data for recipient type
  inds$Recip_Type<-as.character(vlc$Ent_Typ[match(inds$RID,vlc$Actor_ID)])
  
  #If this results in NA (b/c no match with cand cmte ID), match on cand ID
  inds$Recip_Type<-ifelse(is.na(inds$Recip_Type),as.character(vlc$Ent_Typ[match(inds$RID,vlc$Assoc_Cand_ID)]),inds$Recip_Type)
  
  #CREATE EL
  #donor, recipient, amt, date, trans type, donor type,recipient type
  indel<-inds[,c(23,5,22,21,14,24,25)]
  
  #names for EL
  el_nms<-c("Donor_ID","Recip_ID","Amt","Date","Tran_Tp","Donor_Tp","Recip_Tp")
  colnames(indel)<-el_nms
  
  write.csv(indel,paste("../data/Indivs_Contributions_",yr,".csv",sep=""),row.names=F)
  
  #######################
  #pac contribution files
  #######################
    pacs<-fread(paste("../data/pacs",yr,".txt",sep=""),header=F,sep="|",fill = T)
  
  #get relevant columns and name
    pacs<-pacs[,c(seq(2,8,by=2),9,10,12,14,16)]
  
  colnames(pacs)<-c("Year","Record_ID","PAC_ID","Cand_Cmte_ID","Date_Amt","Industry","Trans_Type",
            "IE","CID")
 
  #########
  #deal with date/amount issue
  pacs$Date_Amt2<-ifelse(substr(pacs$Date_Amt, (nchar(pacs$Date_Amt)-1),
                  nchar(pacs$Date_Amt))== ",,", substr(pacs$Date_Amt, 1, nchar(pacs$Date_Amt)-1),pacs$Date_Amt)
  pacs$Date_Amt<-pacs$Date_Amt2

  #separate date from amt (NOTE: in some instances, Date_Amt2 is ,Amt,Date, while in
  #others it is ,Date,Amt,)
    dt<-unlist(strsplit(pacs$Date_Amt,","))
    m<-matrix(dt,ncol=3,byrow=T)
      m[,3]<-ifelse(m[,1]=="", m[,3], m[,2])
      m[,2]<-ifelse(m[,1]=="", m[,2], m[,1])
    
    pacs$Date<-m[,3]
    pacs$Amt<-m[,2]

    #gsub between first ,, for thsoe with date ="", then remove that string, then grab nchar -1
    amt<-str_match(pacs$Date_Amt2, ",(.*?),")
    pacs$Amt2<-amt[,2]
    
    pacs$Date<-ifelse(pacs$Date=="",pacs$Amt,pacs$Date)
    pacs$Amt<-pacs$Amt2

  #####
  pacs$Ent_Typ<-"PAC"

  #add data for recipient type
  pacs$Recip_Type<-as.character(vlc$Ent_Typ[match(pacs$Cand_Cmte_ID,vlc$Assoc_Cand_ID)])
  #match on cand ID for nas
  pacs$Recip_Type<-ifelse(is.na(pacs$Recip_Type),as.character(vlc$Ent_Typ[match(pacs$Cand_Cmte_ID,vlc$Actor_ID)]),pacs$Recip_Type)
  
  #CREATE EL
  #donor, recipient, amt, date, trans type, donor type,recipient type
    pacel<-pacs[,c(3,4,12,11,7,14,15)]
  
  #names for EL
  el_nms<-c("Donor_ID","Recip_ID","Amt","Date","Tran_Tp","Donor_Tp","Recip_Tp")
  colnames(pacel)<-el_nms
  
  write.csv(pacel,paste("../data/PAC_Contributions_",yr,".csv",sep=""),row.names=F)
  
  EL<-rbind(pacel,indel)
  write.csv(EL,paste("../data/EL_",yr,".csv",sep=""),row.names=F)

  
