library(data.table)
library(boxr)
library(readr)

box_auth()

# Config:
LOCAL_DATA = 'data/'
#LOCAL_DATA = NULL

##################
## Cand/PAC Info - create vertex level data
#################

# Candidates
c_names = c("Year", "ID", "CID", "Name", "Party", "District", "Current_Dist", 
          "Active", "Active2", "Incum_Won", "Party_Won", "NoPacs")
## Read 'cands16.txt' from box
if(!is.null(LOCAL_DATA)) {
   cands = read_delim(paste0(LOCAL_DATA, 'cands16.txt'), delim = ',', 
                      quote = '|', col_names = c_names) 
} else {
    cands = box_read(file_id = '302056263793', read_fun = read_delim, delim = ',',
                     col_names = c_names, quote = '|')
}

cands$won = substr(cands$Party_Won, 2, 2)
  
## only retain year, cmte ID, cand ID, name, party, district, incumbency/winner
cands = select(cands, Year, ID, CID, Name, Party, District, Incum_Won, won)
  
## party - change all 3rd party to 3
cands$Party[!cands$Party %in% c("D", "R", "I")] = "3"
  
# Supplement missing incumbency data
## Read 'cn16_fec.txt' from box
if(!is.null(LOCAL_DATA)) {
    yy = read_delim(paste0(LOCAL_DATA, 'cn16_fec.txt'), delim = '|', 
                    col_names = FALSE)
} else {
    yy = box_read(file_id = '302057933693', read_fun = read_delim, delim = '|',
                  col_names = FALSE)   
}

# in yy X1 is candidate ID and X8 is incumbency info
cands = select(yy, X1, X8) %>%
    right_join(cands, by = c('X1' = 'ID')) %>%
    mutate(Incum_Won = ifelse(is.na(Incum_Won), X8, Incum_Won)) %>%
    rename(ID = X1) %>%
    select(-X8)
rm(yy)
  
# PACS
c_names  = c("Year", "ID", "Name", "Affiliate", "Parent_Org", "RID", "R_Type", 
             "CID", "Party", "Industry", "Source", "Multi_Industry", "Foreign", 
             "Active")
## Read 'cmtes16.txt' from box
if(!is.null(LOCAL_DATA)) {
    cmtes = read_delim(paste0(LOCAL_DATA, 'cmtes16.txt'), delim = ',',
                       quote = '|', col_names = c_names)    
} else {
    cmtes = box_read(file_id = '302061819630', read_fun = read_delim, delim = ',',
                     quote = '|', col_names = c_names)
}
 
## only retain year, cmte ID, name, cand ID (or cmte ID if not cand), 
## party/pac type
cmtes = select(cmtes, Year, ID, Name, CID, Industry)
  
# FORMAT CANDS AND CMTES FOR RBIND

## year, cmte ID, cand ID (if applicable), name, party/pac type, 
## district (cands),incumbency (cands),entity type
cands = merge(cands, cmtes, by.x="ID", by.y="CID", all.x=T) #add candidate cmtes
cands = subset(cands, cands$ID!=",")
cands = cands[, c(2, 3, 1, 4:8)]
cands[, 9] = "CAND"

## if cand has no cmte, assign cand id for actor id
cands$ID = ifelse(is.na(cands$ID),
                  as.character(cands$CID),
                  as.character(cands$ID))

#CMTES
cmtes[, 6] = NA
cmtes[, 7] = "PAC"
cmtes = cmtes[, c(1:2, 4, 3, 5, 6, 6, 6, 7)]
cmtes[, 5] = substr(cmtes[, 5], 1, 1)

nms = c("Yr", "Actor_ID", "Assoc_Cand_ID", "Name", "Party_PAC_Type", 
        "District", "Incum", "Winner", "Ent_Typ")
colnames(cands) = nms
colnames(cmtes) = nms

#### add committee info to missing info
cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type, 1, 1) %in% c("A", "B", "C", 
                                                               "D", "E", "F", 
                                                               "G", "H", "K", 
                                                               "M", "T")] = "B"
cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type, 1, 1)=="J"] = "I"
cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type, 1, 1)=="L"] = "L"
cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type, 1, 1)=="X"] = "O"
cmtes$Party_PAC_Type[substr(cmtes$Party_PAC_Type, 1, 1) %in% c("P", "S", "Y", 
                                                               "Z", "R")] = "P"

## augment missing data with FEC data
if(!is.null(LOCAL_DATA)) {
    y = read_delim(paste0(LOCAL_DATA, 'cm16_fec.txt'), delim = '|', 
                   col_names = FALSE)
} else {
    y = box_read(file_id = '302056548034', read_fun = read_delim, delim = '|',
                 col_names = FALSE)
}
y$X13 = as.character(y$X13)
y$X13[y$X13 %in% c("B","T","V","W","C")] = "B"
y$X13[y$X13=="M"] = "I"

cmtes$Party_PAC_Type <- ifelse(cmtes$Party_PAC_Type == "",
                               as.character(y$V13[match(cmtes$Actor_ID,y$V1)]),
                               cmtes$Party_PAC_Type)


##############
# Add individuals
##############

# Read 'indivs16.txt' from box
# Note: This doesn't represent the record ID correctly (64 bit integer) we don't
# use it here but if you want to you have to define the respective conlumn type
# as character
c_names = c("Year", "Record_ID", "ID","Name", "RID", "Employer", "Employer_Parent",
         "Industry_Ideo", "Date", "Amt", "unknown","City", "State", "Zip", "R_Type", 
         "Tran_Type", "Cmte_ID", "Other_ID", "Gender", "Micro", "Position", 
         "Employer_2", "Source")
if(!is.null(LOCAL_DATA)) {
    inds = read_delim(paste0(LOCAL_DATA, 'indivs16.txt'), delim = ',', 
                             quote = '|', col_names = c_names)
} else {
    inds = box_read(file_id = '302063495065', read_fun = read_delim, 
                    delim = ',', quote = '|', col_names = c_names)
}
# Store copy of the data for later use in edgelist construction
ind_contribs = inds

inds = inds %>%
    mutate(Donor_ID = paste(Name, Zip, sep = '_')) %>%
    group_by(Donor_ID) %>%
    summarize(Yr = Year[1],
              Actor_ID = Donor_ID[1],
              Assoc_Cand_ID = NA,
              Name = Name[1],
              Party_PAC_Type = NA,
              District = NA,
              Incum = NA,
              Winner = NA,
              Ent_Typ = 'IND') %>% 
    select(-Donor_ID)

#########################
# CREATE VLC LIST, REMOVE DUPLICATES, WRITE
#########################

vlc = rbind(cands, cmtes, inds)
vlc = subset(vlc, !duplicated(vlc$Actor_ID))

## Write files to box in dir 'Strategic_Donors/final_paper_data/'
if(!is.null(LOCAL_DATA)) {
    write_csv(vlc, paste0(LOCAL_DATA, 'VLC_16.csv'))
} else {
    box_write(vlc, filename = 'VLC_16.csv', write_fun = write_csv,
              dir_id = '50855821402')
}

######################################################
###### CREATE EDGE LIST FROM CONTRIBUTION FILES 
######################################################

#add data for recipient type
inds = ind_contribs
rm(ind_contribs)
inds$Recip_Type = as.character(vlc$Ent_Typ[match(inds$RID, vlc$Actor_ID)])

#If this results in NA (b/c no match with cand cmte ID), match on cand ID
inds$Recip_Type = ifelse(is.na(inds$Recip_Type), as.character(vlc$Ent_Typ[match(inds$RID, vlc$Assoc_Cand_ID)]), inds$Recip_Type)

#CREATE EL
#donor, recipient, amt, date, trans type, donor type,recipient type
inds$Donor_ID<-paste(inds$Name,inds$Zip,sep="_")
inds$Ent_Typ<-"IND"

indel = select(inds, 'Donor_ID', 'RID', 'Amt', 'Date', 'Tran_Type', 
               'Ent_Typ', 'Recip_Type') %>%
    rename(Recip_ID = RID, Tran_Tp = Tran_Type, Donor_Tp = Ent_Typ, 
           Recip_Tp = Recip_Type)

#######################
#pac contribution files
#######################
# Read pacs16.txt from box
# Note: This doesn't represent the record ID correctly (64 bit integer) we don't
# use it here but if you want to you have to define the respective conlumn type
# as character
c_names = c("Year", "Record_ID", "PAC_ID", "Cand_Cmte_ID", "Amt", "Date", 
            "Industry", "Trans_Type", "IE","CID")
if(!is.null(LOCAL_DATA)) {
    pacs = read_delim(paste0(LOCAL_DATA, 'pacs16.txt'), delim = ',', 
                      quote = '|', col_names = c_names)
} else {
    pacs = box_read(file_id = '302150361527', read_fun = read_delim, delim = ',',
                    quote = '|', col_names = c_names)
}
pacs$Ent_Typ = "PAC"

#add data for recipient type
pacs$Recip_Type = as.character(vlc$Ent_Typ[match(pacs$Cand_Cmte_ID, vlc$Assoc_Cand_ID)])
#match on cand ID for nas
pacs$Recip_Type = ifelse(is.na(pacs$Recip_Type), as.character(vlc$Ent_Typ[match(pacs$Cand_Cmte_ID, vlc$Actor_ID)]),pacs$Recip_Type)

#CREATE EL
#donor, recipient, amt, date, trans type, donor type, recipient type
pacel = select(pacs, 'PAC_ID', 'Cand_Cmte_ID', 'Amt', 'Date', 'Trans_Type',
               'Ent_Typ', 'Recip_Type')

#names for EL
colnames(pacel) = c("Donor_ID", "Recip_ID", "Amt", "Date", "Tran_Tp", 
                    "Donor_Tp", "Recip_Tp")

EL = rbind(pacel, indel)
EL_old = read_csv('data/EL_16_old.csv')

if(!is.null(LOCAL_DATA)) {
    write_csv(EL, paste0(LOCAL_DATA, 'EL_16.csv'))
} else {
    ## Write files to box in dir 'Strategic_Donors/final_paper_data/'
    box_write(EL, filename = 'EL_16.csv', write_fun = write_csv,
              dir_id = '50855821402')
}
