library(data.table)
library(boxr)
library(readr)

box_auth()

##################
## Cand/PAC Info - create vertex level data
#################

# Candidates
c_names = c("Year", "ID", "CID", "Name", "Party", "District", "Current_Dist", 
          "Active", "Active2", "Incum_Won", "Party_Won", "NoPacs")
## Read 'cands16.txt' from box
cands = box_read(file_id = '302056263793', read_fun = read_delim, delim = ',',
                 col_names = c_names, quote = '|')

cands$won = substr(cands$Party_Won, 2, 2)
  
## only retain year, cmte ID, cand ID, name, party, district, incumbency/winner
cands = cands[, c(1:6, 10, 13)]
  
## party - change all 3rd party to 3
cands$Party[!cands$Party %in% c("D", "R", "I")] = "3"
  
# Supplement missing incumbency data
## Read 'cn16_fec.txt' from box
yy = box_read(file_id = '302057933693', read_fun = read_delim, delim = '|',
              col_names = FALSE)
cands$Incum_Won[!cands$Incum_Won %in% c("C", "I", "O")] = NA
cands$Incum_Won = ifelse(is.na(cands$Incum_Won),
                         as.character(yy$X8[match(cands$ID, yy$X1)]),
                         as.character(cands$Incum_Won))
rm(yy)
  
# PACS
c_names  = c("Year", "ID", "Name", "Affiliate", "Parent_Org", "RID", "R_Type", 
             "CID", "Party", "Industry", "Source", "Multi_Industry", "Foreign", 
             "Active")
## Read 'cmtes16.txt' from box
cmtes = box_read(file_id = '302061819630', read_fun = read_delim, delim = ',',
                 quote = '|', col_names = c_names)
  
## only retain year, cmte ID, name, cand ID (or cmte ID if not cand), 
## party/pac type
cmtes = cmtes[, c(1:3, 8, 10)]
  
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
          dir_id = '50855821402')

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
# If file available locally, use this code:
ind_contribs = read_delim('~/Downloads/indivs16.txt', delim = ',', quote = '|',
                  col_names = c_names) %>%
#inds = box_read(file_id = '302063495065', read_fun = read_delim, 
#                delim = ',', quote = '|', col_names = c_names) %>%
    mutate(Donor_ID = paste(Name, Zip, sep = '_'))
inds = ind_contribs %>%
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
box_write(vlc, filename = 'VLC_16.csv', write_fun = write_csv,
          dir_id = '50855821402')

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
inds$Ent_Typ = 'IND'
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
pacs = box_read(file_id = '302150361527', read_fun = read_delim, delim = ',',
                quote = '|', col_names = c_names)

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
## Write files to box in dir 'Strategic_Donors/final_paper_data/'
box_write(EL, filename = 'EL_16.csv', write_fun = write_csv, 
          dir_id = '50855821402')