#Danae Hernandez-Cortes hernandezcortes@ucsb.edu
#INFRASTRUCTURE TO SEND TO THE MODELING TEAM

# Updated 2/14/2024 by MG 
# Updated 4/3 MP

#Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(maptools)
library(raster)
library(rgeos)
library(plyr)
library(reshape2)
library(purrr)
library(foreign)
library(haven)
library(readr)
library(dplyr)
library(janitor)
# rm(list=ls())


# setting working directory 
setwd('/capstone/freshcair/meds-freshcair-capstone')

# UPDATED - MG
# (0) Load CES3.0
ces3<-read_csv("data/inputs/health/ces3results_part.csv")

ces3 <- ces3 %>% 
  janitor::clean_names()
ces3$GEOID=paste("0",as.character(ces3$census_tract),sep="")

ct_total_pop<-ces3%>%dplyr::select("GEOID","total_population")

ct_dac_pop<-ces3%>%dplyr::filter(sb535_dac == 'Yes')%>%
  dplyr::select("GEOID","total_population","sb535_dac")%>%
  dplyr::rename(dac_population=total_population)

demographics<-left_join(ct_total_pop,ct_dac_pop)
# (1.1) Load source receptor matrix (srm) #######

#Extraction

fields_vector <- c(1:26)

field_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction'





read_extraction <- function(buff_field){
  
  bfield <- buff_field
  
  nh3<-read_csv(paste0(field_data,"/nh3/srm_nh3_field",bfield,".csv",sep=""))%>%mutate(poll="nh3")
  nox<-read_csv(paste0(field_data,"/nox/srm_nox_field",bfield,".csv",sep=""))%>%mutate(poll="nox")
  pm25<-read_csv(paste0(field_data,"/pm25/srm_pm25_field",bfield,".csv",sep=""))%>%mutate(poll="pm25")
  sox<-read_csv(paste0(field_data,"/sox/srm_sox_field",bfield,".csv",sep=""))%>%mutate(poll="sox")
  voc<-read_csv(paste0(field_data,"/voc/srm_voc_field",bfield,".csv",sep=""))%>%mutate(poll="voc")
  
  all_polls<-rbind(nh3,nox,pm25,sox,voc)
  
  all_polls$field=bfield
  
  tmp<-as.data.frame(all_polls) 
  
  return(tmp)
  
}

#build extraction srm
srm_all_pollutants_extraction <-map_df(fields_vector, read_extraction) %>% 
  bind_rows()%>%
  dplyr::rename(weighted_totalpm25=totalpm25_aw)%>%
  dplyr::select(-totalpm25)%>%
  spread(poll, weighted_totalpm25)%>%
  dplyr::rename(weighted_totalpm25nh3=nh3,
                weighted_totalpm25nox=nox,
                weighted_totalpm25pm25=pm25,
                weighted_totalpm25sox=sox,
                weighted_totalpm25voc=voc,
                id=field)


srm_all_pollutants_extraction$total_pm25=srm_all_pollutants_extraction$weighted_totalpm25nh3+srm_all_pollutants_extraction$weighted_totalpm25nox+srm_all_pollutants_extraction$weighted_totalpm25pm25+srm_all_pollutants_extraction$weighted_totalpm25sox+srm_all_pollutants_extraction$weighted_totalpm25voc

# Join with total population
srm_all_pollutants_extraction_population<-left_join(srm_all_pollutants_extraction,demographics)


#Obtain measure 1: filtering if positive pm2.5
measure1<-srm_all_pollutants_extraction_population%>%
  dplyr::filter(total_pm25>0.0001)
measure1_by_cluster<-measure1%>%
  dplyr::group_by(id)%>%
  dplyr::summarize(total_population=sum(total_population,na.rm=T),dac_population=sum(dac_population,na.rm=T))

measure1_by_cluster$share_dac=measure1_by_cluster$dac_population/measure1_by_cluster$total_population


#Obtain measure 2: population weighting by PM2.5
measure2<-srm_all_pollutants_extraction_population%>%
  dplyr::select(GEOID,id,total_pm25,total_population,dac_population)

measure2$dac_population<-ifelse(is.na(measure2$dac_population)==TRUE,0,measure2$dac_population)

measure2$numA=measure2$total_pm25*measure2$total_population

measure2$numD=measure2$total_pm25*measure2$dac_population

measure2_by_cluster<-measure2%>%dplyr::group_by(id)%>%
  dplyr::summarize(numA=sum(numA,na.rm=T),numD=sum(numD,na.rm=T),total_pm25=sum(total_pm25))

measure2_by_cluster$share_dac_weighted=measure2_by_cluster$numD/measure2_by_cluster$numA



measures<-left_join(measure1_by_cluster,measure2_by_cluster,by="id")

measures<-measures%>%
  dplyr::select(id,share_dac,share_dac_weighted,numA)

setwd('/capstone/freshcair/meds-freshcair-capstone')
write_csv(measures,"data/processed/extraction_cluster_affectedpop.csv")


######EXTRACTION FIELDS
#LOAD AND PROCESS X-WALK BETWEEN FIELDS AND CLUSTERS
# UPDATED - MG -3/19
setwd('/capstone/freshcair/meds-freshcair-capstone')

extraction_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/extraction-model'

extraction_field_clusters_10km<-read_csv(paste0(extraction_data,"/extraction_fields_clusters_10km.csv",sep=""))%>%
  dplyr::select(OUTPUT_FID,INPUT_FID)

extraction_field_clusters_10km<-dplyr::rename(extraction_field_clusters_10km,id=OUTPUT_FID,input_fid=INPUT_FID)

extraction_fields_xwalk<-read.dbf(paste0(extraction_data,"/extraction_fields_xwalk_id.dbf",sep=""))

extraction_fields_xwalk<-dplyr::rename(extraction_fields_xwalk,input_fid=id,doc_field_code=dc_fld_)

extraction_xwalk<-left_join(extraction_field_clusters_10km,extraction_fields_xwalk,by=c("input_fid"))

extraction_xwalk$doc_field_code=as.numeric(as.character(extraction_xwalk$doc_field_code))


setwd('/capstone/freshcair/meds-freshcair-capstone')


write_csv(extraction_xwalk,"data/processed/extraction_xwalk.csv")




# 
# # alternate way of reading in field data - MG 
# 
# # -----------nh3 data------------
# nh3_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction/nh3'
# 
# 
# nh3_files <- fs::dir_ls(nh3_data, regexp = '\\.csv$')
# 
# 
# nh3 <- nh3_files %>% 
#   map_dfr(read_csv)
# 
# 
# # -----------nox data --------------
# nox_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction/nox'
# 
# nox_files <- fs::dir_ls(nox_data, regexp = '\\.csv$')
# 
# nox <- nox_files %>% 
#   map_dfr(read_csv)
# 
# 
# # -----------pm 2.5 data ------------------
# pm_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction/pm25'
# 
# pm_files <- fs::dir_ls(pm_data, regexp = '\\.csv$')
# 
# pm25 <- pm_files %>% 
#   map_dfr(read_csv)
# 
# 
# # --------- sox data ------------------------
# sox_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction/sox'
# 
# sox_files <- fs::dir_ls(sox_data, regexp = '\\.csv$')
# 
# sox <- sox_files %>% 
#   map_dfr(read_csv)
# 
# # ----------------voc data -------------------
# 
# voc_data <- '/capstone/freshcair/meds-freshcair-capstone/data/intermediate-zenodo/intermediate/inmap-processed-srm-extraction/voc'
# 
# voc_files <- fs::dir_ls(voc_data, regexp = '\\.csv$')
# 
# voc <- voc_files %>% 
#   map_dfr(read_csv)
# 

# all_polls <- rbind(nh3,nox,pm25,sox,voc)