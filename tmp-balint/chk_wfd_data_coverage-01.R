
###########################################
###
### Expolring data coverage for the WFD ecol status indicators 
### in the vannmiljø database
### Data downloaded by Hanno, 
### R script by Balint
###  Aug 2023
###
###########################################

library(tidyverse)
library(googledrive)
library(googlesheets4)

h.clean <- function(x) if (mode(x)=="character") #remove NAs (and also line breaks from texts)
  ret <- trimws(ifelse(is.na(x),"",gsub("[\r\n]", " ", x))) else ifelse(is.na(x), 0L, x)            

setwd("C:/Users/balint.czucz/OneDrive - NINA/_nina/fagsystem_tilstand/wfd_data")
sht <-   drive_get("https://docs.google.com/spreadsheets/d/1nmg1yT1rbQBVCIoAJ8y-ID1q5RTnJlTUsKwP1UA-DVM") %>%
  {print(sheet_names(.)); I(.)} ### tilstand_metadb
1
1

load("Vannmilj_ferskvann.RData") #data in Vannmilj_ferskvann
dat0 <- Vannmilj_ferskvann %>%
  # head(1000) %>%
  mutate(vassdr=map_chr(strsplit(Vannlok_kode,"-"),first)) %>%
  mutate(place_time_id= paste0(Vannlok_kode, Tid_provetak)) %>%
  mutate(tid = lubridate::ymd_hms(Tid_provetak)) %>%
  mutate(yr = lubridate::year(tid)) %>%
  {.}
# rm(Vannmilj_ferskvann)
str(dat0)
table(dat0$yr)
# table(dat0$UnntasKlassifisering)

vars0 <- range_read(sht, "main", skip=7, col_types="c") %>% 
  # rename(colB = tmp1) %>%
  select(-starts_with("..")) %>%
  filter(i.contrib=="AKS") %>% #only WFD 
  filter(h.clean(tmp1)!="x") %>% #"deleted" lines have "x" in col B (tmp1 for now)
  {.}

vars1 <- vars0 %>%
  mutate(i.vmvars=i.replaces, et=paste0(n71,n72,n8)) %>% # 1..:rivers, .1.: lakes, ..1:kyst
  select(i.id0, et, i.name.en, i.name.no, i.vmvars) %>%
  filter(i.vmvars!="") %>% #drop the vars that are not linked to any vannmiljo parameterID (n=2)  
  filter(!grepl("+",i.vmvars,fixed=T)) %>% #drop the vars filtered also by other response fields beyond parameterID (n=3) 
  mutate(vmv2=strsplit(i.vmvars,", ",fixed=T))

out <- vars1 %>%
  filter(substr(et,1,2) %in% c("10","01","11")) %>% #only the ones for freshwater now
  mutate(tmp1=list(NULL), tmp2=list(NULL), tmp3=NA_integer_)

th1=  5 # min number of surveys (per catchmt x yr) to count ctm as aqdequate (in yr)
th2= 20 # min number of adequate catchmts (per yr) to count yr as an adequate 
### TODO: it wd be good to divide n by area and turn th1 into a density threshold

# i=15
for (i in 1:nrow(out)) {
  tmp1 <- dat0 %>%
    filter(Parameter_id %in% out$vmv2[i]) %>%
    filter(yr > 2010) %>%
    group_by(yr, vassdr) %>%
    summarise(n1=n_distinct(place_time_id)) #number of distinct survey event per catchment x year
  tmp2 <- tmp1 %>% summarise(n2=sum(n1>th1)) #n of "adequate catchmts" per yr
  tmp3 <- tmp2 %>% summarise(n3=sum(n2>th2)) #n of "adequate years" since 2010 (single scalar, max 12)
  out$tmp1[i] <- list(ungroup(tmp1))
  out$tmp2[i] <- list(tmp2)
  out$tmp3[i] <- tmp3$n3 
  }

out2 <- out %>%
  select(n, i.id0, tmp3, i.vmvars, i.name.en)

out2




