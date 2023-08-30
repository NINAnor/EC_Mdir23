
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

h.clean <- function(x) {if (mode(x)=="character") #helper fun to remove NAs (and also line breaks from texts)
  ret <- trimws(ifelse(is.na(x),"",gsub("[\r\n]", " ", x))) else ifelse(is.na(x), 0L, x)            
  }

h.lkp <- function(x, tab) { #simple lookup of values of vector x in the second column of data.frame (or matrix) tab
  tab[[2]][match(as.character(x), as.character(tab[[1]]))]
  }      

###
### Load in candidate EC vars from the metadb
###
setwd("C:/Users/balint.czucz/OneDrive - NINA/_nina/fagsystem_tilstand/wfd_data")
sht <-   drive_get("https://docs.google.com/spreadsheets/d/1nmg1yT1rbQBVCIoAJ8y-ID1q5RTnJlTUsKwP1UA-DVM") %>%
  {print(sheet_names(.)); I(.)} ### tilstand_metadb
1
1

vars0 <- range_read(sht, "main", skip=7, col_types="c") %>%  # list of potential EC vars from the googlesheet 
  select(-starts_with("..")) %>%
  filter(i.contrib=="AKS") %>% #only WFD 
  filter(h.clean(tmp1)!="x") %>% #"deleted" lines have "x" in col B (tmp1 for now)
  {.}

vars1 <- vars0 %>% # filtered var list, one line per indicator (not just "indi family"!) 
  mutate(i.vmvars=i.replaces) %>%
  filter(i.vmvars!="") %>% #drop the vars that are not linked to any vannmiljo parameterID (n=2)  
  filter(!grepl("+",i.vmvars,fixed=T)) %>% #drop the vars filtered also by other response fields beyond parameterID (n=3) 
  mutate(R=ifelse(n71=="1",1L,NA), L=ifelse(n72=="1",1L,NA), C=ifelse(n8=="1",1L,NA)) %>% # rivers, lakes, coast
  select(i.id0, i.name.en, i.name.no, i.vmvars, R, L, C) %>%
  pivot_longer(all_of(c("R","L","C")), values_drop_na = TRUE, names_to= "kat") %>% #repeat each line for each ET separately  
  select(-value) %>%
  mutate(vmv2=strsplit(i.vmvars,", ",fixed=T))


###
### Load in the lookup for vannlokaliteiter 
###
load("wfd_data/VF+VL.RData")
# VL: vannlokaliteiter
#   -- lokkod: an id matching the Vannlok_kode in dat0
#   -- id: WFD water body ID
# V:  water body descriptors
#   -- id: WFD water body ID
#   -- typ & kat-str: water body charateristics (described in the Veileder --> p22 and beyond)
#   -- kat: L: lake, R: river/stream, C: coast
#   -- reg: NO ecoregions (adm regions w the north divided into two parts)
#   -- område: vannområder (smaller areal units of relatively similar size...)

### 
### Load a simplified list of datain Vannmiljø (VM)
###

load("wfd_data/Vannmilj_ferskvann.RData") #data in Vannmilj_ferskvann
dat0 <- Vannmilj_ferskvann %>%
  # tail(10000) %>%
  select(Vannlok_kode, Tid_provetak, Parameter_id, Aktivitet_id) %>%
  mutate(bin_id= paste0(Vannlok_kode, Tid_provetak)) %>%
  mutate(yr = Tid_provetak %>% lubridate::ymd_hms() %>% lubridate::year()) %>%
  filter(yr > 2010) %>%  #limit the year range
  mutate(id=h.lkp(Vannlok_kode, select(VL, lokkod, id))) %>%
  mutate(kat=h.lkp(id, select(V, id, kat))) %>%
  mutate(typ=h.lkp(id, select(V, id, typ))) %>%
  mutate(reg=h.lkp(id, select(V, id, reg))) %>%
  mutate(omr=h.lkp(id, select(V, id, starts_with("omr")))) %>%
  {.}
rm(Vannmilj_ferskvann) #to free up memory
# str(dat0)
# table(dat0$yr)
# table(dat0$UnntasKlassifisering)

load("wfd_data/Vannmilj_saltvann.RData") #data in Vannmilj_saltvann
tmp <- Vannmilj_saltvann %>%
  # tail(10000) %>%
  select(Vannlok_kode, Tid_provetak, Parameter_id, Aktivitet_id, UnntasKlassifisering) %>%  #Just the minimum necessary fields
  #TODO: filter for aktivitet & unntas as well
  mutate(bin_id= paste0(Vannlok_kode, Tid_provetak)) %>%
  mutate(yr = Tid_provetak %>% lubridate::ymd_hms() %>% lubridate::year()) %>%
  filter(yr > 2010) %>%  #limit the year range
  mutate(id=h.lkp(Vannlok_kode, select(VL, lokkod, id))) %>%
  mutate(kat=h.lkp(id, select(V, id, kat))) %>%
  mutate(typ=h.lkp(id, select(V, id, typ))) %>%
  mutate(reg=h.lkp(id, select(V, id, reg))) %>%
  mutate(omr=h.lkp(id, select(V, id, starts_with("omr")))) %>%
  {.}
dat0 <- dat0 %>% bind_rows(tmp)
rm(Vannmilj_saltvann) #to free up memory

# table(dat0$kat)
# str(dat0)


### 
### Assess the evenness of coverage for the VM params behind the vars 
###
# th1: min number of (distinct) surveys in "bins" defined by vannområde & year for considering the bin as "well-covered" (in data)
# th2: min number of "well-covered bins" (områdes in a specific yr) to count the yr as "well-covered" 
### TODO: it wd be good to divide n by area and turn th1 into a density threshold

setup <- expand_grid(th1=c(3,5,10), th2=c(3,5,10)) %>% #experimenting a bit with the best threshold values 
  # mutate(th_= paste0("th_",th1,"_",th2)) %>%
  {.} 
out1 <- out2 <- NULL

# ii=4; jj=6
for (ii in 1:nrow(setup)) {  #main cycle
  th1 <- setup$th1[ii]  # th1 & th2 in the ii.th round of experiment
  th2 <- setup$th2[ii]
  res <- vars1 %>% # df for storing the results from one round 
    mutate(th1=th1, th2=th2) %>%
    mutate(tmp1=list(NULL), tmp2=list(NULL), tmp3=list(NULL), nn1=NA_integer_) 
  cat(format(Sys.time(),"%H:%M:%S"), "Calculating for ", th1, th2, "\n")
  for (jj in 1:nrow(res)) {
    tmp1 <- dat0 %>%
      filter(kat == res$kat[jj]) %>%
      filter(Parameter_id %in% res$vmv2[jj]) %>%
      # filter(yr > 2010) %>%
      group_by(reg, yr, omr) %>%  #number of distinct survey event per catchment x year
      summarise(n1=n_distinct(bin_id), .groups="drop") %>% 
      filter(!is.na(reg)) #remove the cases with a missing region
    tmp2 <- tmp1 %>% 
      group_by(reg, yr) %>%  #n of "well-covered områder" per yr & (eco)region
      summarise(n2=sum(n1>th1), .groups="drop")
    #TODO: it would be better to apply a threshold for the density of data points (dividing their number w the area of the område) 
    tmp3 <- tmp2 %>% 
      group_by(reg) %>%      #n of "well-covered years" (since 2010) per region 
      summarise(n3=sum(n2>th2), .groups="drop")
    #TODO: it would be better to apply a threshold for the % of omrades in the ecoregion (dividing w the total number of områder) 
    res$tmp1[jj] <- list(tmp1)
    res$tmp2[jj] <- list(tmp2)
    res$tmp3[jj] <- list(tmp3)
    res$nn1[jj]  <- mean(tmp3$n3) #mean across the ecoregions
    #TODO: check & ensure that regions w no data do not bias the mean...
    }
  
  tmp <- res %>%
    mutate(tmp3= map(tmp3, function(x) x$n3 %>% t %>% as.data.frame %>% set_names(x$reg))) %>%
    select(th1, th2, i.id0, kat, i.name.en, i.vmvars, nn1, tmp3) %>%
    unnest(tmp3) %>%
    mutate(across(nn1:H, h.clean))
    
  out1 <- out1 %>%
    bind_rows(tmp)
  }
cat(format(Sys.time(),"%H:%M:%S"), "Done.\n")
  
out2 <- out1 %>%
  mutate(th_= paste0(kat,"_",th1,"_",th2)) %>%
  select(i.id0, i.name.en, i.vmvars, th_, nn1) %>%
  pivot_wider(names_from="th_", values_from=nn1) %>%
  relocate(i.id0, i.name.en, i.vmvars, starts_with("R"), starts_with("L"), starts_with("C"))

out2 %>%
  write_csv("tmp-balint/out2.csv", na="")




