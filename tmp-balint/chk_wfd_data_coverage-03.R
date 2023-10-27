
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
library(sf)

h.clean <- function(x) {if (mode(x)=="character") #helper fun to remove NAs (and also line breaks from texts)
  ret <- trimws(ifelse(is.na(x),"",gsub("[\r\n]", " ", x))) else ifelse(is.na(x), 0L, x)            
  }

h.lkp <- function(x, tab) { #simple lookup of values of vector x in the second column of data.frame (or matrix) tab
  tab[[2]][match(as.character(x), as.character(tab[[1]]))]
  }      

###
### Load in candidate EC vars from the metadb
###
# setwd("C:/Users/balint.czucz/OneDrive - NINA/_nina/fagsystem_tilstand/wfd_data")
sht <-   drive_get("https://docs.google.com/spreadsheets/d/1nmg1yT1rbQBVCIoAJ8y-ID1q5RTnJlTUsKwP1UA-DVM") %>%
  {print(sheet_names(.)); I(.)} ### tilstand_metadb
1
1

vars0 <- range_read(sht, "main", skip=7, col_types="c") %>%  # list of potential EC vars from the googlesheet 
  select(-starts_with("..")) %>%
  # filter(i.contrib=="AKS") %>% #only WFD 
  filter(!is.na(tmp7)) %>% #only WFD vars with a known "list of dependencies"
  filter(is.na(tmp1)) %>% #"deleted" lines have something in col C (tmp1 for now)
  {.}

vars1 <- vars0 %>% # filtered var list, one line per indicator (not just "indi family"!) 
  mutate(i.vmvars=tmp7) %>%
  filter(i.vmvars!="") %>% #drop the vars that are not linked to any vannmiljo parameterID (n=2)  
  filter(!grepl("+",i.vmvars,fixed=T)) %>% #drop the vars filtered also by other response fields beyond parameterID (n=3) #TODO: solve this 
  mutate(R=ifelse(is.na(n71),NA,1L), L=ifelse(is.na(n72),NA,1L), C=ifelse(is.na(n8a),NA,1L)) %>% # rivers, lakes, coast
  select(i.id0, i.name.en, i.name.no, i.vmvars, R, L, C) %>%
  pivot_longer(all_of(c("R","L","C")), values_drop_na = TRUE, names_to= "kat") %>% #repeat each line for each ET separately  
  select(-value) %>%
  mutate(vmv2=strsplit(i.vmvars,", ",fixed=T))

vmpar1 <- vars1 %>% {.$vmv2} %>% do.call(c,.) %>% unique %>% sort # all relevant params in VM 
# vmpar1 %>% dput

lkp.akt <- range_read(sht, "vm_aktivitet", skip=0, col_types="c") %>%  # list of activity codes + "biasedness" scores 
  select(-starts_with("..")) %>%
  select(ActivityID, bias=final, Name) %>%
  {.}



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

# V$kommune %>% table

# load municipality polygons
lau <- st_read("shp/Fylker_Kommuner_2020.shp") %>%  # the only one I found that has both fylkes & municips...  
  select(kommunenum, fylkesnumm, area=SHAPE_Area, geometry)
# plot(lau %>% st_geometry)

# add municipality & fylke IDs as columns to VL
VL <- VL %>%
  # head(10000) %>%
  filter(!is.na(X)) %>%  # thr are 15 locations with missing coordinates
  st_as_sf(coords=c("X","Y"), agr="constant", crs=25833) %>%  # epsg: 25833 (ETRS89 / UTM zone 33N)
  mutate(tmp= st_intersects(.,lau) %>% map_int(~c(.x,NA)[1])) %>%
  mutate(komm= lau$kommunenum[tmp], fylk=lau$fylkesnumm[tmp]) %>%
  # st_drop_geometry %>%
  select(-tmp)
# table(tmp$fylk)
# table(tmp$komm)


### 
### Load a simplified list of datain Vannmiljø (VM)
###

load("wfd_data/Vannmilj_ferskvann.RData") #data in Vannmilj_ferskvann
dat0 <- Vannmilj_ferskvann %>%
  # tail(10000) %>%
  filter(is.na(UnntasKlassifisering)) %>% # filter out the ones marked for unntas
  select(Vannlok_kode, Tid_provetak, Parameter_id, Aktivitet_id) %>%
  mutate(abias= h.lkp(Aktivitet_id, lkp.akt)) %>%
  mutate(abias=case_match(abias, "0"~0L, "1"~1, "-1"~1, .default=2)) %>%
  mutate(yr = Tid_provetak %>% lubridate::ymd_hms() %>% lubridate::year()) %>%
  mutate(ymd = Tid_provetak %>% str_sub(1,10)) %>%
  mutate(survid= str_c(Vannlok_kode,"-",ymd)) %>%
  filter(yr >= 2010) %>%  #limit the year range
  mutate(id=h.lkp(Vannlok_kode, select(VL, lokkod, id))) %>%
  mutate(fylk=h.lkp(Vannlok_kode, select(VL, lokkod, fylk))) %>%
  mutate(komm=h.lkp(Vannlok_kode, select(VL, lokkod, komm))) %>%
  mutate(kat1=str_sub(id,-1,-1)) %>%
  mutate(kat2=h.lkp(id, select(V, id, kat))) %>%
  mutate(kat=kat1 %>% case_match("C"~"C","L"~"L","R"~"R",.default="X")) %>%
  mutate(typ=h.lkp(id, select(V, id, typ))) %>%
  mutate(reg=h.lkp(id, select(V, id, reg))) %>%
  # mutate(omr=h.lkp(id, select(V, id, starts_with("omr")))) %>%
  mutate(geometry=h.lkp(Vannlok_kode, select(VL, lokkod, geometry))) %>% st_as_sf %>%
  {.}
# str(dat0)
# dat0 %>% st_drop_geometry %>% count(fylk)
# dat0 %>% st_drop_geometry %>% count(komm)
# dat0 %>% st_drop_geometry %>% count(yr)
# dat0 %>% st_drop_geometry %>% count(Parameter_id)
# dat0 %>% st_drop_geometry %>% count(abias)
# dat0 %>% st_drop_geometry %>% count(kat, kat2)
# dat0 %>% filter(kat=="R") %>% sample_n(10000) %>% st_geometry %>% plot()
# dat0 %>% filter(kat=="L") %>% sample_n(10000) %>% st_geometry %>% plot()
# dat0 %>% filter(kat=="C") %>% st_geometry %>% plot()
# dat0 %>% filter(kat=="1") %>% st_geometry %>% plot() #for some strange reasons there are quite some locations where the "id" ends in a number

rm(Vannmilj_ferskvann); gc() #to free up memory

load("wfd_data/Vannmilj_saltvann.RData") #data in Vannmilj_saltvann
tmp <- Vannmilj_saltvann %>%
  # tail(10000) %>%
  filter(is.na(UnntasKlassifisering)) %>% # filter out the ones marked for unntas
  select(Vannlok_kode, Tid_provetak, Parameter_id, Aktivitet_id) %>%  #Just the minimum necessary fields
  mutate(abias= h.lkp(Aktivitet_id, lkp.akt)) %>%
  mutate(abias=case_match(abias, "0"~0L, "1"~1, "-1"~1, .default=2)) %>%
  mutate(yr = Tid_provetak %>% lubridate::ymd_hms() %>% lubridate::year()) %>%
  mutate(ymd = Tid_provetak %>% str_sub(1,10)) %>%
  mutate(survid= str_c(Vannlok_kode,"-",ymd)) %>%
  filter(yr >= 2010) %>%  #limit the year range
  mutate(id=h.lkp(Vannlok_kode, select(VL, lokkod, id))) %>%
  mutate(fylk=h.lkp(Vannlok_kode, select(VL, lokkod, fylk))) %>%
  mutate(komm=h.lkp(Vannlok_kode, select(VL, lokkod, komm))) %>%
  mutate(kat1=str_sub(id,-1,-1)) %>%
  mutate(kat2=h.lkp(id, select(V, id, kat))) %>%
  mutate(kat=kat1 %>% case_match("C"~"C","L"~"L","R"~"R",.default="X")) %>%
  mutate(typ=h.lkp(id, select(V, id, typ))) %>%
  mutate(reg=h.lkp(id, select(V, id, reg))) %>%
  # mutate(omr=h.lkp(id, select(V, id, starts_with("omr")))) %>%
  mutate(geometry=h.lkp(Vannlok_kode, select(VL, lokkod, geometry))) %>% st_as_sf %>%
  {.}
# tmp %>% st_drop_geometry %>% count(fylk)
# tmp %>% st_drop_geometry %>% count(komm)
# tmp %>% st_drop_geometry %>% count(yr)
# tmp %>% st_drop_geometry %>% count(Parameter_id)
# tmp %>% st_drop_geometry %>% count(abias)
# tmp %>% st_drop_geometry %>% count(kat, kat2)
# tmp %>% filter(kat=="C") %>% sample_n(10000) %>% st_geometry %>% plot()
# tmp %>% filter(kat=="L") %>% st_geometry %>% plot()
# tmp %>% filter(kat=="R") %>% st_geometry %>% plot()
# tmp %>% filter(kat=="7") %>% st_geometry %>% plot() #for some strange reasons there are quite some locations where the "id" ends in a number
# lau %>% st_geometry %>% plot(border="grey90")
# tmp %>% filter(is.na(kat)) %>% st_geometry %>% plot(add=T) #for some strange reasons there are quite some locations where the "id" ends in a number
# tmp %>% filter(is.na(kat)) %>%  st_drop_geometry %>% count(Parameter_id)

rm(Vannmilj_saltvann); gc() #to free up memory
dat0 <- dat0 %>% bind_rows(tmp) 
rm(tmp) #to free up memory

###
### plotting diagnostic maps
###
allpars <- dat0$Parameter_id %>% unique %>% c(vmpar1) %>% unique %>% sort
ctry <- st_read("shp/Nasjon.shp") %>%
  st_geometry # country polygon for plotting
# plot(ctry)

# pp="KLFA"
pdf(format(now(), "tmp-balint/maps1-%m%d-%H%M.pdf"), paper="a4", width=8, height=11)
for (pp in allpars) {
  dd <- dat0 %>% filter(Parameter_id==pp)
  vv <- vars1 %>% 
    filter(map_lgl(vmv2,~(pp %in% .x))) %>% pluck("i.id0") %>% unique
  lab <- str_c(pp," (",nrow(dd),") -- used for ",str_c(vv, collapse=", "))  
  ctry %>% plot(border="grey90", main=lab)
  if (nrow(dd)==0) next
  if (nrow(dd) > 2000) dd <- slice_sample(dd, n=2000, by=kat) 
  dd <- slice_sample(dd, prop=1) #random order
  dd %>% st_geometry %>% plot(add=T, col=c(C="brown",L="blue",R="green",X="red")[dd$kat])
  }
dev.off()



### 
### Assess the evenness of coverage for the VM params behind the vars 
###
# b01:  min number of (distinct) surveys in a "B1 bin" (municipality) for considering it "well-covered" (in data)
# b12:  min number of "well-covered B1 bins" to count a B2 bin (fylke) "well-covered"
# b23:  min number of "well-covered B2 bins" to consider the country "well-covered" in a period 
# tle:  the length of accounting periods in yrs (=the "temporal size" of bins)
# tth:  the % of accounting periods that need to be well-covered in order to accept a variable

### TODO: it wd be good to divide n by area and turn th1 into a density threshold

setup <- expand_grid(b01=c(1,2,3,5), b12=c(1,2,3,5), b23=c(1,2,3,5), tle=c(1,3,5,10), tth=.33) %>% #experimenting a bit with the best threshold values 
  # mutate(th_= paste0("th_",th1,"_",th2)) %>%
  {.} 
# out1c <- out1; out2c <- out2
out1 <- out2 <- NULL

dat1 <- dat0 %>%
  st_drop_geometry %>%
  select(kat, param=Parameter_id, abi=abias, yr, komm, fylk, survid) %>%
  filter(!is.na(kat))

gc()
# ii=128; jj=6; setup[ii,]
for (ii in 1:nrow(setup)) {  #main cycle
  b01 <- setup$b01[ii]; b12 <- setup$b12[ii]; b23 <- setup$b23[ii]
  tle <- setup$tle[ii]; tth <- setup$tth[ii]
  res <- vars1 %>% # df for storing the results from one round 
    mutate(b01=b01, b12=b12, b23=b23, tle=tle, tth=tth) %>%
    # mutate(tmp1=list(NULL), tmp2=list(NULL), tmp3=list(NULL)) %>%
    mutate(pa0=0, pa1=0, pa2=0) #pass/fail with different tolerance for bias (0: zero, 2: anything goes) 
  thresh_n <- max(round(10/tle*tth),1) # pmax(round(10/c(1,3,5,10)*tth),1) #[1] 3 1 1 1
  # jj <- which(res$i.id0=="AIP")[1]; res[jj,]
  # b01=1; b12=2; b23=3; tle=5
  cat(format(Sys.time(),"%H:%M:%S"), "Calculating for ", b01, b12, b23, tle, "\n")
  for (jj in 1:nrow(res)) {
    tmp0 <- dat1 %>%
      filter(kat == res$kat[jj]) %>%
      filter(param %in% res$vmv2[jj][[1]]) %>%
      mutate(yr= yr %/% tle * tle) %>%  # 3-5 yr accounting periods coded with their start year 
      group_by(komm, fylk, yr, abi) %>%  #number of distinct survey event per catchment x acct.period 
      summarise(n0= n_distinct(survid), .groups="drop") %>% 
      filter(!is.na(komm)) #remove the cases with a missing bin
    # sum(tmp0$n0)
    tmp0b <- tmp0 %>% 
      pivot_wider(names_from=abi, names_prefix="ab", values_from=n0, values_fill=0L) %>% 
      mutate(ab2= if ("ab2" %in% colnames(.)) ab2 else 0) %>% 
      mutate(ab1= if ("ab1" %in% colnames(.)) ab1 else 0) %>% 
      mutate(ab0= if ("ab0" %in% colnames(.)) ab0 else 0) %>% 
      mutate(ab2= ab2+ab1+ab0) %>% 
      mutate(ab1= ab1+ab0) %>%
      pivot_longer(starts_with("ab"), names_to="abi", values_to="n0")
    if (nrow(tmp0)==0) next
    # table(tmp0$n0)
    # table(tmp0$yr,tmp0$n0)
    # table(tmp0$fylk,tmp0$n0)
    # sum(tmp0b$n0)
    tmp1 <- tmp0b %>% 
      group_by(fylk, yr, abi) %>%  #n of "well-covered B1 bins" (municip) per yr & B2 fylke
      summarise(n1=sum(n0>=b01), .groups="drop")
    # table(tmp1$n1)
    # table(tmp1$yr,tmp1$n1)
    # table(tmp1$fylk,tmp1$n1)
    # sum(tmp1$n1)
    if (sum(tmp1$n1)==0) next
    tmp2 <- tmp1 %>% 
      group_by(yr, abi) %>%  #n of "well-covered B2 bins" (fylk) per yr
      summarise(n2=sum(n1>=b12), .groups="drop")
    if (sum(tmp2$n2)==0) next
    # table(tmp2$n2)
    # table(tmp2$yr,tmp2$n2)
    #TODO: check options for a "density" threshold (dividing the number of data points w the area of the område) 
    #   or at least dividing by the number of available bins ("feasible bins": coastal ones for the coast...) 
    tmp3 <- tmp2 %>% 
      group_by(abi) %>%  
      summarise(n3=sum(n2>=b23), .groups="drop") %>%
      with(set_names(n3,abi))
    res$pa0[jj] <- as.integer(tmp3["ab0"] >= thresh_n)
    res$pa1[jj] <- as.integer(tmp3["ab1"] >= thresh_n)
    res$pa2[jj] <- as.integer(tmp3["ab2"] >= thresh_n)
    }
  
  tmp <- res %>%
    select(any_of(names(setup)), i.id0, kat, i.name.en, i.vmvars, starts_with("pa")) #%>%
    # mutate(tmp3= map(tmp3, function(x) x$n3 %>% t %>% as.data.frame %>% set_names(x$reg))) %>%
    # select(th1, th2, i.id0, kat, i.name.en, i.vmvars, nn1, tmp3) %>%
    # unnest(tmp3) %>%
    # mutate(across(nn1:H, h.clean))
    
  out1 <- out1 %>%
    bind_rows(tmp)
  }
cat(format(Sys.time(),"%H:%M:%S"), "Done.\n")

out2 <- out1 %>% 
  pivot_longer(pa0:pa2, names_to="abi", values_to="pass") %>%
  mutate(th_= str_c("s",b01,b12,b23,tle, sep="_")) %>%
  select(i.id0, kat, i.name.en, i.vmvars, abi, th_, pass) %>%
  pivot_wider(names_from="th_", values_from="pass") %>%
  rowwise %>% mutate(pass= mean(c_across(starts_with("s_")))) %>% ungroup %>%
  arrange(abi, desc(pass), kat) %>%
  relocate(abi, kat, i.id0, i.name.en, i.vmvars, pass, starts_with("s_"))

out2 %>%
  write_csv(str_c("tmp-balint/out2-",format(now(), "-%m%d-%H%M.csv"),".csv"), na="")



