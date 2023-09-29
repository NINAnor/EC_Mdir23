###########################################
###
### Expoloring the options for "pivoting" the EC variables 
### in the "utredning tistand" project  
### 
### R script by Balint
###  Sep 2023
###
###########################################

library(tidyverse)
library(googledrive)
library(googlesheets4)

vv="02" #version number
source("pivoting/pivot_init-02.R") #initialise constants & helper functions
# setwd("C:/Users/balint.czucz/OneDrive - NINA/_nina/fagsystem_tilstand/")

# connect to the "metadatabase" spreadsheet:
sht <-   drive_get("https://docs.google.com/spreadsheets/d/1nmg1yT1rbQBVCIoAJ8y-ID1q5RTnJlTUsKwP1UA-DVM") %>%
  {print(sheet_names(.)); I(.)} ### tilstand_metadb
1
1


###
### Load in candidate EC vars from the metadb
###

dat0 <- range_read(sht, "main", skip=7, col_types="c", na=character()) %>%  # read in raw table  
  select(-starts_with("..")) %>%
  filter(i.id0!="") %>%      # remove empty lines
  filter(h.clean(tmp1)!="1") %>% # remove "deleted" lines
  filter(h.clean(tmp1)!="2") %>% # remove "temporarily deleted" lines (TODO: check these!)
  rename(cls=ect.p, chr=grp) %>% #a bit more harmonised names (cls: ECT class, chr: ECT subclass/characteristic(-group))
  {.}

dat1 <- dat0 %>% # cleanup & add missing vars
  mutate(d.startdate2= as.integer(d.startdate)) %>%                                       
  mutate(d.temp.res2=  as.integer(d.temp.res)) %>%                                         
  mutate(d.spat.res2=  h.extract(d.spat.res, l.spat.res) %>% ordered(l.spat.res)) %>%      
  mutate(i.etlink2=    h.extract(i.etlink, l.etlink)) %>%                                    
  mutate(r.dir2=       h.extract(r.dir, l.dir)) %>%                                             
  # mutate(i.cpl2=       h.extract(i.cpl, l.cpl)) %>%   # i.cpl (simplified)
  mutate(i.cpl2=       str_remove(i.cpl, fixed("."))) %>%                                       
  mutate(ect.s2= map2(ect.s1, ect.s2,    # secondary ECT types
                     ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank)) %>%
           map_chr(~str_c(.x, collapse=","))) %>%
  mutate(i.host2= i.host %>% 
           str_remove_all("\\([^)]*\\)") %>%  # remove everything in parentheses
           str_remove_all(fixed("*")) %>%
           str_split("[,&+]") %>%
           map(h.clean) %>% map(trimws) %>% map(h.deblank) %>% map(sort) %>%
           map(~.x[!str_detect(.x, "Various")]) %>%  # remove an annoying label (for "misc")
           map_chr(~str_c(.x, collapse=","))) %>%
  mutate(id.no2= i.id0 %>%     # id.0 #TODO: swap to i.replaces
           map(~(str_split_1(.x, ","))) %>% 
           map(trimws) %>% map(sort) %>%
           map_chr(~str_c(.x, collapse=","))) %>%
  {.} %>%   # TODO: ect.no
  # mutate(novar= map(i.id0, ~(str_split_1(.x, ",") %>% trimws %>% h.deblank))) %>% 
  pack(ETs= n1:nM) %>% mutate(ETs=ETs %>% as.list %>% list_transpose) %>%     # transforming ET status codes into a nested vector 
  ##################################################################################### creating "status codes" for the Estat ETs: 
  mutate(ETs= map(ETs, function(x) c(x, a=h.et.agac(x[c("n1")])))) %>%                #a, ur: Settlements and other artificial areas
  mutate(ETs= map(ETs, function(x) c(x, b=h.et.agac(x[c("n21")])))) %>%               #b, cr: Croplands
  mutate(ETs= map(ETs, function(x) c(x, c=h.et.agac(x[c("n22","n51","n52")])))) %>%   #c, gr: Grasslands
  mutate(ETs= map(ETs, function(x) c(x, e=h.et.agac(x[c("n31","n32")])))) %>%         #e, fo: Forests and woodlands
  mutate(ETs= map(ETs, function(x) c(x, f=h.et.agac(x[c("n53","n54")])))) %>%         #f, sh: Heathland and shrub
  mutate(ETs= map(ETs, function(x) c(x, g=h.et.agac(x[c("n55","n6")])))) %>%          #g, sv: Sparsely vegetated ecosystems
  mutate(ETs= map(ETs, function(x) c(x, h=h.et.agac(x[c("n41")])))) %>%               #h, iw: Inland wetlands
  mutate(ETs= map(ETs, function(x) c(x, j=h.et.agac(x[c("n71")])))) %>%               #j, rc: Rivers and Canals
  mutate(ETs= map(ETs, function(x) c(x, k=h.et.agac(x[c("n72")])))) %>%               #k, lr: Lakes and reservoirs
  mutate(ETs= map(ETs, function(x) c(x, i=h.et.agac(x[c("nC")])))) %>%                #i, co: Coastal beaches, dunes and wetlands
  mutate(ETs= map(ETs, function(x) c(x, l=h.et.agac(x[c("n8a","n8b","n42")])))) %>%   #l, tw: Marine inlets and transitional waters
  mutate(ETs= map(ETs, function(x) c(x, m=h.et.agac(x[c("n8a","n8b")])))) %>%         #m, ma: Marine ecosystems
  select(-starts_with("tmp"), -ends_with(".old")) %>%
  mutate(all="all") %>% # aux col putting all ECTs in a single group 
  {.}
# "abc (xyv 12) 173+a (...!)" %>% str_remove_all("\\([^)]*\\)") 

if (FALSE) { # optional testing phase...
  dat1 %>% pluck("i.name.en",9) # Lirype
  dat1 %>% pluck("ETs",9)
  dat1 %>% pluck("ETs",9) %>% h.et.getS
  dat1 %>% pluck("ETs",9) %>% h.et.getp
  dat1 %>% pluck("ETs",9) %>% names
  head(select(dat1,i.name.no)) #---> TODO: correct encoding!
  dat1 %>% select(d.spat.res, i.etlink, r.dir, i.cpl) %>% map(table)
  dat1 %>% select(d.spat.res2) %>% map(table)
  dat1 %>% select(d.spat.res2, i.etlink2, r.dir2, i.cpl2) %>% map(table)
  dat1 %>% select(i.host2, ect.s2) %>% map(table)
  dat1 %>% pluck("i.name.no",3)
  dat1 %>% select("i.name.no") %>% head %>% write_csv("tmp-balint/test.csv") #UTF8-ban vannak a szovegek, excelben nem jol nyilik :(
  }


###
### Load the Estat (proto)vars (=data needs) from the metadb
###
est0 <- range_read(sht, "estat", skip=4, col_types="c", na=character()) %>%  # potentially requested mandatory&voluntary EC indicators (by ESTAT)
  select(-starts_with(".")) %>%
  filter(bc.ID!="") %>%      # remove empty lines
  # filter(h.clean(tmp1)!="1") %>% # remove "deleted" lines
  rename(cls=ect.p, chr=grp, r.dir=Dir) %>% #a bit more harmonised names
  {.}
est1 <- est0 %>% #cleanup
  pack(ETs= a:nM) %>% mutate(ETs=ETs %>% as.list %>% list_transpose) %>%
  # mutate(across(n1:nM, ~ordered(case_when(str_detect(.x, fixed("!")) ~"m", .x!="" ~"v" , .default =""), levels=c("","v","m")))) %>%
  mutate(id.e.all= ETs %>% map(~(.x[lkp.et.eu$et])) %>% map(h.deblank) %>% map(trimws) %>% map(sort) %>% map_chr(~str_c(.x,collapse=","))) %>% #TOCHK: remove the "!" showing mandatoryness?  
  mutate(r.dir2= h.extract(r.dir, l.dir)) %>%
  mutate(i.cpl2= str_remove(i.cpl, fixed("."))) %>%                                       
  mutate(ect.s2= map2(ect.s1, ect.s2,    # secondary ECT types
                      ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank)) %>%
           map_chr(~str_c(.x, collapse=","))) %>%
  mutate(all="all") %>% # aux col putting all ECTs in a single group 
  {.}


###
### flexibly aggregate the info on candidate EC vars 
###

l.chr <- dat1$chr %>% unique %>% sort 
# gg.lkp <- tibble(gg= "all", .col= "all") %>%
#   bind_rows(tibble(gg= l.ect, .col= "cls")) %>%
#   bind_rows(tibble(gg= l.chr, .col= "chr")) 
# deframe(gg.lkp)
gg.cols <- c("all", "cls", "chr")
et.cols <- c("all", lkp.et.eu$et)

# et_="b"   # <any et name> // all
# gg_="all" # chr // cls // all
res0 <- NULL

for (et_ in et.cols) {  ### [13] iterate across ETs (each type of the EU typology: et.eu / "all" )
  for (gg_ in gg.cols) {  ### [3//34] (indirectly) iterate across "EC chars" (groups of vars as defined on tab "cw": chr / cls / "all")
    # for (gg_ in c("all", l.ect, l.chr)) {  ### [34] iterate across "EC chars" (groups of vars as defined on tab "cw": chr / cls / "all")
    # .col <- h.lkp(gg_, gg.lkp)
    
    # .st = dat1$ETs %>% map_chr(~pluck(.x, et)) %>% unname
    dat1.ch <- dat1 %>% ##### dat1: Norwegian vars ||ETs contains "status codes" (st)
      mutate(.st = if(et_=="all") i.st else ETs %>% map_chr(~pluck(.x, et_)) %>% unname) %>% # the st of indis for the chosen ET (a temp column)
      mutate(.pl = i.cpl=="") %>% # the "plus" argument for the h.summarise function calls 
      mutate(gg = .data[[gg_]]) %>% #TODO: "{{gg_}}" if supplied as fn argument! 
      group_by(gg) %>%
      summarise(nn=        h.summarise(.st, .st), 
                nn_c2=     h.summarise(.st, .st, NULL, "C2", .pl),
                st_max=    h.summarise(h.et.getS(.st), .st, max),  #inf
                st0=       h.summarise(as.character(h.et.getS(.st)), .st, as.character(1:3)), #packed cols
                # dir_all.n= h.summarise(r.dir2, .st, ~str_c(.x, collapse="")), 
                dir0.n=    h.summarise(r.dir2, .st, l.dir), #packed cols
                tsd=       h.summarise(d.startdate2, .st, ~min(.x, na.rm=T)), 
                tsd_c2=    h.summarise(d.startdate2, .st, ~min(.x, na.rm=T), "C2", .pl), 
                trs=       h.summarise(d.temp.res2, .st, ~min(.x, na.rm=T)), 
                trs_c2=    h.summarise(d.temp.res2, .st, ~min(.x, na.rm=T), "C2", .pl),   
                srs=       h.summarise(d.spat.res2, .st, ~min(.x, na.rm=T)), 
                srs_c2=    h.summarise(d.spat.res2, .st, ~min(.x, na.rm=T), "C2", .pl),   
                etl0=      h.summarise(i.etlink2, .st, l.etlink), #packed cols
                cpl_ok.n=  h.summarise(if_else(i.cpl2=="",1,0), .st, sum), #compliant count
                cpl0.n=    h.summarise(i.cpl2, .st, l.cpl),  #packed cols
                ect_s.n=   h.summarise(ect.s2,  .st, h.concat, .na=""),
                hosts=     h.summarise(i.host2, .st, h.concat, .na=""),
                iid.n=     h.summarise(id.no2,  .st, h.concat, .na="")) %>%
      # separate_wider_delim(gg, " | ", names=c("cls", "echar"), cols_remove=F) %>%
      mutate(dir_d.n= dir0.n %>% as.list %>% list_transpose %>% map2_chr(nn, ~c(names(.x)[.x/.y >= 2/3],"x")[1])) %>% 
      mutate(et=et_, lvl=gg_) %>% relocate(et,lvl,gg) %>%
      {.}
    # dat1.ch$dir0.n %>% as.list %>% list_transpose %>% # pluck(9)
    #   map2_chr(nn, ~names(.x)[.x/.y >= 1/3]) %>% {c(.,"x")[1]}
      
    est1.ch <- est1 %>% ##### est1: the Estat vars (info needs) || ETs contains "indicator ids" (ii)
      mutate(.ii = if(et_=="all") id.e.all else ETs %>% map_chr(~pluck(.x, et_)) %>% unname) %>% # indi ids (and mandatoryness) for the chosen ET (a temp column)
      # pluck(".ii") %>% str
      mutate(gg = .data[[gg_]]) %>% #TODO: "{{gg_}}" if supplied as fn argument! 
      group_by(gg) %>%
      summarise(nm=         h.summarise(str_count(.ii, fixed("!")), 1, sum), 
                nv=         h.summarise(str_count(.ii, fixed(".")), 1, sum),
                # dir_all.e=  h.summarise(r.dir2, 1, ~str_c(.x, collapse="")), 
                dir0.e=     h.summarise(r.dir2, 1, l.dir), #packed cols
                cpl_ok.e=   h.summarise(if_else(i.cpl2=="",1,0), 1, sum), #compliant count
                cpl0.e=     h.summarise(i.cpl2, 1, l.cpl),  #packed cols
                ect_s.e=    h.summarise(ect.s2,  1, h.concat, .na=""),
                iid.e =     h.summarise(.ii,  1, h.concat, .na="")) %>%
      mutate(nv=nv-nm) %>% # to avoid double counting of the mandatory ones...
      mutate(dir_d.e= dir0.e %>% as.list %>% list_transpose %>% map2_chr(nm+nv, ~c(names(.x)[.x/.y >= 2/3],"x")[1])) %>% 
      mutate(et=et_, lvl=gg_) %>% relocate(et,lvl,gg) %>%
      {.}
    
    tmp <- dat1.ch %>%   ##### tmp: merging the info on "available & needed"
      full_join(est1.ch, by=c("et","lvl","gg")) %>%
      mutate(across(where(~typeof(.x)!="list"), h.clean)) %>% # eliminate NAs (I have to, because of structural NAs)
      mutate(across(where(~typeof(.x)=="list"), ~(map(.x, h.clean) %>% as_tibble))) %>%
      mutate(n= nn+nv+nm) %>%
      mutate(cpl_ok =  cpl_ok.n + cpl_ok.e) %>% # (the full share will be (cpl_ok.n+cpl_ok.e)/(nn+nv+nm))
      # mutate(dir_all = str_c(dir_all.n,dir_all.e)) %>%
      mutate(cpl0 = as_tibble(as.matrix(cpl0.n)+as.matrix(cpl0.e))) %>%
      mutate(dir0 = as_tibble(as.matrix(dir0.n)+as.matrix(dir0.e))) %>%
      rowwise %>% mutate(ect_s = h.concat(c(ect_s.n, ect_s.e))) %>%
      mutate(id = h.concat(c(iid.n, iid.e))) %>% ungroup %>%
      {.}
    
    res0 <- bind_rows(res0, tmp)
    # res0 <- append(res0, list(tmp))
    }
  }

tmp <- lkp.et.eu %>% select(et, et_label) %>% deframe %>% c(all="All ecosystem types",.)

res1 <- res0 %>%
  filter(!(lvl=="chr" & n==0)) %>%  #drop the irrelevant characteristics 
  mutate(et_label= str_c(et, tmp[et], sep=": ")) %>%
  mutate(lvl= ordered(lvl, levels=gg.cols)) %>%
  mutate(et= ordered(et, levels=et.cols)) %>%
  arrange(et, lvl, gg) %>%
  {.}

res1 %>% 
  unpack(where(~typeof(.x)=="list"), names_sep=".") %>%
  write_csv(str_c("tmp-balint/res_full-", vv, format(now(), "-%m%d-%H%M.csv")), na="")





