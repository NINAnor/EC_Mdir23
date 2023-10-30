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

vv="06" #version number
# source(str_c("pivoting/pivot_init-",vv,".R")) #initialise constants & helper functions
source(str_c("pivoting/pivot_init-05",".R")) #initialise constants & helper functions
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
  mutate(d.startdate2= d.startdate %>% str_remove_all(fixed("?")) %>% as.integer) %>%                                       
  mutate(d.temp.res2=  d.temp.res %>% str_remove_all(fixed("?")) %>% {if_else(.=="irr", Inf, as.integer(.))}) %>%   # Inf is the irregular / unknown repeat period                                      
  mutate(d.spat.res2=  h.extract(d.spat.res, l.spat.res) %>% ordered(c(l.spat.res,"NA")) %>% replace_na("NA")) %>%  # "NA" is the "highest"/"worst" level for sorting
  mutate(i.etlink2=    h.extract(i.etlink, l.etlink)) %>%                                    
  mutate(r.dir2=       h.extract(r.dir, l.dir)) %>%                                             
  mutate(i.cpl2=       str_remove(i.cpl, fixed("."))) %>%                                       
  # mutate(i.cpl3=       h.extract(i.cpl, l.cpl1)) %>%   # i.cpl (simplified)
  mutate(ect.s2= map2(ect.s1, ect.s2,    # secondary ECT types
                     ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank)) %>%
           map_chr(~str_c(.x, collapse=","))) %>%
  mutate(i.host2= i.host %>% 
           str_remove_all("\\([^)]*\\)") %>%  # remove everything in parentheses
           str_remove_all(fixed("*")) %>%
           str_split("[,&+]") %>%
           # map(h.clean) %>% map(~replace_na(.x,"")) %>% map(trimws) %>% map(h.deblank) %>% map(sort) %>%
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

dat1 %>% saveRDS("dat1.rds")


###
### Load the Estat (proto)vars (=data needs) from the metadb
###
est0 <- range_read(sht, "estat", skip=4, col_types="c", na=character()) %>%  # potentially requested mandatory&voluntary EC indicators (by ESTAT)
  select(-starts_with("."), -starts_with("v5.id2")) %>%
  filter(bc.v5id!="") %>%      # remove empty lines
  # filter(h.clean(tmp1)!="1") %>% # remove "deleted" lines
  rename(cls=ect.p, chr=grp, r.dir=Dir) %>% #a bit more harmonised names
  {.}
est1 <- est0 %>% #cleanup
  pack(ETs= a:nM) %>% mutate(ETs=ETs %>% as.list %>% list_transpose) %>%
  mutate(id.e.all= ETs %>% map(~(.x[lkp.et.eu$et])) %>% map(h.deblank) %>% map(trimws) %>% map(sort) %>% map_chr(~str_c(.x,collapse=","))) %>% #TOCHK: remove the "!" showing mandatoryness?  
  mutate(r.dir2= h.extract(r.dir, l.dir)) %>%
  mutate(i.cpl2= str_remove(i.cpl, fixed("."))) %>%                                       
  # mutate(i.cpl3= h.extract(i.cpl, l.cpl1)) %>%   # i.cpl (simplified)
  mutate(ect.s2= map2(ect.s1, ect.s2,    # secondary ECT types
                      ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank)) %>%
           map_chr(~str_c(.x, collapse=","))) %>%
  mutate(all="all") %>% # aux col putting all ECTs in a single group 
  {.}
# est1 %>% select(r.dir2) %>% map(table)

est1 %>% saveRDS("est1.rds")



###
### quick dumps from the metadb (following the structure of Erik's demo forest table)
###

lkp.cpl <- range_read(sht, "names_dl", skip=1, col_types="c", na=character()) %>%  # read in i.cpl codes  
  select(-starts_with("..")) %>%
  filter(grp=="i.cpl") %>%      # remove empty lines
  select(code, label.short, label) %>% #a bit more harmonised names (cls: ECT class, chr: ECT subclass/characteristic(-group))
  {.}

stab.li <- list(urb="n1", opn=c("n21","n22","n51","n52","n53","n54","n55","n6","nC","nM"),
                `for`=c("n31","n32"), wet="n41", frw=c("n71","n72"), mar=c("n8a","n8b"))
# opn: j jordbruksmark, g grasmark, h hei og buskmark, l lite vegetert mark, 
#   k terrestriske kystøkosystemer, f fjell, Å åpen fastmark under skoggrensa unntatt jordbruksmark.

# max(h.et.getS(dat1$ETs[[1]][stab.li[[ii]]]))
# ii="opn"
out1 <- NULL
for (ii in names(stab.li)) {
  tmp <- dat1 %>%
    mutate(.st = ETs %>% map_int(~max(h.et.getS(.x[stab.li[[ii]]]), na.rm=T)) %>% unname) %>% # 
    filter(.st>0) %>%
    select(-ETs) %>%
    # mutate(i.cpl2b= if_else(i.cpl2=="", "yes","no")) %>%
    mutate(d.spat.res2b= l2.spat.res[d.spat.res2] %>% unname) %>%
    rowwise %>% mutate(ECT2= c_across(c(cls, ect.s1, ect.s2)) %>% h.deblank %>% str_c(collapse=",") %>% str_replace_all(fixed(","),", ")) %>% ungroup %>%
    mutate(ch43tab= ii) %>%
    mutate(i.cpl.issues= i.cpl %>% str_remove(fixed(".")) %>% str_split("") %>% map(~h.lkp(.x,lkp.cpl)) %>% map_chr(~str_c(.x, collapse=", "))) %>%
    mutate(i.comment1= {.[[str_c("t.",ii,".m")]]}) %>%
    mutate(ET= if (ii %in% c("opn","frw")) {.[[str_c("t.",ii,".e")]]} else "") %>%
    mutate(i.comments.all=str_c(main.comments, "###", comments.from.notes, sep=" ")) %>%
    mutate(.st=as.character(.st)) %>%
    select(ch43tab, i.id0, ET, i.name.no, 
           Status=.st, Datakilde= i.host, `Startar`= d.startdate2, 
           `Frekvens (ar)`= d.temp.res2, `Romlig opplosning`= d.spat.res2b, 
           ECT= cls, ECT2, Egenskap= ect.no, Dir= r.dir, 
           Ref.verdi= r.opt, Merknad= i.comment1,
           i.description, i.cpl.issues, i.comments.all) 
  tmp2 <- est1 %>%
    mutate(.ii = ETs %>% map_chr(~str_c(unique(h.deblank(.x[stab.li[[ii]]])), collapse=", ")) %>% unname) %>% # 
    select(-ETs) %>%
    mutate(.iin = str_count(.ii, fixed("."))/2) %>% # number of indis (mand+vol) for each record (=family) (EACH ID has two dots now!!)
    mutate(.iiM = str_count(.ii, fixed("!"))) %>% # number of mandatory indis for each record (=family)
    filter(.iin>0) %>%
    mutate(MV= if_else(.iiM>0, "M","")) %>%
    mutate(MV= if_else(.iin>.iiM, str_c(MV,"V"), MV)) %>%
    mutate(d.spat.res2b= "") %>%
    rowwise %>% mutate(ECT2= c_across(c(cls, ect.s1, ect.s2)) %>% h.deblank %>% str_c(collapse=",") %>% str_replace_all(fixed(","),", ")) %>% ungroup %>%
    mutate(ch43tab= ii) %>%
    mutate(i.cpl.issues= i.cpl %>% str_remove(fixed(".")) %>% str_split("") %>% map(~h.lkp(.x,lkp.cpl)) %>% map_chr(~str_c(.x, collapse=", "))) %>%
    # mutate(i.comment1= "") %>% #{.[[str_c("t.",ii,".m")]]}) %>%
    mutate(ET= "", i.host="", d.startdate2=NA_integer_, d.temp.res2=NA_real_, ect.no="", r.opt="", i.comment1="") %>%
    mutate(i.comments.all=str_c(h.clean(`own notes to v4`), "###", h.clean(`notes to v5`), sep=" ")) %>%
    select(ch43tab, i.id0=.ii, ET, i.name.no=fam.name.short, 
           Status=MV, Datakilde= i.host, `Startar`= d.startdate2, 
           `Frekvens (ar)`= d.temp.res2, `Romlig opplosning`= d.spat.res2b, 
           ECT= cls, ECT2, Egenskap= ect.no, Dir= r.dir, 
           Ref.verdi= r.opt, Merknad= i.comment1,
           i.description=fam.name.long, i.cpl.issues, i.comments.all) 

  out1 <- bind_rows(out1, tmp, tmp2) 
  }

out1 %>% 
  write_excel_csv2(str_c("tmp-balint/simple-table-", vv, format(now(), "-%m%d-%H%M.csv")), na="")




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

# et_="e"   # <any et name> // all
# gg_="chr" # chr // cls // all
res0 <- NULL

for (et_ in et.cols) {  ### [13] iterate across ETs (each type of the EU typology: et.eu / "all" )
  for (gg_ in gg.cols) {  ### [3//34] (indirectly) iterate across "EC chars" (groups of vars as defined on tab "cw": chr / cls / "all")

    # .st = dat1$ETs %>% map_chr(~pluck(.x, et_)) %>% unname
    dat1.ch <- dat1 %>% ##### dat1: Norwegian vars ||ETs contains "status codes" (st)
      mutate(.st = if(et_=="all") i.st else ETs %>% map_chr(~pluck(.x, et_)) %>% unname) %>% # the st of indis for the chosen ET (a temp column)
      mutate(.pl = i.cpl=="") %>% # the "plus" argument for the h.summarise function calls 
      mutate(gg = .data[[gg_]]) %>% #TODO: "{{gg_}}" if supplied as fn argument! 
      group_by(gg) %>%
      summarise(nn=        h.summarise(.st, .st), 
                nn_c1=     h.summarise(.st, .st, NULL, "C1"), # N of vars that cover the whole ET
                nn_p2=     h.summarise(.st, .st, NULL, "P2"), # N of vars that are at or above status 2
                nn_p1p=    h.summarise(.st, .st, NULL, "P1", .pl), # N of vars that fully comply w SEEA EA 
                nn_c2p=    h.summarise(.st, .st, NULL, "C2", .pl), # N of vars that meet all 3 above ("readily available")
                st_max=    h.summarise(h.et.getS(.st), .st, max),  #inf
                st0=       h.summarise(as.character(h.et.getS(.st)), .st, as.character(1:3)), #packed cols
                dir0.n=    h.summarise(r.dir2, .st, l.dir), #packed cols
                tsd=       h.summarise(d.startdate2, .st, ~min(.x, na.rm=T)), 
                tsd_c2p=   h.summarise(d.startdate2, .st, ~min(.x, na.rm=T), "C2", .pl), 
                trs=       h.summarise(d.temp.res2, .st, ~min(.x, na.rm=T)), 
                trs_c2p=   h.summarise(d.temp.res2, .st, ~min(.x, na.rm=T), "C2", .pl),  
                trs0=      h.summarise(d.temp.res2 %>% as.character %>% paste0("y",.), .st, 
                                       d.temp.res2 %>% unique %>% sort %>% as.character %>% paste0("y",.) %>% c("NA"), .na="NA"), # packed
                # srs=       h.summarise(d.spat.res2, .st, ~min(fct_c(.x,factor("")), na.rm=T)), 
                srs=       h.summarise(d.spat.res2, .st, ~min(.x, na.rm=T), .na=vctrs::vec_cast("NA", d.spat.res2)), 
                srs_c2p=   h.summarise(d.spat.res2, .st, ~min(.x, na.rm=T), "C2", .pl, .na=vctrs::vec_cast("NA", d.spat.res2)),   
                srs0=      h.summarise(d.spat.res2, .st, d.spat.res2 %>% levels), # packed
                etl0=      h.summarise(i.etlink2, .st, l.etlink), #packed cols
                cpl_ok.n=  h.summarise(if_else(i.cpl2=="",1,0), .st, sum), #compliant count
                # cpl0.n=    h.summarise(i.cpl3,  .st, l.cpl1),  #packed cols
                cpl0.n=    h.summarise(i.cpl2,  .st, ~str_c(.x, collapse=""), .na=""),  #packed cols
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
      mutate(.ii  = if(et_=="all") id.e.all else ETs %>% map_chr(~pluck(.x, et_)) %>% unname) %>% # indi ids (and mandatoryness) for the chosen ET (a temp column)
      mutate(.iin = str_count(.ii, fixed("."))/2) %>% # number of indis (mand+vol) for each record (=family)
      mutate(.iiM = str_count(.ii, fixed("!"))) %>% # number of mandatory indis for each record (=family)
      # pluck(".iin") %>% sign %>% print
      mutate(gg = .data[[gg_]]) %>% #TODO: "{{gg_}}" if supplied as fn argument! 
      group_by(gg) %>%
      summarise(nm=         h.summarise2(.iiM, sign(.iin), sum, 0), 
                nv=         h.summarise2(.iin, sign(.iin), sum, 0),
                dir0.e=     h.summarise2(r.dir2, .iin, l.dir), #packed cols
                cpl_ok.e=   h.summarise2(if_else(i.cpl2=="",1, 0), .iin, sum, 0), #compliant count
                cpl0.e=     h.summarise2(i.cpl2, sign(.iin), ~str_c(.x, collapse=""), .na=""),  #concat
                ect_s.e=    h.summarise2(ect.s2, .iin, h.concat, .na=""),
                iid.e =     h.summarise2(.ii,  sign(.iin), h.concat, .na="")) %>%
      mutate(nv=nv-nm) %>% # to avoid double counting of the mandatory ones...
      mutate(dir_d.e= dir0.e %>% as.list %>% list_transpose %>% map2_chr(nm+nv, ~c(names(.x)[.x/.y >= 2/3],"x")[1])) %>% 
      mutate(et=et_, lvl=gg_) %>% relocate(et,lvl,gg) %>%
      {.}
    # est1.ch %>% select(cpl0.e) %>% map(table)
    
    tmp <- dat1.ch %>%   ##### tmp: merging the info on "available & needed"
      full_join(est1.ch, by=c("et","lvl","gg")) %>%
      mutate(across(where(~!(typeof(.x)=="list"|"factor" %in% class(.x))), h.rena)) %>% # eliminate NAs (I have to, because of structural NAs)
      mutate(across(where(~typeof(.x)=="list"), ~(map(.x, h.rena) %>% as_tibble))) %>%
      mutate(n= nn+nv+nm) %>%
      mutate(cpl_ok =  cpl_ok.n + cpl_ok.e) %>% # (the full share will be (cpl_ok.n+cpl_ok.e)/(nn+nv+nm))
      # mutate(dir_all = str_c(dir_all.n,dir_all.e)) %>%
      # mutate(cpl0 = as_tibble(as.matrix(cpl0.n)+as.matrix(cpl0.e))) %>%
      mutate(cpl0 = str_c(cpl0.n, cpl0.e)) %>%
      mutate(cpl0 = map(l.cpl1, ~str_count(cpl0, fixed(.x))) %>% set_names(l.cpl1) %>% as_tibble) %>%
      mutate(dir0 = as_tibble(as.matrix(dir0.n)+as.matrix(dir0.e))) %>%
      mutate(dir_d= dir0 %>% as.list %>% list_transpose %>% map2_chr(nm+nv, ~c(names(.x)[.x/.y >= 2/3],"x")[1])) %>% 
      rowwise %>% mutate(ect_s = h.concat(c(ect_s.n, ect_s.e))) %>%
      mutate(iid = h.concat(c(iid.n, iid.e))) %>% ungroup %>%
      {.}
    
    res0 <- bind_rows(res0, tmp)
    # res0 <- append(res0, list(tmp))
    }
  }

tmp <- lkp.et.eu %>% select(et, et_label) %>% deframe %>% c(all="All ecosystem types",.)

res1 <- res0 %>%
  filter(!(lvl=="chr" & n==0)) %>%  #drop the irrelevant characteristics 
  # mutate(et_label= str_c(et, tmp[et], sep=": ")) %>%
  mutate(lvl= ordered(lvl, levels=gg.cols)) %>%
  mutate(et= ordered(et, levels=et.cols)) %>%
  arrange(et, lvl, gg) %>%
  {.}

res1 %>% 
  select(-cpl0.n,-cpl0.e) %>%
  unpack(where(~typeof(.x)=="list"), names_sep=".") %>%
  write_csv(str_c("tmp-balint/res_full-", vv, format(now(), "-%m%d-%H%M.csv")), na="")


###
### make an overview table w 2 sub-tables (rows: ECT_grps, cols: ETs)
# #   A: status codes + them.coverage
# #   B: M / v

tmp1 <- res1 %>% 
  filter(lvl=="chr") %>%
  filter(gg!="" & str_sub(gg,1,2)!="X ") %>%
  filter(et!="all") %>%
  mutate(value=if_else(nn==0,"",str_c(st_max, if_else(nn_c1==0,"'","")))) %>%
#   mutate(et=as.character(et)) %>%
#   select(gg,et,value) 
# tmp1 %>%
#   group_by(gg, et) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   filter(n > 1L) 
# tmp1 %>%
  pivot_wider(id_cols= gg, names_from= et, names_prefix= "st.", values_from= value, values_fill="") 
tmp2 <- res1 %>% 
  filter(lvl=="chr") %>%
  filter(gg!="" & str_sub(gg,1,2)!="X ") %>%
  filter(et!="all") %>%
  mutate(value=if_else(nm>0,"M",if_else(nv>0,"v",""))) %>%
  pivot_wider(id_cols= gg, names_from= et, names_prefix= "mv.", values_from=value, values_fill="")
ovr1 <- full_join(tmp1,tmp2) %>%
  arrange(gg) %>%
  mutate(ect= gg %>% str_split_i(fixed("|"),1) %>% trimws) %>%
  relocate(ect) %>%
  {.}

ovr1 %>%
  write_csv(str_c("tmp-balint/res_ovr-", vv, format(now(), "-%m%d-%H%M.csv")), na="")

