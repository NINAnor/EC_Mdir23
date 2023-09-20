
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

h.clean <- function(x) {if (mode(x)=="character") #helper fun to remove NAs (and also line breaks from texts)
  ret <- trimws(ifelse(is.na(x),"",gsub("[\r\n]", " ", x))) else ifelse(is.na(x), 0L, x)            
  }
h.lkp <- function(x, tab) { #simple lookup of values of vector x in the second column of data.frame (or matrix) tab
  tab[[2]][match(as.character(x), as.character(tab[[1]]))]
} 
h.deblank <- function(x) x[x!=""]
h.merge.vars <- function(xx, nas=NULL, action="warn") { #collapse a tibble of n x m into a single var (a vector of n) 
  # xx: egy keskeny tibble: az első oszlop egy ID (értelmes hibaüzikhez) a többi a mergelendő adatok (azonos típusú változók!)
  # nas= c("") # azok az értékek, amiket (az igazi NA-k mellett) még NA-nak kell tekinteni
  # action="warn" # egy függvény a többszörös értékek kezelésére (NULL: warning + first)
  if (ncol(xx)==2L) return(xx[[2]])
  if ((xx[,-1] %>% map(typeof) %>% unique %>% length) != 1L) stop("error!")
  ret <- xx[[2]]
  for (i in 1:nrow(xx)) {
    vals <- unlist(xx[i,-1])
    ret2 <- vals[!is.na(vals) & !(vals %in% nas)] %>% unique
    if (length(ret2)==0L) next 
    if (length(ret2)>=2L) ret2 <- switch(action, 
                                         first= ret2[1], 
                                         sum= sum(ret2),
                                         warn= {warning("Multiple valid values for vars ",  # warning + first (the default action)
                                                        paste(names(vals), collapse=", "), " in: ", x[[1]][1], "(first one returned)", call.=F); ret2[1]},
                                         paste(ret2, collapse=action))     # the default case for switch: paste+collapse with the string in "action"
    ret[i] <- ret2
  }
  ret
}

###
### Load in candidate EC vars from the metadb
###
# setwd("C:/Users/balint.czucz/OneDrive - NINA/_nina/fagsystem_tilstand/")
sht <-   drive_get("https://docs.google.com/spreadsheets/d/1nmg1yT1rbQBVCIoAJ8y-ID1q5RTnJlTUsKwP1UA-DVM") %>%
  {print(sheet_names(.)); I(.)} ### tilstand_metadb
1
1

novar0 <- range_read(sht, "main", skip=7, col_types="c", na=character()) %>%  # potentially available Norwegian EC vars  
  select(-starts_with("..")) %>%
  filter(i.id0!="") %>%      # remove empty lines
  filter(h.clean(tmp1)!="1") %>% # remove "deleted" lines
  {.}
novar1 <- novar0 %>% #cleanup
  # mutate(across(n1:nM, ~replace_na(.x,""))) %>%
  mutate(across(n1:nM, ~ordered(.x, levels=c("","?","x?","x","1?","1","2?","2")))) %>%
  mutate(across(c(d.startdate, d.temp.res), as.integer)) %>%
  mutate(ect.s=map2(ect.s1, ect.s2, ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank))) %>%
  mutate(novar=map(i.id0, ~(str_split_1(.x, ",") %>% trimws %>% h.deblank))) %>%
  {.}
nochar <- novar1 %>% #aggregation for the ec chars (as defined on tab "cw")
  group_by(grp) %>%
  summarise(nvar=n(), across(n1:nM, max),
            across(c(d.startdate, d.temp.res), min),
            across(c(d.spat.res, i.etlink, r.opt), ~str_c(h.deblank(sort(unique(.x))),collapse=" ")),
            across(c(ect.s), ~list(unique(list_c(.x)))),
            r.dir=list(str_extract(h.deblank(sort(r.dir)),"[pnux]")),
            novar= novar %>% list_c %>% unique %>% sort %>% h.deblank %>% str_c(collapse=", ")) %>%
  filter(grp!="") %>%
  # separate_wider_delim(grp, " | ", names=c("ect.p", "echar"), cols_remove=F) %>%
  {.}

estat0 <- range_read(sht, "estat", skip=4, col_types="c", na=character()) %>%  # potentially resquested mandatory&voluntary EC indicators (by ESTAT)
  select(-starts_with(".")) %>%
  filter(bc.ID!="") %>%      # remove empty lines
  # filter(h.clean(tmp1)!="1") %>% # remove "deleted" lines
  {.}
estat1 <- estat0 %>% #cleanup
  # mutate(across(n1:nM, ~replace_na(.x,""))) %>%
  mutate(across(n1:nM, ~ordered(case_when(str_detect(.x, fixed("!")) ~"m", .x!="" ~"v" , .default =""), levels=c("","v","m")))) %>%
  rowwise %>% mutate(estat= list(h.deblank(c_across(a:m)))) %>% ungroup %>% #from cols a:m because in these cols there are still no duplications & name editing
  mutate(ect.s=map2(ect.s1, ect.s2, ~c(.x %>% h.deblank, .y %>% str_split_1(",") %>% trimws %>% h.deblank))) %>%
  {.}
eschar <- estat1 %>% #aggregation for the ec chars (as defined on tab "cw")
  group_by(grp) %>%
  summarise(nest=n(), across(n1:nM, max),
            across(c(ect.s), ~list(unique(list_c(.x)))),
            r.dir=list(str_extract(h.deblank(sort(Dir)),"[pnux]")),
            estat= estat %>% list_c %>% unique %>% sort %>% h.deblank %>% str_c(collapse=", ")) %>%
  filter(grp!="") %>%
  {.}

tmp <- nochar %>%
  rename_with(~paste0(.x,".x"),n1:nM) %>%
  full_join(eschar, by="grp") %>%
  mutate(across(c(n1.x:nM.x, n1:nM), as.character))
for (ii in names(select(nochar, n1:nM))) {
  tmp[[ii]] <- h.merge.vars(tmp[,c("grp",paste0(ii,".x"),ii)],nas="",action=" ")
  }
noes <- tmp %>%
  mutate(ect.s= map2_chr(ect.s.x, ect.s.y, ~(c(.x,.y) %>% sort %>% unique %>% str_c(collapse=" ")))) %>%
  mutate(r.dir= map2_chr(r.dir.x, r.dir.y, ~(c(.x,.y) %>% sort %>% unique %>% str_c(collapse="")))) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  separate_wider_delim(grp, " | ", names=c("ect.p", "echar"), cols_remove=F) %>%
  mutate(across(c(nvar, nest), ~replace_na(0))) %>%
  arrange(grp) %>%
  relocate(n1:nM, .after=grp) %>%
  relocate(nest, .after=nvar) %>%
  relocate(ect.s:r.dir, .after=nest) %>%
  {.}

noes %>% 
  write_csv("tmp-balint/noes-01b.csv", na="")




