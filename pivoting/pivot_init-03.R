
###########################################
###
### Helper functions & constants for
### 
### Expoloring the options for "pivoting" the EC variables 
### in the "utredning tistand" project  
### 
### R script by Balint
###  Sep 2023
###
###########################################


###
### constants
###
l.spat.res <- c(paste0("g",1:6),"p1","p2","pp","px") #levels of d.sapt.res 
l.dir <- "pnux" %>% str_split_1("")
l.etlink <- c("cc","fo","so","lm","na")

# dat1$i.cpl3 %>% c(est1$i.cpl3) %>% unique %>% sort %>% {.[-1]} %>% dput #remove the "" #(with combinations)
# res1$cpl0 %>% names %>% unique %>% sort %>% {.[-1]} %>% dput #remove the "" #(with combinations)
l.cpl  <- c("ai","aqi","d","dn","e","n","o","q","qo","r","s","sq","t","tq","xd","xt")
l.cpl1 <- l.cpl %>% str_c(collapse="") %>% str_split_1("") %>% unique %>% sort #%>% dput #(without combinations)

# # dat1$d.temp.res2 %>%  unique %>% sort %>%  dput #remove the "" #(with combinations)
# l.trs  <- c(1L, 5L, 10L)

# est0 %>% select(a:m) %>% names %>% dput
# l.et.eu <- c("a", "b", "c", "e", "f", "g", "h", "j", "k", "i", "l", "m")
l.et.nina <- c("n1", "n21", "n22", "n31", "n32", "n41", "n42", "n51", "n52",
               "n53", "n54", "n55", "n6", "n71", "n72", "n8a", "n8b", "nC", "nM")
lkp.et.eu <- read_csv2(show_col_types=F, 
  "et; et2; et_label
    a;  ur; Settlements and other artificial areas
    b;  cr; Croplands
    c;  gr; Grasslands
    e;  fo; Forests and woodlands
    f;  sh; Heathland and shrub
    g;  sv; Sparsely vegetated ecosystems
    h;  iw; Inland wetlands
    j;  rc; Rivers and Canals
    k;  lr; Lakes and reservoirs
    i;  co; Coastal beaches, dunes and wetlands
    l;  tw; Marine inlets and transitional waters
    m;  ma; Marine ecosystems")

# dat1$ect.p %>% unique %>% sort %>% dput
l.ect <- c("A1", "A2", "B1a", "B1b", "B2", "B3", "C1") #without the "X"


###
### helpers
###
h.lkp <- function(x, tab) { #simple lookup of values of vector x in the second column of data.frame (or matrix) tab
  tab[[2]][match(as.character(x), as.character(tab[[1]]))]
  } 

h.clean <- function(x) {if (mode(x)=="character") #helper fun to clean up a char vector (remove line breaks)
  ret <- x %>% str_replace_all("[\r\n]", " ") %>% trimws else x
  }

h.rena <- function(x, na=if (mode(x)=="character") "" else 0) {  #helper fun to remove NAs w intelligent defaults
  ret <- x %>% replace_na(na) 
  }

h.deblank <- function(x, na="") x %>% {.[!is.na(.)]} %>% {.[.!=na]} #helper to remove blank values from a char vector
# c(1:3,NA,"",4:6) %>% h.deblank

h.extract <- function(x, l) { #extract the first occurrence of any value of l[chr] as a substring in each of x[chr]
  x %>% str_extract(str_c(sprintf("(%s)",l), collapse="|"))  
  }

h.concat <- function(x, sep=NULL) { #concatenate multiple comma separated lists into a single one
  x %>% str_split(if (is.null(sep)) fixed(",") else sep) %>% 
    list_c %>% trimws %>% h.deblank %>% unique %>% sort %>% 
    str_c(collapse=if (is.null(sep)) fixed(", ") else sep)
  }
c("NVE", "MDir", "MDir,NINA", "MDir, , NINA", "NINA") %>% h.concat

x=c("p3?", "?", "11")
# x: a character vector of "ET status codes" (the same [(p)S(?)] notation as in cols n1:nM in "main")
h.et.getS <- function(x) { #get the status (S/i.st: S=1-3) out from a vector of ET codes 
  x %>% str_extract("[:digit:]") %>% as.integer %>% replace_na(0)
  }
h.et.getp <- function(x) { #get the "partiality" (p: "p" or "") out from a vector of ET codes 
  x %>% str_extract("p") %>% replace_na("")
  }
h.et.agac <- function(x) { #across-ET aggregation of "ET status codes", rules:
  #    -- if none of sel are blank or "p" --> max(X)
  #    -- if some of sel are blank or "p" --> "p"+max(X)
  #    -- if all of sel are blank --> blank
  if (all(x %in% c("","?"))) return("")
  ret <- x %>% h.et.getS %>% max %>% as.character
  if (any(x %in% c("","?") | h.et.getp(x)=="p")) str_c("p",ret) else ret
  }

# x=LETTERS[1:5]; et1=c("p3?","2?","11", "2","?"); pt="C2"; fun=c("A","B","d")
# x=1:5; et1=c("p3?","2?","11", "2","?"); pt="C2"; fun=~sum(c(.x,1)); .na=NULL
# x=LETTERS[1:5]; et1=c("p1?","1?","11", "","?"); pt="C2"; fun=c("A","B","d")

h.summarise <- function(x, st1, fun=NULL, pt="P1", plus=NULL, .na=NA) {
  # x:    vec[n]: values to summarise (=aggregate)
  # st1:  char[n]: a vector of "ET status codes" (the [(p)S(?)] notation)
  # pt:   char[1]: a 2+1 byte "projection type" code:  
  #         1: C/P: consider just the vars w complete coverage (C), or partial vars, too (P)
  #         2: 1/2/3: consider only vars w status equal to or higher than this
  # fun:  aggregation function:
  #         NULL: n()
  #         any function/lambda (with a single vector argument returning a (named) vector of constant type and length) 
  #         a char vector: each of its elements gets counted in x
  # na:   the value returned for an empty selection (if fun is a function)  
  # plus: logi[n]: an optional additional filter (e.g. one based on i.cpl)
  sel <- h.et.getS(st1)>=h.et.getS(pt)
  sel <- sel & !(is.na(x))
  if (str_sub(pt,1,1)=="C") sel <- sel & (h.et.getp(st1)=="")
  if (!is.null(plus)) sel <- sel & plus
  if (is.null(fun)) return(sum(sel))
  if (is.character(fun)) {
    map(fun, ~sum(as.character(x[sel])==.x)) %>% set_names(fun) %>% as_tibble
   }else{
     # cat(paste(x,st1,sel,sep=":"),"\n")
     if (any(sel)) exec(rlang::as_function(fun), x[sel]) else .na
    }
  }

# x=LETTERS[1:5]; w=c(0,2,1,0,1); fun=c("A","B","d")
h.summarise2 <- function(x, w, fun=NULL, .na=NA) {
  # x:    vec[n]: values to summarise (=aggregate)
  # w:    int[n]: a vector of integer weights (indicator "counts") for each element in x  
  # fun:  aggregation function:
  #         NULL: sum(w)
  #         any function/lambda (with a single vector argument returning a (named) vector of constant type and length) 
  #         a char vector: each of its elements gets counted in x
  # na:   the value returned for an empty selection (if fun is a function)  
  # cat(paste(x,w,sep=":",collapse=" | "),"\n")
  if (is.null(fun)) return(sum(w))
  if (length(w)==0|sum(w)==0) return(.na)
  x <- rep(x, times=w)
  if (is.character(fun)) {
    map(fun, ~sum(as.character(x)==.x)) %>% set_names(fun) %>% as_tibble
  }else{
    # cat(paste(x,st1,sel,sep=":"),"\n")
    if (length(x)>=1L) exec(rlang::as_function(fun), x) else .na
    }
  }



