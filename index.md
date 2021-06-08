Pre-ZP4 Address Parsing and Standardization - ED
================
Kendall Frimodig
07 June, 2021

### Objective

Address information from Medical records can commonly differ from
official address registries such as those maintained by county and state
agencies, and the United State Postal Service. One example is when a
patients address is transcriped during data entry from handfilled
patient forms. Programs such as ZP4, or geocode files utilized by
ArcMap’s address matcher are able to detect errors such as mis-ordered
adresss elements and basic missing elements such as directionals or
street suffix’s. Manually verifying address with lower confidence can be
time-consuming when working with large data sets.

In order to reduce manual processes this program aims to make basic
corrections, or formatting to common element variations such as multiple
spellings of ‘S’ or ‘RD’. Additionally, information deemed non-essential
to geocoding such as apartment number will be seperated from the street
address. The program has three distincive sections.

The prepatory phase reads in patient data and assigns variables to
generic names. The rest of the code ideally does not have to be edited,
however certain sections in the manual parsing may be manipulated
depending on the address qualities.

The first utilizes the ‘postmstr’ package which performs basic parsing
and re-formatting of directional and street suffix elements.

The second analzes the reamining address name with most elements parsed
out already, using frequency analysis throughout to build custom
criteria of individual strings to parse out or re-format. This is
accomplished using nested loops, that analyze each adress sequentially,
splitting the address into sub-strings and scaning for identifiers.

### Packages used

postmastr for batch subsetting of street address elements and
standardization of prefix and suffix elements,

knitr for ‘kable()’ function (tidy frequency tables) and markdown format
dplyr for ‘word’ function (substring),

stringr for regular expression and string matching, concatenation,
whitespace trimming, and parsing individual stringsfrom street address.

data.table for simple re-joining of working address objects.

‘foreign’ for the saving of .dbf files used with ZP4 address reference
program

**Change working drive depending on workstation, uncomment
install.packages once for local installation and re-comment, loading of
pcakages only neccesary once in session.**

**reports of address string frequencies will eventually be embedded to
display interactively throughout what is happening to the data, for now
I just wanted to get this working**

``` r
#install.packages("knitr")
#install.packages("stringr")
#install.packages("remotes")
#remotes::install_github("slu-openGIS/postmastr")
#manually install data.table using R GUI
#install.packages("foreign")


library(stringr)
library(dplyr)
library(knitr)
library(postmastr)
library("data.table")
library(foreign)
```

### Importing from csv

``` r

src <- read.csv("edr1.csv", header = TRUE, sep=",",quote = "\"",
                dec = ".", fill = TRUE, comment.char = "", 
                stringsAsFactors=FALSE)

names(src) <- c("control","add","city","county","state",
                "zip" )

obs <- length(src$control)
```

### Setting temporary variable names

rename original variable names to common names, shared by different data
sets for the sake of reproducing chunks of code later on

``` r

names(src) <- c("control","add","city","county","state","zip")

obs <- length(src$control)
```

### Creating New Data-Frames in R for analysis and cleaning

code-blocks are modular for imported data, as the expected measures now
have identical variable names first I’ll assign the ED data to a
temporary data-frame, set blank initially with the columns desired and
row length determined by a variable within the original data set, in
this case the address column

### Working data sets

``` r

#setting blank data-frame  
tmp <-  setNames(data.frame(matrix(ncol=25, nrow=obs)), 
                 c("RECID","ADDRESS","CITY","ZIPCODE","COUNTY","ADDTYPE","GC_ID","ADDWORK",
                   "NOTE","GCST","GCCITY","GCZIP","GCSTATE","GCCOUNTY","GCSOURCE","GCNOTE",
                   "ZPST","ZPCITY","ZPSTATE","ZPZIP","ZPCOUNTY","ZP1","ZP2","ZP3","ZPFLAG"
                 ))


#assinging select columns from original data, and creating unique identifier


blank <- ""
fw <- "                                                                                                        "

tmp$RECID   <-c(src$control)
tmp$ADDRESS <-c(src$add)
tmp$CITY    <-c(src$city)
tmp$ZIPCODE <-c(src$zip)
tmp$GC_ID   <-seq(1:obs)
tmp$COUNTY  <-c(src$county)
tmp$GCCITY  <-c(src$city)
tmp$GCZIP   <-c(src$zip)
tmp$GCSTATE <-c(src$state)
tmp$GCCOUNTY    <-c(src$county)

tmp$GCST    <-  rep(blank, times=obs)
tmp$ZPST    <-  rep(blank, times=obs)
tmp$ZPCITY  <-  rep(blank, times=obs)
tmp$ZPZIP   <-  rep(blank, times=obs)
tmp$ZPSTATE <-  rep(blank, times=obs)
tmp$ZPCOUNTY    <-  rep(blank, times=obs)
tmp$ZP1 <-  rep(blank, times=obs)
tmp$ZP2 <-  rep(blank, times=obs)
tmp$ZP3 <-  rep(blank, times=obs)
tmp$ZPFLAG  <-rep(0, times=obs)
tmp$ZPNOTE  <-  rep(blank, times=obs)


tmp$GCST[1] <- fw
tmp$ZPST[1] <-  fw
tmp$ZPCITY[1]   <-  fw
tmp$ZPZIP[1]    <-  fw
tmp$ZPSTATE[1]  <-  fw
tmp$ZPCOUNTY[1] <-  fw
tmp$ZP1[1]  <-  fw
tmp$ZP2[1]  <-  fw
tmp$ZP3[1]  <-  fw

tmp$ZPNOTE[1]   <-  fw


#not neccesary if a 5 digit zip field is already present
tmp$GCZIP   <- str_sub(tmp$GCZIP,1,5) 
```

### Sub-setting

``` r
#subsetting data into 2 tables, depending on character or numeric prefix 

st <- c(tmp$ADDRESS)
first <- word(st, 1)

isnumeric <- str_extract(first, "[[:digit:]]+")
tmp$nump <- ifelse(is.na(isnumeric),NA,1)

ischar <- str_extract(first, "[[:alpha:]]+")
tmp$charp <- ifelse(is.na(ischar),NA,1)


#change subset name depeding on data set
ed.alt <- subset(tmp, charp==TRUE) #selects adresses with character prefix
ed.main <- subset(tmp, nump==TRUE) #selects adresses with numeric prefix (has house number)
```

## Cleaning standard addresses with ‘postmstr’ address parser

the ‘postmastr’ library will be used to scan the address for number,
directional, street name, and suffix (st, STREET, dr, etc..)Regardless
of the directionals position (before or after street name) it will be
extracted as a new column. It will also be standardized (SOUTH –&gt;
S…..S. –&gt; S) Similar logic for suffix (st). the ‘postmastr’ library
contains common spellings of these strings and should take the load off
ZP4. (additional errors formats must be added such as ‘Sth’ that don’t
exist in the standard library - this is determined by frequency analysis
prior)

it also identifies duplicates, assigns a individual ID for each address,
and then deplicates assigining a ‘UID’ which allows for eventual linkage
back to original duplicated data dictionaries for possible spellings can
be appended, but I found it doesn’t completely parse a few address. Due
to this I decided to skip appending the dictionaries, and rather include
the atpical formats of pre/post elements in the lists’ below.

-   order of parsing
    -   prep
    -   house number
    -   house suffix
    -   street directionals
    -   street suffix
    -   street name

``` r


dirs <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), 
                      locale = "us", case = c("title", "upper"))

original <- pm_identify(ed.main, var = "ADDRESS") 
work <- pm_prep(original, var = "ADDRESS", type = "address")
work <- pm_house_parse(work)
work <- pm_streetDir_parse(work, dictionary = dirs)
work <- pm_streetSuf_parse(work)
```

    ## Warning in stri_sub(string, from = start, to = end): argument is not an atomic
    ## vector; coercing

``` r
work <- pm_street_parse(work, ordinal = TRUE, drop = TRUE)
```

## Manual string analysis

The following code below takes the cleaned address fields from the
data-table created by ‘postmstr’ package, stores them as individual
objects (for simmplicity, not having to reference the data-frame
everytime) performs logical comparisons between address fields, and
eventually concatenates them back into a final cleaned and ordered
data-set.

The primary objective is to take extraneous address information (unit
type or number) and store as a note field, and to identify state route
or street intersection type addresses and work through them. regular
expressions can’t be used since the string order is variable and the
number as well.

As there isn’t a one size fits all approach, each individual string is
stored as a unique column and logical statements search through the
array of columns to identify and components and parse accordingly the
‘postmstr’ parser does a good job of seperating directional pre-fixes,
house numbers, and street types, but it misses a few addresses.

After parsing with the package, there were numerous addresses where the
pre-directional or street suffix had not been seperated from the street
name. The parsed elements from ‘postmstr’ are stored as working
vector’s, and the code-blocks below fill in the remaining pre and post
address elements. Since the logic works with exact matches to defined
strings, it is crucial that white-space is trimmed throughout, several
times individual strings will be replaced if not essential information.

based on manual frequency analysis of address data, lists of possible
matches for elements to be seperated are set for logical statements
below. Additionally elements which would flag an adress as non-standard
type are stored as lists.

``` r
num_st  <- c("1st","2nd","3rd","4th","5th","6th","7th","8th",
            "9th","10th","11th","12th", "13th","14th","15th",
            "16th","17th","18th","19th","20th")


rt.list <- c("HCR","Highway","HWY","County","Cr","Nm","Us","Hwy", 
             "Box","Country","Contry","Nmsr")


st.list <- c("St","st","stree","Stree","street","Street","dri","Dr",
             "Ave","Rd","Ln","ct","CT","Ct","blvd","blv","bl","Aly",
             "Blvd","Cir","Hls","Ln","Loop","Pl","Way","Vis","Road",
             "Drive","Blv","Blvd","Bl")


unit.list <- c("Apt","apt","spc","space","Space","Sp","Unit","Trlr",
               "Lot","Tr","Trailer","Apt.","#")


unit.let <- c("A","B","C","D","E","F")


dir.list <- c("E","East","EAST","N","North","NORTH","S","South","Sth",
              "SOUTH","STH","W","West","WEST","SE","SW","NE","NW")


int.sec <- c("half","1/2","At","at")


string.trim <- function(x,...) { x <- str_trim(x, side="both") }
```

``` r
#create working vectors intially filled with elements 'postmstr' parsed, 
#to which remaining unparsed elements are added  

st      <- work$pm.street
st.suf      <- ifelse(is.na(work$pm.streetSuf),"",work$pm.streetSuf)
st.dsuf <- ifelse(is.na(work$pm.sufDir),"",work$pm.sufDir)
st.pdir <- ifelse(is.na(work$pm.preDir),"",work$pm.preDir)
housenum <- ifelse(is.na(work$pm.house),"",work$pm.house)
```

``` r
obs <-length(st)
atype <- rep("", times=obs)
i <- 1

for (add in st){
  
  n.words <- unlist(strsplit(add, " +"))
  
  for (sub in n.words){
    
    if(sub %in% int.sec){atype[i] <- "3" 
    break}
    
    else if(sub %in% rt.list){atype[i] <- "88"}
    
    else if(atype[i] == "") {atype[i] <- "1"}
  }
  
  i <- i+1
  
}


kable(table(atype))
```

| atype | Freq |
|:------|-----:|
| 1     | 1888 |
| 3     |   19 |
| 88    |   53 |

``` r
note <- rep("", times=obs)

st      <- c(str_replace(c(st), "#"," # "))

i <- 1


for (add in st){
  
  
  diced <- unlist(strsplit(add, " +"))
  string.trim(diced)
  num.w <- length(diced)
  
  j <- 1
  
  for (sub in diced){
    
    if(sub %in% unit.list){
      
      note[i] <- str_c(diced[j:num.w], sep = " ", collapse = " ")  
      
      diced[j:num.w] <-  ""
      
      break
      
    }
    
    else{j <- j+1}
    
  }
  
  
  string.trim(diced)    
  
  st[i] <- str_c(diced, sep = " ", collapse = " ")
  
  i <- i + 1
  
}




note<- str_to_upper(note, locale = "en")

kable(table(note))
```

| note          | Freq |
|:--------------|-----:|
|               | 1810 |
| \# 1          |    2 |
| \# 10         |    1 |
| \# 102        |    1 |
| \# 104        |    1 |
| \# 108        |    1 |
| \# 11         |    1 |
| \# 115        |    1 |
| \# 117        |    1 |
| \# 119        |    1 |
| \# 13         |    2 |
| \# 14         |    2 |
| \# 15         |    2 |
| \# 17         |    1 |
| \# 19         |    1 |
| \# 2          |    3 |
| \# 206        |    1 |
| \# 209        |    1 |
| \# 21         |    1 |
| \# 218        |    2 |
| \# 3          |    2 |
| \# 3 WHITTIER |    1 |
| \# 3E         |    1 |
| \# 4          |    3 |
| \# 4G         |    1 |
| \# 5          |    1 |
| \# 5A         |    1 |
| \# 6          |    2 |
| \# 6B         |    1 |
| \# 7          |    3 |
| \# 9          |    1 |
| \# B-7        |    1 |
| \# C          |    1 |
| \# D17        |    2 |
| \# D7         |    1 |
| APT \# 1      |    1 |
| APT \# 111    |    1 |
| APT \# 2      |    1 |
| APT \# 3      |    1 |
| APT \# 3A     |    1 |
| APT \# 4      |    1 |
| APT \# 4 -C   |    1 |
| APT \# 6      |    2 |
| APT \# 6A     |    1 |
| APT 1         |    4 |
| APT 101       |    1 |
| APT 102       |    1 |
| APT 107       |    1 |
| APT 108       |    1 |
| APT 110       |    1 |
| APT 111       |    1 |
| APT 112       |    1 |
| APT 116       |    1 |
| APT 118       |    1 |
| APT 1C        |    1 |
| APT 1E        |    1 |
| APT 201       |    1 |
| APT 202       |    1 |
| APT 209       |    1 |
| APT 210       |    2 |
| APT 216       |    1 |
| APT 219       |    2 |
| APT 2C        |    1 |
| APT 2H        |    1 |
| APT 3-D       |    1 |
| APT 3D        |    1 |
| APT 3F        |    1 |
| APT 5         |    1 |
| APT 5C        |    1 |
| APT 6         |    1 |
| APT 6 B       |    1 |
| APT 6B        |    1 |
| APT 6C        |    1 |
| APT 6D        |    1 |
| APT 7         |    1 |
| APT 7A        |    1 |
| APT A1        |    1 |
| APT A4        |    1 |
| APT LOT 3     |    1 |
| APT. 111      |    1 |
| LOT 1         |    1 |
| LOT 101       |    1 |
| LOT 210       |    1 |
| SP \# 101     |    1 |
| SP \# 109     |    1 |
| SP \# 14      |    2 |
| SP \# B5      |    1 |
| SP 1          |    1 |
| SP 108        |    1 |
| SP 109        |    1 |
| SP 11         |    2 |
| SP 14         |    1 |
| SP 2          |    1 |
| SP 5          |    1 |
| SP B3         |    1 |
| SP C1         |    1 |
| SP D 17       |    1 |
| SPACE \# 13   |    1 |
| SPACE \# 15   |    1 |
| SPACE \# 2    |    1 |
| SPACE \# 4    |    2 |
| SPACE \# 8    |    1 |
| SPACE 1       |    1 |
| SPACE 10      |    1 |
| SPACE 12      |    1 |
| SPACE 13      |    1 |
| SPACE 14      |    1 |
| SPACE 2       |    2 |
| SPACE 4       |    1 |
| SPACE 8       |    1 |
| SPACE 9       |    1 |
| SPACE B       |    1 |
| SPACE B9      |    1 |
| SPACE D5      |    1 |
| SPACE E6      |    1 |
| TR            |    1 |
| TR \# 19      |    1 |
| TR 7          |    1 |
| TRAILER D5    |    1 |
| TRLR 13       |    1 |
| UNIT \# 1     |    2 |
| UNIT 1        |    1 |
| UNIT 16       |    1 |
| UNIT 4        |    1 |
| UNIT 8        |    1 |
| UNIT B        |    1 |

``` r

#STREET SUFFIX PARSING

st.suf  <- ifelse(is.na(work$pm.streetSuf),"",work$pm.streetSuf)

kable(table(st.suf))
```

| st.suf | Freq |
|:-------|-----:|
|        |  816 |
| Aly    |    2 |
| Ave    |  324 |
| Blvd   |   17 |
| Cir    |   16 |
| Dl     |    2 |
| Dr     |   69 |
| Hl     |    6 |
| Hls    |    6 |
| Ln     |   41 |
| Loop   |    5 |
| Mdw    |    2 |
| Mdws   |    1 |
| Pl     |    2 |
| Rd     |  109 |
| St     |  519 |
| Ter    |    1 |
| Trl    |    3 |
| Trlr   |    1 |
| Vis    |   13 |
| Vly    |    1 |
| Vw     |    2 |
| Way    |    2 |

``` r
x <- st.list
y <- st.suf
i <- 1

for (add in st){
  
  
  diced <- unlist(strsplit(add, " +"))
  string.trim(diced)
  num.w <- length(diced)
  
  j <- 1
  
  if(atype[i] != 3){
    
      for (sub in diced){
      
        if(sub %in% x){
      
           y[i] <- sub
      
           diced[j] <-  ""
      
           j <- j+1
      
          }
    
        else{j <- j+1}
    
        }
  
  diced <- str_trim(diced, side="both")
  
  st[i] <- str_c(diced, sep = " ", collapse = " ")
  
  }
  
  i <- i+1
  
}




#y <- str_to_upper(y, locale = "en")


st.suf <- y

kable(table(st.suf))
```

| st.suf | Freq |
|:-------|-----:|
|        |  678 |
| Aly    |    2 |
| Ave    |  342 |
| Bl     |    2 |
| Blv    |    2 |
| Blvd   |   24 |
| Cir    |   16 |
| Dl     |    2 |
| Dr     |   74 |
| Drive  |    1 |
| Hl     |    6 |
| Hls    |    6 |
| Ln     |   41 |
| Loop   |    5 |
| Mdw    |    2 |
| Mdws   |    1 |
| Pl     |    2 |
| Rd     |  122 |
| Road   |    2 |
| St     |  602 |
| Stree  |    1 |
| Street |    5 |
| Ter    |    1 |
| Trl    |    3 |
| Vis    |   13 |
| Vly    |    1 |
| Vw     |    2 |
| Way    |    2 |

``` r

#DIRECTIONAL PARSING





x <- dir.list

y <- st.pdir

z <- st.dsuf

kable(table(y))
```

| y   | Freq |
|:----|-----:|
|     | 1369 |
| E   |  179 |
| N   |  186 |
| S   |  204 |
| W   |   22 |

``` r
kable(table(z))
```

| z   | Freq |
|:----|-----:|
|     | 1952 |
| E   |    4 |
| W   |    4 |

``` r

i <- 1

for (add in st){
  
  
  diced <- unlist(strsplit(add, " +"))
  string.trim(diced)
  num.w <- length(diced)
  
  j <- 1
  
  for (sub in diced){
    
      if(sub %in% x){
      
          if (j==1){
      
              y[i] <- sub
      
              diced[j] <-  ""
      
              j <- j + 1 }
      
            else if (atype[i] != 3){ 
              
               z[i] <- sub
      
               diced[j] <-  ""
      
               j <- j + 1 }
      
         }
    
      else{ j <- j + 1 }
    
    }
  
  diced <- str_trim(diced, side = "both")
  st[i] <- str_c(diced, sep = " ", collapse = " ")
  
  i <- i +  1
  
}





y <- str_sub(y, start=1, end=1)
z <- str_sub(z, start=1, end=1)

y <- str_to_upper(y, locale = "en")
Z <- str_to_upper(z, locale = "en")


st.pdir <- y
st.dsuf <- z

kable(table(y))
```

| y   | Freq |
|:----|-----:|
|     | 1355 |
| E   |  182 |
| N   |  186 |
| S   |  213 |
| W   |   24 |

``` r
kable(table(z))
```

| z   | Freq |
|:----|-----:|
|     | 1946 |
| E   |    6 |
| W   |    8 |

``` r
#st <- str_to_upper(st, locale = "en")
```

``` r
string.trim(housenum)
string.trim(st.pdir)
string.trim(st)
string.trim(st.suf)
string.trim(st.dsuf)


addwork <- str_c(housenum,st.pdir,st,st.suf,st.dsuf, 
                 
                 sep = " ", collapse = NULL)

addwork <- str_replace(addwork,"  ", " ")


addwork <- ifelse(st=="", "",addwork)



ad <- data.table(uid = work$pm.uid, ADDWORK = addwork, 
                 NOTE=note, ADDTYPE=atype)




original <- as.data.table(original)           

new <- original[ad, on = .(pm.uid = uid)]


quality <- data.table(orig=new$ADDRESS, clean=new$i.ADDWORK)

#join indexed data back to original data using UID, which then re-duplicates addresses. 
#kable(table(quality))



ed.mainn <- setDT(ed.main)


ed.mainn <- new[, charp:= NULL]
ed.mainn <- ed.mainn[, nump:= NULL ]

ed.mainn <- ed.mainn[, ADDTYPE:=i.ADDTYPE ]
ed.mainn <- ed.mainn[, ADDWORK:=i.ADDWORK ]
ed.mainn <- ed.mainn[, NOTE:=i.NOTE ]


ed.mainn <- ed.mainn[, i.ADDTYPE:= NULL ]
ed.mainn <- ed.mainn[, i.ADDWORK:= NULL ]
ed.mainn <- ed.mainn[, i.NOTE:= NULL ]
ed.mainn <- ed.mainn[, pm.type:= NULL ]

ed.mainn <- ed.mainn[, pm.id:= NULL ]
```

``` r

#process for cleaning 'alt' address set is all manual as opposed to the procedure used for standard addresses or those begining with a house number
#peek <- str_sub(ed.alt$ADDRESS,1,5)
#kable(table(peek))

#can be used to determine what the first strings look like for this set of addresses. Generally this allows for a summary of the address 'type' with a frequency table.

tp.four <- c("RR", "RT", "RURAL", "ROUTE", "Rt", "Rural", "Route")

tp.five <- c("HC", "HCR", "Hcr")

tp.six  <- c("Box", "BOX", "box", "PO", "P.O.", "P.O,", "P O", "POBOX", "Po")

tp.seven <- c("GENERAL", "General")

srt         <- ed.alt$ADDRESS
```

``` r

obs <-length(srt)

atype <- rep("", times=obs)

i <- 1

for (add in srt){
  
  n.words <- unlist(strsplit(add, " +"))
  
  for (sub in n.words){
    
    if(sub %in% tp.four){atype[i] <- "4" 
    break}
    
    else if(sub %in% tp.five){atype[i] <- "5"
    break}
    
    else if(sub %in% tp.six){atype[i] <- "6"}
    
    else if(sub %in% tp.seven){atype[i] <- "7"}
    
    
    else if(atype[i] == "") {atype[i] <- "2"}
  }
  
  i <- i+1
  
}

ed.altt <- setDT(ed.alt)


ed.altt <- ed.altt[, charp:= NULL]
ed.altt <- ed.altt[, nump:= NULL ]

ed.altt <- ed.altt[, ADDTYPE:=atype ]
ed.altt <- ed.altt[, ADDWORK:=ed.altt$ADDRESS ]

ed.altt <- as.data.frame(ed.altt)
```

final cleaned addresses ready for geocoding software

``` r
add1 <- ed.mainn$ADDWORK


head(add1, n=50)
```

    ##  [1] "4704 Burton Se St "       "4704 Burton Se St "      
    ##  [3] "1262 Little Big Horn Rd " "11 Squaw Valley Rd "     
    ##  [5] "35 Buckskin Rd "          "41 County C23 Rd "       
    ##  [7] "3521 Hwy 434  "           "33 Elliott Barker  "     
    ##  [9] "120 Jackdon Hl "          "120 Jackdon Hl "         
    ## [11] "90 Conchas Dr "           "90 Conchas Dr "          
    ## [13] "3381 Hwy 434  "           "49 Vista Del Valle  "    
    ## [15] "49 Vista Del Valle  "     "52 Knollwood Way "       
    ## [17] "52 Knollwood Way "        "106 Halo Pines Ter "     
    ## [19] "106 Halo Pines Ter "      "106 Halo Pines Ter "     
    ## [21] "25 Agua Rd "              "27736 Us Hwy 64  "       
    ## [23] "11 Squaw Vly "            "14 Cochiti Cir "         
    ## [25] "3381 Mountain View B  "   "52 Knollwood Way "       
    ## [27] "276537 Hwy 64  "          "276537 Hwy 64  "         
    ## [29] "87 Halo Pines Terrac  "   "11430 W 84th Pl "        
    ## [31] "11430 W 84th Pl "         "510 White Pine Rd "      
    ## [33] "1300 Elver Rd "           "700 Washington  "        
    ## [35] "700 Washington  "         "117 E 7th St "           
    ## [37] "117 E 7th St "            "529 E 7th St "           
    ## [39] "105 Lambert Hls "         "105 Lambert Hls "        
    ## [41] "105 Lambert Hls "         "105 Lambert Hls "        
    ## [43] "105 Lambert Hls "         "105 Lambert Hls "        
    ## [45] "105 Lambert Hls "         "549 W 19th St "          
    ## [47] "549 W 19th St "           "429 E 11th St "          
    ## [49] "729 E 7th St "            "729 E 7th St "
