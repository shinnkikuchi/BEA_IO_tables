rm(list = ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 0) Description ----
# In this file we extract Input Output data from the BEA benchmark 1987 data

# There are multiple files, we use the 1987 Benchmark I-O Table Six-Digit Transactions:
# https://www.bea.gov/industry/historical-benchmark-input-output-tables

# we are interested in the "use table", i.e. table 2, which contains 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load dependencies 
packages <- c("magrittr", "data.table", "stringr")
to.install <- setdiff(packages, rownames(installed.packages()))
if (length(to.install) > 0) {
  install.packages(to.install)
}
lapply(packages, library, character.only = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data 

#i <- readLines("1987/raw_data/sic-io.doc")
i = readLines("1992/raw_data/Sic-IO.txt")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process correspondance ----

# taking away the header and save the footer somewhere else
i <- i[-c(1:15)]
notes <- i[(1023:length(i))] 
i <- i[-c(1023:length(i))] 
# delete rows that are empty
i <- i[! grepl("^\\s*$", i )]

#' we want to split the strings in 3 columns.
#' The trick is that sometimes descriptions and sic codes are on two lines.

# First of all, some lines contain the general descriptions. They are all in capital characters
head_grp <- grep("^[A-Z]{2}", i)
# sector description
sect2_grp1 <- grep("^\\d{1,2}\\+", i)
sect2_grp2 <- grep("^\\d{1,2}\\s", i)
sect2_grp3 <- grep("^\\d{1,2}\\s?[A-Z]{1}", i)
sect2_grp=c(sect2_grp1,sect2_grp2,sect2_grp3)
# Sort sect2_grp
sect2_grp <- sect2_grp[order(sect2_grp)]
# Drop dupliates
sect2_grp <- unique(sect2_grp)
# rows that contain the 6 digits codes
digits6_grp <- grep("\\d{1,2}\\.\\d{4}", i)
# what is left should be entries that span two rows:
digits6_2nd_grp <- setdiff( c(1:length(i)) , c(head_grp, digits6_grp, sect2_grp) )
i[digits6_2nd_grp]


# create an empty data.table and populate it
d <- data.table(matrix(nrow = length(i)))

# we want to fill in all rows with the header and digit 2 level, therefore need to know how many 
# times to repeat it

wieder1 <- c(head_grp, length(i) + 1)[-1] - head_grp
d[, "header" :=  rep(i[head_grp],
                     wieder1 )]

wieder2 <- c(sect2_grp, length(i) + 1)[-1] - sect2_grp
wieder2[1] <- wieder2[1] + 1 # because the first entry in the column is an NA 
d[, "digits2" := rep(i[sect2_grp],
                     wieder2)]

# clean up strings
d[, names(d) := lapply(.SD, function(x){gsub("\\s*:\\s*", "", x )}), .SDcols = names(d)]

# now we add the digits 6 code.
# for some need to repeat two times, because description or SIC is on two rows
d[digits6_grp , "digits6" :=  str_extract(i[digits6_grp], "\\d{1,2}\\.\\d{4}") ]
# repeat when necessary (in two steps, using shift)
d[, "digits6_tmp" := shift(digits6)]
d[ digits6_2nd_grp, "digits6" := digits6_tmp]
d[,"digits6_tmp" := NULL]

# add the description of the 6 digits and the corresponding SIC codes
d[digits6_grp , "digits6_des" := gsub( "\\d{1,2}\\.\\d{4}\\s+", "", i[digits6_grp] ) ]
# sometimes the description and codes are on two rows (we matched those above)
d[digits6_2nd_grp, "digits6_des" := i[digits6_2nd_grp] ] 
# d[is.na(digits6_des), "digits6_des" := ""]

# # substitute two strings in the description and then get SIC
d[, "digits6_des" := gsub("1-unit","one unit", digits6_des )]
d[, "digits6_des" := gsub("2-4 unit","two to four unit", digits6_des )]

d[, "SIC" := str_extract(digits6_des, "\\.*\\({0,1}\\*{0,1}\\d+.*$") ]
d[, "digits6_des" := gsub( "\\.*\\({0,1}\\*{0,1}\\d+.*$", "", digits6_des ) ]

# now we can drop all rows that dont contain 6 digits codes
d <- d[!is.na(digits6),]

# paste the description and SIC list if on two rows
d[is.na(SIC), "SIC" := ""]
d <- d[ , .(digits6_des = paste0(digits6_des, collapse = ""),
           SIC = paste0(SIC, collapse = ",")),
            by = .(header,digits2,digits6)]

# some SIC codes are in fact notes:
d[, "notes" := str_extract( SIC, "\\(.*\\)" ) ]
d[, "SIC" := gsub( ",{0,1}\\(.*\\)", "", SIC  ) ]

# some notes are in fact excluded SIC
d[ grep( "\\(excl", notes), "SIC_exclu" := str_extract(notes, "\\d+" ) ]
d[ grep( "\\(excl", notes), "notes" :=  NA ]

# trim variables
d[, names(d) := lapply(.SD, str_trim) , .SDcols = names(d) ]

# duplicated
d[, "dupli" := duplicated(digits6)]
d[ dupli == T,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process SIC correspondance ----

# fix some issues in the SIC colum
# d[, "SIC2" := SIC] # uncomment if want to keep the old version

# the * just indicates that the code shows up multiple times, can recover this later. Also dont need staring ,
d[, "SIC" := gsub( "\\*|^,", "", SIC )  ]
# we want entries to be separated by a comma only
d[, "SIC" := gsub( "\\s*,\\s*,\\s*", ",", SIC )  ]
# if there is just a comma and empty characters set to NA
d[ grepl("^\\s*,*\\s*$", SIC )  , "SIC" := NA ]

d <- setDT(d)[, strsplit(as.character(SIC), ",", fixed=F), by = .(header, digits2,digits6,digits6_des,dupli,notes,SIC, SIC_exclu)]
d[, "V1" := str_trim(V1)]
setnames(d, "SIC", "SICs")

# ______________________________________________________________________
# define a function that will be applied to all elements in each sublist
SIC.extract <- function(x){
  # get the complete series if required: 
  # eg. 23-7: 23, 24, 25, 26, 27. We get the 4 digits below
  # debug:
  # x <- SIC[[1]][1]
  # x <- "075-6"
  # x <- "4567"

  id <- x
  if(grepl("-", x )){
    stem <- gsub("\\d{1}-\\d{1}","",x )
    a <- str_split( str_extract( x , "\\d{1}-\\d{1}" ), "-" ) %>% unlist
    x <- paste0( stem, c(a[1]:a[2]))
  }
  # get the 4 digits code 
  get4 <- function(x){
                      if(nchar(x)==2){
                        out <- paste0(x, substr(c(100:999), 2,3 ))
                      } else if (nchar(x)==3){
                        out <- paste0(x, c(0:9) )
                      } else if (nchar(x)==4){
                        out <- x # do nothing if already 4 digits
                      }
                      return( data.table("V1"=id, "SIC"=out))
    
  }
 out <- lapply(x, get4)
 out <- rbindlist(out, use.names = T, fill = T)
  
}

d_l <- lapply( d$V1[!is.na(d$V1)], SIC.extract )
#d_l <- sapply(d_l, "[", 1)
d_l <- rbindlist(d_l,
                 use.names = T, fill = T)

# now can merge back together
d <- merge(d, unique(d_l), by = "V1", all.x = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## deal with excluded ----

# create a column with indicators for rows to be dropped
d$drop <- mapply( function(x,y){ grepl( paste0("^",x), y ) }, d$SIC_exclu, d$SIC )

d <- d[drop==F,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## indicator for doubles ----

# sometimes SIC codes are matched to more then one 6 digits code. Create an indicator for that
d[!is.na(SIC), "SIC_n" := .N, by= .(SIC)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add notes ----
comment(d) <- notes

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## fix 6digits format ----
d[ grep("^\\d{1}\\.", digits6 )  , "digits6" := paste0("0", digits6)]
d[, "digits6" := gsub("\\.", "", digits6) ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## additional string fixes ----
d[, (names(d)) := lapply( .SD, function(x){ gsub("\\s+", " ", x)}), .SDcols=names(d)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  save ----

colonne <- c("header", "digits2", "digits6", "digits6_des", "dupli", "notes", 
             "SICs", "SIC_exclu", "SIC", "SIC_n")

d <- d[order(digits6, SIC), ..colonne]

saveRDS( d,
        "1987/1987_6digits_to_SIC_conversion.RDS")






