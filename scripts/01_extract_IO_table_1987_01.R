
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 0) Description ----
# In this file we extract Input Output data from the BEA benchmark 1987 data

# There are multiple files, we use the 1987 Benchmark I-O Table Six-Digit Transactions:
# https://www.bea.gov/industry/historical-benchmark-input-output-tables

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

# __________________________________________________________
# import use data

# use
io <- c(
  readLines("1987/raw_data/TBL2-87.DAT", encoding = "ASCII" )
) 

# direct requirements (dollar amount AND value added required to produce one output? )
d_dr <- c(
  readLines("1987/raw_data/TBL3-87.DAT", encoding = "ASCII" )
) 

# industry output and total final (col sums)
d_ind <- c(
  readLines("1987/raw_data/TBL6-87.DAT", encoding = "ASCII" )
) 

# commodity output and value added (row sums)
d_com <- c(
  readLines("1987/raw_data/TBL7-87.DAT", encoding = "ASCII" )
) 

# conversion table (already processed)
codes <- fread("1987/1987_6digits_to_SIC_conversion.csv", colClasses = rep("character", 9), na.strings = c("", "NA"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set reclycled arguments ----

# we will use this set of columns many times
colonne <- c( "header", "digits2", "digits6", "digits6_des")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Explore correspondance ----

# digits6 that have no code
no_SIC <- codes[is.na(SIC), digits6]

# unque digits6
codes_uni <- codes[, ..colonne ] %>% unique

# info for SIC
codes_uni[ , "noSIC" := ifelse( digits6 %in% no_SIC, 1, 0 )  ]

# some codes show up in different categories
codes_uni$digits6[duplicated(codes_uni$digits6) ]
# "110601" "110602" "110603" "120215"

# check how many uniques
codes_uni$digits6 %>% unique %>% length
# 560

# some entries are final demand
#final <- codes[header=="FINAL USES", ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process direc coefficients ----

d_dr <- data.table( str_split_fixed( d_dr, "\\s+", 12)  )
# io_f <- copy(io)
d_dr <- d_dr[, c(1,2,4)]
names(d_dr) <- c("source", "destination", "flow")
d_dr[, "flow" := ifelse( grepl("^\\.", flow), paste0(0,flow), flow ) %>% as.numeric() ]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process industry output ----

# extract data
d_ind <- data.table( str_split_fixed( d_ind, "\\s+", 3)  )
d_ind <- d_ind[, c(1, 3)]
names(d_ind) <- c("digits6", "flow")
# convert to numeric
d_ind[, "flow" := ifelse( grepl("^\\.", flow), paste0(0,flow), flow ) %>% as.numeric() ]

summary( duplicated(d_ind$digits6))
length( unique(d_ind$digits6) )
# 522

# add a column to unique digits6
codes_uni[ , "ind_out" := ifelse( digits6 %in% d_ind$digits6, 1 ,0) ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process commodity output ----

# extract data
d_com <- data.table( str_split_fixed( d_com, "\\s+", 3)  )
d_com <- d_com[, c(1, 3)]
names(d_com) <- c("digits6", "flow")
# convert to numeric
d_com[, "flow" := ifelse( grepl("^\\.", flow), paste0(0,flow), flow ) %>% as.numeric() ]

summary( duplicated(d_com$digits6))
length( unique(d_com$digits6) )
# 522

# add a column to unique digits6
codes_uni[ , "com_out" := ifelse( digits6 %in% d_com$digits6, 1 ,0) ]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process IO table ----

# ____________________________________________________________
# the data also includes transport cost and stuff
io <- data.table( str_split_fixed( io, "\\s+", 12)  )
# io_f <- copy(io)
io <- io[, c(1,2,4)]
names(io) <- c("source", "destination", "flow")
summary( io[,  c(1,2)] %>% duplicated)

# ____________________________________________________________
# check which codes show up as source, as destination and in both
# need to do this because some of the entries are final demand
io_source <- codes_uni[ digits6 %in% unique(io$source), ][["digits6"]]
io_dest <- codes_uni[ digits6 %in% unique(io$destination), ][["digits6"]]

io_source_only <- setdiff( io_source, io_dest )
io_dest_only <- setdiff( io_dest,  io_source )
io_both <- intersect( io_source, io_dest )

# create a category

codes_uni[ digits6 %in% io_source_only , "io_type" := "source_only" ]
codes_uni[ digits6 %in% io_dest_only, "io_type" :=  "dest_only"  ]
codes_uni[ digits6 %in% io_both, "io_type" := "both" ]

codes_uni[, "dupli" := duplicated(digits6)]


dcast(codes_uni, header ~ io_type, fun.agg = function(x) sum(!is.na(x)), value.var = "io_type")
dcast(codes_uni, digits2 ~ io_type, fun.agg = function(x) sum(!is.na(x)), value.var = "io_type")

# list some industries
setkey(codes_uni, header, io_type)
codes_uni[  .("ADDENDUMSpecial commodity groupings", "both"), c("digits6", "digits6_des") ]  
codes_uni[  .("MANUFACTURING", "dest_only"), c("digits6", "digits6_des") ]  


codes_uni[  .("CONSTRUCTION"), c("digits6", "digits6_des", "io_type") ]  
codes_uni[  .("GOVERNMENT ENTERPRISES"), c("digits6", "digits6_des", "io_type") ]  
codes_uni[  .("WHOLESALE AND RETAIL TRADE"), c("digits6", "digits6_des", "io_type") ]  
codes_uni[  .("SPECIAL INDUSTRIES"), c("digits6", "digits6_des", "io_type") ]  

saveRDS( codes_uni, "1987/6digits_unique_codes.RDS")

#' NOTES:
#' All 3 VALUE ADDED show up only as source: 880000, 890000, 900000. This makes sense
#' 
#' Almost all ADDENDUM show up only as source. This is not so clear.
#' The exceptions are 110601, 110602, 110603 and 120215, which show up as both source and destination.
#' Those 4 codes however also show up under a different heading (i.e. they show up twice in the correspondance table,
#' once in their normal category, and once in the special commodity groupings)
#' 
#' All final uses show up only in the destination. This makes sense.
#' 
#' There some entries that show up only in the destination.
codes_uni[ .(unique(header), "dest_only", nomatch=1), c("header", "digits6")] [ header != "FINAL USES"] %>% na.omit
#codes_uni[ .(unique(header), "dest_only"), c("header", "digits6")] [ header != "FINAL USES"][, .(unique(header), "dest_only", nom)]
## why NAs?? because we passed the unique argument, therefore keep all headers

#'                                   header digits6
# 1: AGRICULTURE, FORESTRY, AND FISHERIES  020701
# 2:                         CONSTRUCTION  110000  # this one should not be here? See manual, only codes below
# 3:                        MANUFACTURING  180201
# 4:                        MANUFACTURING  180202
# 5:                        MANUFACTURING  180203
# 6:                        MANUFACTURING  270202
# 7:                        MANUFACTURING  370104
# 8:                        MANUFACTURING  370105
# 9:                        MANUFACTURING  380600
# 10:               GOVERNMENT ENTERPRISES  780200
# 11:               GOVERNMENT ENTERPRISES  790100
# 12:               GOVERNMENT ENTERPRISES  790200
#' 
#' 
#' Because of reclassification some commodities dont have an output (i.e. dont show up as sources) see p.83 of pdf:
no_commodity_output <- c(
  "020701",
  "180201",
  "180202",
  "180203",
  "270202",
  "370104",
  "370105",
  "380600",
  "780200",
  "790100",
  "790200"
)

# transform into numeric
io[, "flow" := ifelse( grepl("^\\.", flow), paste0(0,flow), flow ) %>% as.numeric() ]



# __________________________
# now we want to separate the different elements of the IO


# get the 6 digit of different industries:
# first get the list of digits 6 codes
d6_final <- codes_uni[.("FINAL USES"), digits6]
d6_va <- codes_uni[.("VALUE ADDED"), digits6]

io[ , "type" := "dir_coeff" ]

setkey(io, source)
io[ .(d6_va), "type" := "value_added"   ]

setkey(io, destination)
io[ .(d6_final), "type" := "final"   ]


# Final demand
fin_dem  <- io[ type == "final" , ][ , type := NULL]

# Now can add output, so that we can divide
io[  , "dest_output" := sum(flow), by = .(destination) ]
io[  , "flow_per" := flow / dest_output ]

va  <- io[ type == "value_added" , ][ , type := NULL]
io_f  <- io[ type == "dir_coeff" , ][ , type := NULL]


# __________________________
# add additional info

colonne <-  c("digits2" ,"digits6", "digits6_des","noSIC" )
io_f <- merge(io_f,
              codes_uni[ dupli == F,][, ..colonne ],
              by.x="destination", by.y = "digits6")

io_f <- merge(io_f,
              codes_uni[ dupli == F,][, ..colonne ],
              by.x="source", by.y = "digits6", suffixes = c("_desti", "_source") )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## check data ----

# __________________________
# check commodity totals
d_com <- merge(d_com,
               io[ , sum(flow), by = "source"],
               by.x = "digits6", by.y = "source", all = T)
d_com[, "same" :=  round(flow, 7) ==  round(V1, 7) ]
d_com[ same==F | is.na(same),]

# __________________________
# total of industries (contains Final Consumption totals as well)
d_ind <- merge(d_ind,
               io[ , c("destination", "dest_output")] %>% unique,
               by.x = "digits6", by.y = "destination", all = T)
d_ind[, "same" :=   round(flow, 7) == round(dest_output,7) ]
d_ind[same==F | is.na(same),]

# __________________________
# direct coefficients
io[  , "flow_per_test" := flow /dest_output ] # we need this to compare
io[  , "flow_per_test" := round(flow_per_test, 7)]

d_dr <- merge(d_dr,
              io[type=="dir_coeff",],
              by=c("source", "destination"), all = T)
d_dr[, "same" := flow.x == flow_per_test]
d_dr[same==F | is.na(same), "false" := "no_match"  ]

# BEA included also the Value added in the direct coefficients
d_dr <- d_dr[ !(source %in% c("880000","890000","900000")) ,]

# 830001: Rest of the world adjustment to final uses
# BEA sets the ration to 0, but the output of destination is 0.. should be an NA
d_dr[ destination %in% c("830001") , "same" := T]

d_dr[same==F | is.na(same),  ] 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save data ----

# final demand
fwrite( fin_dem, "1987/1987_final_demand.csv", quote = T)
saveRDS(fin_dem, "1987/1987_final_demand.RDS" )

# value added
fwrite( va, "1987/1987_value_added.csv", quote = T)
saveRDS(va, "1987/1987_value_added.RDS" )

# direct_coeff
#saveRDS(io_f, "1987/1987_direct_coefficients.RDS" )

exp_col <- c("source", "destination","flow", "flow_per")
#fwrite( io_f[, ..exp_col ], "1987/1987_direct_coefficients.csv", quote = T)

fwrite( io_f[,  ], "1987/1987_direct_coefficients.csv", quote = T)
saveRDS(io_f, "1987/1987_direct_coefficients.RDS" )







