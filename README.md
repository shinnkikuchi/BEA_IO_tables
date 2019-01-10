# BEA_IO_tables

This repository contains:

* the correspondence table between 6 digits industry codes used in the BEA 1987 IO table and SIC codes
* the direct coefficients of the BEA 1987 IO table, together with the final consumption and value added vectors

Code and raw data can be found in the folders scripts and raw_data, respectively.

## Industry to SIC correspondence table

The column "SICs" includes the SIC codes, for each 6 digits industry code, as in the original text file. SIC_exclu indicates whether particular codes are to be excluded. SIC indicated all the corresponding codes for each 6 digits industry code (in long format). Note that some SIC codes are matched to multiple industries. The column SIC_n indicates to how many industry codes a given SIC code is matched (in the raw original BEA data those codes are marked with a *).


```{r}
crsp <- read.csv("1987/1987_6digits_to_SIC_conversion.csv", colClasses = rep("character",9) )
str(crsp)


'data.frame':	5475 obs. of  9 variables:
 $ header     : chr  "AGRICULTURE, FORESTRY, AND FISHERIES" "AGRICULTURE, FORESTRY, AND FISHERIES" "AGRICULTURE, FORESTRY, AND FISHERIES" "AGRICULTURE, FORESTRY, AND FISHERIES" ...
 $ digits2    : chr  "1 Livestock and livestock products" "1 Livestock and livestock products" "1 Livestock and livestock products" "1 Livestock and livestock products" ...
 $ digits6    : chr  "010100" "010100" "010100" "010100" ...
 $ digits6_des: chr  "Dairy farm products" "Dairy farm products" "Dairy farm products" "Dairy farm products" ...
 $ notes      : chr  "" "" "" "" ...
 $ SICs       : chr  "024,019, 0259, 029" "024,019, 0259, 029" "024,019, 0259, 029" "024,019, 0259, 029" ...
 $ SIC_exclu  : chr  "" "" "" "" ...
 $ SIC        : chr  "0190" "0191" "0192" "0193" ...
 $ SIC_n      : chr  "16" "16" "16" "16" ...
```

Note that 4  6-digits codes appear under different headers. All belong to the ADDENDUM header, and have no SIC correspondence. However, the respective normal entries in the CONSTRUCTION header do have SIC codes.

* 110601
* 110602
* 110603
* 120215


Numbers in the column "notes" refer to:

```
 [1] "  1. Although the SIC assigns the same codes to  activities of both private firms and government"  
 [2] "agencies, SIC codes in the I-O accounts are used only for classifying private activities."         
 [3] "  2. Noncomparable imports include imported goods and services that are not commercially produced "
 [4] "in the United States, and goods and services that are produced abroad and used abroad by U.S."     
 [5] "residents--for example, defense spending abroad."                                                  
 [6] "  3. Industry output is zero because there is no primary producing industry.  Scrap is a "         
 [7] "secondary product of many industries, and used goods are sales and purchases typically  "          
 [8] "between final uses.  The sales are shown as negative values in the use table."                     
 [9] "  4. Industry output is defined as the compensation of general government employees except"        
[10] "for those engaged in construction work; their compensation is included in the construction "       
[11] "industry.  It also excludes the compensation of employees of government enterprises."              
[12] "  5. The commodity entries include adjustments to personal consumption expenditures and "          
[13] "government  purchases that eliminate items that are actually exports."                             
[14] "  6. Industry output is defined as the compensation of domestic household workers."                
[15] "  7. The inventory valuation adjustment converts the inventory changes based on withdrawals"       
[16] "valued primarily at historical cost as reported by most businesses to replacement cost, the"       
[17] "valuation used in the I-O accounts."                                                               
[18] "  8. There are no related SIC codes since these categories are not industries, but are categories" 
[19] "of income."                                                                                        
[20] "  9. There are no related SIC codes since these categories are not industries, but are categories" 
[21] "of final uses." 
```

## BEA 1987 benchmark IO table

A more or less detailed description of the Input-Output table is included in the Rmarkdown file (explore_IO_table.html) in the repository.


