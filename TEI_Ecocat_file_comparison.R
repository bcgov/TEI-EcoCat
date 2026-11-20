# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#Script name: TEI and Ecocat Comparison

#Author: Alessandra Jones

#Description: Compare files available on TEI Distribution folders and EcoCat database
#to prepare for removing GIS files from EcoCat that are already in TEI

#Flag any records where we have files in TEI Distribution folders that aren’t on EcoCat
#Flag EcoCat records where we have files that do not occur in TEI Distribution folders
#Flag EcoCat records with spatial data (shapefiles, gdb or e00 files for example)
#Flag BAPIDs with no files in the Distribution folder but reports, legends, field data etc occur in other folder locations.

#Date created: September 29, 2026
#Date modified: 

#Input requirements: Jul9Doc_Rep_AllBAPID.csv; path_report_2026-10-01_1517.csv

#Output: TEI_EcoCat_file_comparison.csv; BAPID_inventory.csv

#===========================================================================================
###Import and read packages
#===========================================================================================
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#===========================================================================================
###Set working directory and output directory
#===========================================================================================
setwd("<shared_drive_path>")

#===========================================================================================
###Import data
#===========================================================================================
#Import TEI database summary
TEIData <- read.csv("<shared_drive_path>/path_report_2026-10-01_1517.csv")

#View TEI database summary
#TEIData %>% View('TEIData')

#Import Ecocat database summary
EcoCatData <- read.csv("<shared_drive_path>/Jul9Doc_Rep_AllBAPID.csv")

#View Ecocat database summary
#EcoCatData %>% View('EcoCatData')

#===========================================================================================
###Functions and Objects
#===========================================================================================
#Clean up and create a new field for the concise filename,
#removing any sharepoint IDs and file path info
normalize_name <- function(x) {
  
  #Extract the full unedited file name from the file path
  file <- basename(x)
  
  #Extract the file extension
  ext <- str_extract(file, "\\.[^.]+$")
  
  #Extract file name
  stem <- str_remove(file, "\\.[^.]+$")
  
  #Remove the sharepoint characters at end of file name
  stem <- str_remove(stem, "_\\d+_[A-Za-z0-9]+$")
  
  #Paste the cleaned file name back with the file extension
  paste0(stem, ext)
}

#List of GIS file extensions
#Used Copilot LLM to identify and categorize file types
gis_extensions <- c(
  
  # Shapefiles
  "shp", "shx", "dbf", "prj", "sbn",
  "sbx", "cpg", "qix",
  
  # ArcInfo / ArcGIS
  "e00", "adf", "atx", "arc", "aml",
  "apr", "avp", "dir", "nsp", "pal",
  "pat", "pax", "rxp", "sis", "tat",
  "tbn", "tbx", "txx", "aih", "ain",
  "gen", "rat",
  
  # ArcInfo internals
  "dat", "nit", "freelist",
  
  # Geodatabases
  "gdb", "_gdb",
  
  # File geodatabase internals
  "gdbindexes", "gdbtable", "gdbtablx", "gdbtabidx",
  
  # Geodatabase support / index files
  "spx", "sdc", "cdf",
  
  # Enterprise geodatabases
  "sde",
  
  # ArcGIS documents / packages
  "mxd", "aprx", "lyr", "lyrx", "atbx",
  "mxt", "esriaddin", "mpk", "mpkx", "ppkx",
  "tpkx",
  
  # Raster formats
  "tif", "tiff", "img", "ecw", "sid",
  "ers", "dem", "vrt", "grd", "asc",
  "pix",
  
  # Raster support
  "tfw", "tfwx", "jgw", "jgwx", "gfw",
  "pgw", "aux", "ovr", "rrd", "hdr",
  "clr", "thm", "wld",
  
  # Grid formats
  "sgrd", "sdat", "mgrd",
  
  # GIS exchange
  "gpkg", "kml", "kmz", "qgz",
  
  # LiDAR
  "las", "lasx", "laz",
  
  # MapInfo
  "tab", "map", "id", "ind",
  
  # CAD / Engineering
  "dgn", "dwg", "dxf", "cad", "pln",
  "hpg", "mbn", "mbx",
  
  # Coordinate systems / projections
  "geo", "utm", "kutm", "lcc", "mer",
  "tm", "prx", "alb",
  
  # Terrain / Elevation
  "tin", "fc1", "int", "ter", "rep",
  "ctr", "aee", "raw", "npo", "esa",
  "are", "ztr", "trm", "slp", "wet",
  
  # GIS coordinates / point data
  "xyz",
  
  # Polygon / Coverage formats
  "ply", "pol",
  
  # Terrain / GIS modelling
  "nt",
  
  # Misc GIS support
  "avl", "horizon", "gis", "rtl", "mod",
  "meta", "veg", "mshp", "ad", "wor",
  "moe", "gic", "gip", "giw", "gix",
  
  # Legacy ArcInfo sidecars
  "0", "000", "001", "003", "004",
  "005", "006", "007", "008", "009",
  "013", "020", "025", "031", "032",
  "051", "065", "068", "00a", "00b",
  "00c", "00d", "00e", "00f",
  
  "0.001", "0.002", "0.003", "0.004", "0.005",
  "0.006", "0.007", "0.008", "0.009", "0.01",
  "0.011", "0.012", "0.013", "0.014", "0.015",
  "0.016", "0.017", "0.018", "0.019", "0.02",
  "0.021", "0.022", "0.023", "0.025", "0.031",
  "0.032", "0.05", "0.051", "0.06", "0.062",
  "0.065", "0.066", "0.068", "0.069", "0.07",
  "0.071", "0.08", "0.283", "0.383",
  
  "05e", "05f", "06c", "06e", "0b9"
)

potentially_spatial <- c(
  "json", "zip"
)

known_extensions <- c(
  
  # Office
  "doc", "docx", "xls", "xlsx", "xlsm",
  "xlw", "xla", "xlt", "ppt", "pptx",
  
  # Text / markup
  "txt", "csv", "rtf", "xml", "html",
  "htm", "md", "sql", "sgml",
  
  # Images
  "jpg", "jpeg", "png", "gif", "bmp",
  "jfif", "wmf", "svg",
  
  # Archives
  "zip", "zipp", "z!p", "7z", "rar",
  "tar", "gz", "zil", "zipfile", "uzp",
  "cab",
  
  # Databases
  "mdb", "accdb", "accdr", "accd", "db",
  "ldb", "sdf", "fpt", "dbo", "laccdb",
  "odb",
  
  # Programming / scripts
  "py", "pyc", "ipynb", "bat", "awk",
  "r", "rmd", "qmd", "bas", "prg",
  "css", "ins", "lib", "pkg", "sas",
  
  # System / executable
  "exe", "dll", "msi", "pif", "ini",
  "cfg", "log", "dmp", "com",
  
  # Email / Office support
  "msg", "pub", "lnk", "wbk", "onetoc2",
  
  # Media
  "mp4", "vob", "ifo", "bup",
  
  # Graphics / publishing
  "eps", "cdr", "ps", "plt",
  
  # Reports / listings
  "pdf", "lis", "lst", "list", "rpt",
  "prn", "prt", "sum", "vif",
  
  # Configuration / metadata
  "url", "inf", "ctl", "rdf", "qry",
  "def", "opt", "init", "out",
  
  # Legacy office / database support
  "wk1", "cat", "cha", "tbl", "rsc",
  "dbt", "ndx", "pcp", "aep", "rds",
  
  # Accessory files
  "bak", "tmp", "lock"
)

specialized_legacy <- c(
  
  # Survey / photogrammetry / engineering
  "cam", "cor", "ssf", "ntd",
  
  # Graphics / plotting
  "hp", "hp2", "gra",
  
  # Legacy analysis / modelling
  "rpm", "rpp", "rmo",
  
  # Domain-specific ecological / habitat
  "wat", "leg", "lab", "vegcap", "vegcheck",
  
  # Terrain / engineering
  "de3", "to3", "top", "sur",
  
  # Technical support formats
  "msl", "cel", "mdx", "dbg", "cdx",
  "con",
  
  # Specialized legacy formats
  "eds", "bry", "dia", "lvd", "fer",
  "flr", "flw", "frn", "lal", "sal",
  "mat", "sha", "sjs", "key", "z",
  "lpk", "car", "sdw", "obx", "obs",
  "dd", "gl2", "cfs", "mrf", "fmb",
  "der", "zap", "fdt", "fnm", "frq",
  "nrm", "tii", "tis", "ric", "gsg",
  "sdt", "rpc", "mpp", "rpn", "fal",
  "dfl", "rfl", "vfl",
  
  # Grid / tile systems
  "2d", "3d", "c3d", "m3d", "u3d",
  "n3d", "fc3", "np1", "np3", "pl3",
  "pla", "ra1", "ra3", "tr1", "tr2",
  "z10",
  
  # Legacy map tiles
  "n27", "n44", "n45", "n46", "n47",
  "n53", "n54", "n55", "n61", "n62",
  "n63", "n64", "n65", "n66", "n67",
  "n71", "n72", "n73", "n74", "n75",
  "n77", "n81", "n82", "n83", "n84",
  "n92", "n93",
  
  # Survey / terrain / modelling support
  "prm", "her", "saf", "x", "lev",
  "3dc", "dct", "dic", "roc", "tal",
  "sdl", "sif", "exp", "fmw", "lgf",
  "fir", "itw", "mst", "spl", "cre",
  "mwf", "stk", "lxp", "rse", "aty",
  "low", "thr", "eda", "vcs", "alink",
  "shs", "mea", "coa"
)

#===========================================================================================
###Main code sections
#===========================================================================================
#===========================================================================================
##Data cleaning
#===========================================================================================
#===========================================================================================
#Clean up EcoCat dataset to prepare for comparison
#===========================================================================================
#Normalize the file names from the EcoCat dataset so they can be compared with TEI
EcoCatData$key <- normalize_name(EcoCatData$DOCUMENT_POINTER)
#EcoCatData %>% View('EcoCat Data')

#Rename EcoCat fields for file size and file name
EcoCatData <- EcoCatData %>% rename("FileSize_Bytes" = "FILE_SIZE", "FileName" = "key", "FileType"="FILE_TYPE", "ProjectTitle" = "TITLE.1")

#Add formatting removed column to EcoCat
EcoCatData <- EcoCatData %>% mutate(FileName_Compare = FileName %>% iconv(from = "", to = "UTF-8", sub = "") %>%
                                      str_to_lower() %>% str_remove_all("[ _-]"))

#The old reference system field is not normalized since there are multiple different ways of recording BAPID
#EcoCatData %>% distinct(OLD_REFERENCE_SYSTEM) %>% View()

#Rows with a blank for old reference system seem to have the BAPID described elsewhere
#EcoCatData %>% filter(OLD_REFERENCE_SYSTEM == "") %>% View()

#Normalize OLD_REFERENCE_NUMBER field by removing text, and adding commas between multiple numbers
EcoCatData <- EcoCatData %>% mutate(BAPID_raw = OLD_REFERENCE_NUMBER %>%
                                      str_to_upper() %>%
                                      str_replace_all("BAPIDS", "") %>%
                                      str_replace_all("BAPID", "") %>%
                                      str_replace_all("AND", ",") %>%
                                      str_replace_all("TEM", "") %>%
                                      str_replace_all("SEI", "") %>%
                                      str_replace_all("WHA", "") %>%  
                                      str_replace_all("WHR", "") %>%
                                      str_replace_all("REDIGITIZING", "") %>%
                                      str_replace_all("ORIGINAL", "") %>%
                                      str_replace_all("MESSINTER", "") %>%
                                      str_replace_all("ALBREDA", "") %>%
                                      str_replace_all("\\(", "") %>%
                                      str_replace_all("\\)", "") %>%
                                      str_replace_all(";", ",") %>%
                                      str_replace_all("_", ",") %>%
                                      str_replace_all("&", ",") %>%
                                      str_replace_all("# ", "") %>% 
                                      str_replace_all("- ", "") %>% 
                                      str_replace_all(", ,", ",") %>% 
                                      str_remove_all("\\b\\d{5,}(-[^ ,;]*)?\\b") %>%
                                      str_replace_all(",\\s*,", ",") %>%
                                      str_squish()
)

#EcoCatData %>% distinct(BAPID_raw) %>% View() 
#EcoCatData %>% filter(grepl("\\(", BAPID_raw)) %>% View() 

#Some of the blank rows in old reference system, have a bapid number in the document name
#Add those report numbers to the old reference number field
#EcoCatData %>% filter(OLD_REFERENCE_SYSTEM == "" & grepl("(?i)bapid[ _]?[0-9]+", DOCUMENT_POINTER)) %>% View()
EcoCatData <- EcoCatData %>% mutate(BAPID_raw = ifelse(OLD_REFERENCE_SYSTEM == "" & grepl("(?i)bapid[ _]?[0-9]+", DOCUMENT_POINTER, perl = TRUE),
                                                       str_extract(DOCUMENT_POINTER, "(?i)(?<=bapid[ _]?)[0-9]+"), BAPID_raw))

#There are still multiple BAPIDs per line, separated by commas - put each on own line
EcoCatData <- EcoCatData %>% mutate(BAPID_raw = str_remove(BAPID_raw, ",$"))
EcoCatData  <- EcoCatData %>% separate_rows(BAPID_raw, sep = ",") %>% mutate(BAPID_raw = trimws(BAPID_raw))

#Some BAPID numbers are still blank - investigate
#These look like they lack any info about BAPID # They will be excluded
#EcoCatData  %>% filter(BAPID_raw == "") %>% View()

#Filter for any BAPIDs that are not just one to four characters
#EcoCatData  %>% filter(!grepl("^\\d{1,4}$", BAPID_raw) & BAPID_raw != "") %>% View()

#Compare original and cleaned version of BAPIDs
#EcoCatData  %>% filter(OLD_REFERENCE_NUMBER != BAPID_raw) %>% select(OLD_REFERENCE_NUMBER, BAPID_raw) %>% View()

#One row with BAPID "119-425100-41500" got over trimmed by the data cleaning
#Set it back to original value
#EcoCatData  %>% filter(OLD_REFERENCE_NUMBER == "119-425100-41500") %>% View()
EcoCatData  <- EcoCatData  %>% mutate(BAPID_raw = ifelse(OLD_REFERENCE_NUMBER == "119-425100-41500", OLD_REFERENCE_NUMBER, BAPID_raw)) 

#These six BAPIDs should be included but may be missing the word BAPID
#EcoCatData  %>% filter(REPORT_ID %in% c(10227, 11178, 10430, 35937, 40140, 59061)) %>% View() #Records are there but missing info not able to populate all

#===========================================================================================
# Clean up TEI dataset to prepare for comparison
#===========================================================================================
#Remove . before extension type in TEI dataset for comparison
TEIData <- TEIData %>% mutate(Extension = str_remove(Extension, "^\\."))
#TEIData %>% View('TEI Data')

#Rename TEI fields for file size and file name 
TEIData <- TEIData %>% rename("FileSize_Bytes" = "SizeBytes", "FileName" = "Name", "FileType" = "Extension")

#Add a column where spaces, underscores and dashes are removed to compare for files with slightly different format
#Add formatting removed column to TEI
TEIData <- TEIData %>% mutate(FileName_Compare = FileName %>% iconv(from = "", to = "UTF-8", sub = "") 
            %>% str_to_lower() %>% str_remove_all("[ _-]"))

#Extract the BAPID number from the file path
TEIData <- TEIData %>% mutate(BAPID_number = str_extract(DirectoryName, "(?<=TEI_data\\\\)\\d+(?=_)"))

#===========================================================================================
#Create datasets containing distinct BAPIDs to compare
#===========================================================================================
#List of all BAPID numbers from TEI
BAPIDS_TEI <- TEIData %>% filter(!is.na(BAPID_number)) %>% distinct(BAPID_number) %>% arrange(BAPID_number)

#Check that there is one BAPID per line for comparison
BAPIDS_TEI %>% filter(grepl(",", BAPID_number)) %>% distinct(BAPID_number) #no rows with commas separating BAPIDs

#List of BAPIDs with files (excluding FolderInstructions.txt) in TEI distribution folders
TEI_Distribution <- TEIData %>% filter(grepl("\\\\Distribution", iconv(DirectoryName, from = "", to = "UTF-8", sub = "")), FileName != "FolderInstructions.txt") %>% 
mutate(BAPID_number = str_extract(DirectoryName, "(?<=TEI_data\\\\)\\d+(?=_)")) %>% filter(!is.na(BAPID_number)) %>% distinct(BAPID_number) %>% arrange(BAPID_number)

#TEI_Distribution %>% View('TEI_Distribution')

#List of BAPIDs with empty distribution folders
Empty_Distribution_Folders <- TEIData %>% filter(
grepl("\\\\Distribution", iconv(DirectoryName, from = "", to = "UTF-8", sub = ""))) %>% group_by(BAPID_number) %>% summarise(
n_files = n(), n_content_files = sum(FileName != "FolderInstructions.txt"), .groups = "drop") %>%
filter(n_content_files == 0)  #n = 89 folders with only FolderInstructions.txt

#Empty_Distribution_Folders %>% View('Empty_Distribution_Folders')

#Check that there is one BAPID per line for comparison
TEI_Distribution %>% filter(grepl(",", BAPID_number)) %>% distinct(BAPID_number) #no rows with commas separating BAPIDs

#List of all EcoCat BAPIDs for comparison
EcoCat_BAPID_list <- EcoCat_BAPIDs %>% filter(!is.na(BAPID_raw) & BAPID_raw != "") %>% distinct(BAPID_raw) %>% arrange(BAPID_raw)

#Check that there is one BAPID per line for comparison
EcoCat_BAPID_list %>% filter(grepl(",", BAPID_raw)) %>% distinct(BAPID_raw) #no rows with commas separating BAPIDs

#Summarize EcoCat data by unique BAPID number to use in future joins so they are one-to-one on BAPID number
EcoCat_BAPID_summary <- EcoCatData %>% filter(!is.na(BAPID_raw), BAPID_raw != "") %>%
  group_by(BAPID_raw) %>% summarise(
    Report_Count = n_distinct(REPORT_ID),
    REPORT_ID = paste(sort(unique(REPORT_ID)), collapse = ","),
    ProjectTitle = paste(sort(unique(ProjectTitle)), collapse = "; "),
    UTM_EASTING = first(UTM_EASTING),
    UTM_NORTHING = first(UTM_NORTHING),
    UTM_ZONE = first(UTM_ZONE),
    .groups = "drop"
  )
#===========================================================================================
#Count unique BAPIDs in each dataset
#===========================================================================================
#Number of unique BAPIDs in complete TEI dataset
nrow(BAPIDS_TEI) #n = 1502

#Number of unique BAPIDs in complete EcoCat dataset
nrow(EcoCat_BAPID_list) #n = 911

#Number of unique BAPID numbers that occur in TEI distribution folders
nrow(TEI_Distribution) #n = 844

#===========================================================================================
#Compare number of unique BAPID numbers in each dataset and search for discrepancies
#===========================================================================================
#BAPIDs in TEI but not in EcoCat:
TEInotinEcoCat <- BAPIDS_TEI %>% filter(!(BAPID_number %in% EcoCat_BAPID_list$BAPID_raw))

#View TEInotinEcoCat
TEInotinEcoCat %>% View('TEInotinEcoCat') #n = 919

#BAPIDs in EcoCat but not in TEI
EcoCatnotTEI <- EcoCat_BAPID_list %>% filter(!(BAPID_raw %in% BAPIDS_TEI$BAPID_number))

#Join ReportID and Project Title
EcoCatnotTEI  <- left_join(
  EcoCatnotTEI,
  EcoCat_BAPID_summary,
  by = c("BAPID_raw" = "BAPID_raw"),
  relationship = "one-to-one") 

#Rename and select fields
EcoCatnotTEI <-EcoCatnotTEI %>% rename("BAPID" = "BAPID_raw") %>% select(BAPID, REPORT_ID, ProjectTitle)
EcoCatnotTEI %>% View('EcoCatnotTEI') # n= 328

#Are there any BAPIDs on EcoCat that are not in TEI Distribution folders?
EcoCatnotDist <- EcoCat_BAPID_list %>% filter(!(BAPID_raw %in% TEI_Distribution$BAPID_number))

#Join ReportID and Project Title
EcoCatnotDist  <- left_join(
EcoCatnotDist,
EcoCat_BAPID_summary,
by = c("BAPID_raw" = "BAPID_raw"),
relationship = "one-to-one") 

#Rename and select fields
EcoCatnotDist <- EcoCatnotDist%>% rename("BAPID" = "BAPID_raw") %>% select(BAPID, REPORT_ID, ProjectTitle)
EcoCatnotDist %>% View('EcoCatnotDist') # n= 422

#Are there any BAPIDs in TEI Distribution folders that are not on EcoCat?
DistnotEcoCat <- TEI_Distribution %>% filter(!(BAPID_number %in% EcoCat_BAPID_list$BAPID_raw))
DistnotEcoCat %>% View('DistnotEcoCat') # n= 355

#BAPIDs on TEI that don't have a distribution folder
TEI_no_distribution <- BAPIDS_TEI %>% filter(!BAPID_number %in% TEI_Distribution$BAPID_number)
TEI_no_distribution %>% View('TEI_no_distribution') # n= 658

#Flag BAPIDs with no files in the Distribution folder but reports, legends, field data etc occur in other folder locations.
BAPIDs_missing_distribution <- bind_rows(TEI_no_distribution %>% transmute(BAPID = BAPID_number, Source = "TEI"),
  EcoCatnotDist %>% transmute(BAPID, Source = "EcoCat")) %>% distinct(BAPID) %>% arrange(BAPID)
BAPIDs_missing_distribution %>% View('BAPIDs missing distribution') #n=1080

#Join ReportID and Project Title
BAPIDs_missing_distribution  <- left_join(
  BAPIDs_missing_distribution,
  EcoCat_BAPID_summary,
  by = c("BAPID" = "BAPID_raw"),
  relationship = "one-to-one",
  multiple = "error"
)

#Rename and select fields
BAPIDs_missing_distribution <- BAPIDs_missing_distribution %>% select(BAPID, REPORT_ID, ProjectTitle)
#BAPIDs_missing_distribution %>% View('BAPIDs_missing_distribution')
                                     
#Flag any possible matches where file name differs but size in bytes and 
# #file type are the same - these files may have been renamed by people at some point
#####
# Examined this but the potential matches are many to many and it doesn't help narrow things down much
#####

#===========================================================================================
#Create a table organized by unique BAPIDs
#===========================================================================================
#Compile BAPID numbers into one dataframe
BAPID_inventory <- bind_rows(BAPIDS_TEI %>% transmute(BAPID = BAPID_number), 
                  EcoCat_BAPID_list %>% transmute(BAPID = BAPID_raw)) %>% distinct() %>%
  
#Assign columns for which datasets the BAPID occurs in TEI, Distribution or EcoCat
  mutate(
    In_TEI = BAPID %in% BAPIDS_TEI$BAPID_number,
    In_EcoCat = BAPID %in% EcoCat_BAPID_list$BAPID_raw,
    In_Distribution = BAPID %in% TEI_Distribution$BAPID_number
  )

#Join Ecocat data
BAPID_inventory <- left_join(
  BAPID_inventory,
  EcoCat_BAPID_summary,
  by = c("BAPID" = "BAPID_raw"),
  relationship = "one-to-one"
)

#Rearrange columns before export
BAPID_inventory <- BAPID_inventory %>% select(BAPID, REPORT_ID, ProjectTitle, In_TEI, In_EcoCat, In_Distribution, Report_Count, UTM_EASTING, UTM_NORTHING, UTM_ZONE)

#BAPID_inventory %>% View('BAPID Inventory')
#===========================================================================================
#Join TEI and EcoCat
#===========================================================================================
#Summarize TEI Data
tei_summary <- TEIData %>% group_by(FileName_Compare, FileSize_Bytes, FileType) %>%
  summarise(FileName_TEI = first(FileName), BAPID_number_TEI = paste(unique(BAPID_number), collapse = ","), TEI_Count = n(), .groups = "drop")

#Summarize EcoCat Data
EcoCat_summary <- EcoCatData %>% group_by(FileName_Compare, FileSize_Bytes, FileType) %>%
  summarise(FileName_EcoCat = first(FileName), BAPID_number_Ecoct = paste(sort(unique(BAPID_raw)), collapse = ","), 
            TITLE = first(TITLE), ProjectTitle = first(ProjectTitle), REPORT_ID = first(REPORT_ID), SHORT_DESCRIPTION = first(SHORT_DESCRIPTION), EcoCat_Count = n(), .groups = "drop")

#Join the summaries of each dataset to compare
comparison <- full_join(
  tei_summary,
  EcoCat_summary,
  by = c("FileName_Compare", "FileSize_Bytes", "FileType"),
  suffix = c("_TEI", "_EcoCat"),
  relationship = "one-to-one"
)

#Replace NA values with 0
comparison <- comparison %>% mutate(TEI_Count = coalesce(TEI_Count, 0), EcoCat_Count = coalesce(EcoCat_Count, 0))

#Set FileType to lower case
comparison <- comparison %>% mutate(FileType = tolower(FileType))

#Calculate file Status and spatial data fields based on conditions
comparison <- comparison %>% mutate(
      Status = case_when(
       #A. Flag EcoCat records where we have files that do not occur in TEI Distribution folders
       TEI_Count == 0 ~ "Only in EcoCat",

       #B. Flag any records where we have files in TEI Distribution folders that aren’t on EcoCat
       EcoCat_Count == 0 ~ "Only in TEI",
       
       #C. Flag any exact matches of file size, file type, and File Name
       FileName_TEI == FileName_EcoCat & TEI_Count == EcoCat_Count ~ "Exact Match",
       
       #D. Flag any matches of file size, file type and formatting removed File Name
       TEI_Count == EcoCat_Count ~ "Formatting Match",
       
       #E. Flag any records where file count differs between TEI and EcoCat
       TEI_Count != EcoCat_Count ~ "Count differs",
       
       TRUE ~ "Review"),
     
       #F. Flag EcoCat records with spatial data (shapefiles, gdb or e00 files for example)
       File_Category = case_when(
          FileType %in% gis_extensions ~ "GIS",
          grepl("\\.gdb", FileName_Compare, ignore.case = TRUE) ~ "GIS",
          grepl("\\.mxd", FileName_Compare, ignore.case = TRUE) ~ "GIS",
          FileType  %in% potentially_spatial ~ "Potentially GIS",
          grepl("polygon|spatial|coverage|linework|shapefile|arcinfo|arcview|e00|geodatabase|gdb|microstation",
          paste(TITLE, SHORT_DESCRIPTION), ignore.case = TRUE) ~ "Potentially GIS",
          FileType  %in% known_extensions ~ "Known Non-GIS",
          FileType  %in% specialized_legacy ~ "Specialized Legacy",
          TRUE ~ "Review"
        ),
      
   )

#comparison %>% View('Comparison') #n= 151302

#===========================================================================================
#Review comparison - this is arranged by unique files rather than by unique BAPIDs as above
#===========================================================================================
#View files that were matched in both EcoCat and TEI based on file name, file size and file type
comparison %>% filter(Status == "Exact Match") %>% View('Exact Match') # n = 1447

#These files were matched on file name, file size and file type
#with essentially the same name minus formatting (spaces, dashes, underscores)
comparison %>% filter(Status == "Formatting Match") %>% View('Formatting Match') # n = 81

#These files were matched on file name, file size and file type and only differ in the count in each dataset
comparison %>% filter(Status == "Count differs") %>% View('Count differs') # n = 2342

#Matches of any kind including exact, formatting and count differs
allmatches <- comparison %>% filter(Status == "Count differs"|Status == "Formatting Match"|Status == "Exact Match") # n = 3960

#Check what file types are in "Review" category
comparison %>% filter(File_Category == "Review") %>% distinct(FileType) %>% View('Uncategorized file types')

#Rearrange columns before export
comparison <- comparison %>% select(FileName_TEI, FileName_EcoCat, FileType, BAPID_number_TEI, BAPID_number_Ecoct, TEI_Count, EcoCat_Count, FileSize_Bytes, Status, File_Category)

#===========================================================================================
### Export files
#===========================================================================================
#Export inventory comparison final results
write.csv(comparison, file = "TEI_EcoCat_file_comparison.csv")

#Export BAPID inventory results
write.csv(BAPID_inventory, file = "BAPID_inventory.csv")

#List of BAPIDs only in TEI not EcoCat
write.csv(TEInotinEcoCat, file = "TEInotinEcoCat.csv")

#List of BAPIDs in EcoCat not TEI
write.csv(EcoCatnotTEI, file = "EcoCatnotTEI.csv")

#List of BAPIDs in EcoCat but not TEI distribution folders 
write.csv(EcoCatnotDist, file = "EcoCatnotDist.csv")

#List of BAPIDs in TEI distribution folders but not EcoCat 
write.csv(DistnotEcoCat, file = "DistnotEcoCat.csv")

#List of BAPIDs on TEI that don't have a distribution folder
write.csv(TEI_no_distribution, file = "TEI_no_distribution.csv")

#BAPIDs with no files in the Distribution folder but reports, legends, field data etc occur in other folder locations
write.csv(BAPIDs_missing_distribution, file = "BAPIDs_missing_distribution.csv")

#BAPIDs with empty distribution folders
write.csv(Empty_Distribution_Folders, file = "Empty_distribution_folders.csv")

