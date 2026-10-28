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


#Description: Inventory and compare files available on TEI Distribution folders and EcoCat database
#to prepare for removing GIS files from EcoCat that are already in TEI

#Flag any records where we have files in TEI Distribution folders that aren’t on EcoCat
#Flag EcoCat records where we have files that do not occur in TEI Distribution folders
#Flag EcoCat records with spatial data (shapefiles, gdb or e00 files for example)
#Flag BAPIDs with no files in the Distribution folder but reports, legends, field data etc occur in other folder locations.

#Date created: September 29, 2026
#Date modified: 

#Input requirements: Jul9Doc_Rep_AllBAPID.csv; path_report_2026-05-21_1252.csv

#Output: TEI_EcoCat_file_comparison.csv
#===========================================================================================
###Import and read packages
#===========================================================================================
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringr")

library(readxl)
library(dplyr)
library(stringr)

#===========================================================================================
###Set working directory and output directory
#===========================================================================================
setwd()

#===========================================================================================
###Import data
#===========================================================================================

#Import TEI database summary
TEIData <- read.csv()

#View TEI database summary
TEIData %>% View('TEIData')

#Import Ecocat database summary
EcoCatData <- read.csv()

#View Ecocat database summary
EcoCatData %>% View('EcoCatData')

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
  "json"
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
#Normalize the file names from the EcoCat dataset so they can be compared with TEI
EcoCatData$key <- normalize_name(EcoCatData$DOCUMENT_POINTER)
#EcoCatData %>% View('EcoCat Data')

#Remove . before extension type in TEI dataset for comparison
TEIData <- TEIData %>% mutate(Extension = str_remove(Extension, "^\\."))
#TEIData %>% View('TEI Data')

#Rename TEI fields for file size and file name 
TEIData <- TEIData %>% rename("FileSize_Bytes" = "SizeBytes", "FileName" = "Name", "FileType" = "Extension")

#Rename EcoCat fields for file size and file name
EcoCatData <- EcoCatData %>% rename("FileSize_Bytes" = "FILE_SIZE", "FileName" = "key", "FileType"="FILE_TYPE")

#Add a column where spaces, underscores and dashes are removed to compare for files with slightly different format
#Add formatting removed column to TEI
TEIData <- TEIData %>% mutate(FileName_Compare = FileName %>% iconv(from = "", to = "UTF-8", sub = "") 
            %>% str_to_lower() %>% str_remove_all("[ _-]"))

#Add formatting removed column to EcoCat
EcoCatData <- EcoCatData %>% mutate(FileName_Compare = FileName %>% iconv(from = "", to = "UTF-8", sub = "") %>%
              str_to_lower() %>% str_remove_all("[ _-]"))

#===========================================================================================
#Compare datasets
#===========================================================================================
#Summarize TEI Data
tei_summary <- TEIData %>% group_by(FileName_Compare, FileSize_Bytes, FileType) %>%
  summarise(FileName_TEI = first(FileName), TEI_Count = n(), .groups = "drop")

#Summarize EcoCat Data
EcoCat_summary <- EcoCatData %>% group_by(FileName_Compare, FileSize_Bytes, FileType) %>%
  summarise(FileName_EcoCat = first(FileName), EcoCat_Count = n(), .groups = "drop")

#Join the summaries of each dataset to compare
comparison <- full_join(
  tei_summary,
  EcoCat_summary,
  by = c("FileName_Compare", "FileSize_Bytes", "FileType"),
  suffix = c("_TEI", "_EcoCat")
)

#Replace NA values with 0
comparison <- comparison %>% mutate(TEI_Count = coalesce(TEI_Count, 0), EcoCat_Count = coalesce(EcoCat_Count, 0))

#Set FileType to lower case
comparison <- comparison %>% mutate(FileType = tolower(FileType))
comparison %>% View()

#Calculate file Status and spatial data fields based on conditions
comparison <- comparison %>% mutate(Status = case_when(
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
     
       #E. Flag EcoCat records with spatial data (shapefiles, gdb or e00 files for example)
       File_Category = case_when(
          FileType %in% gis_extensions ~ "GIS",
          grepl("\\.gdb", FileName_Compare, ignore.case = TRUE) ~ "GIS",
          grepl("\\.mxd", FileName_Compare, ignore.case = TRUE) ~ "GIS",
          FileType  %in% potentially_spatial ~ "Potentially GIS",
          FileType  %in% known_extensions ~ "Known Non-GIS",
          FileType  %in% specialized_legacy ~ "Specialized Legacy",
          TRUE ~ "Review"
        ),

     #E. Flag BAPIDs with no files in the Distribution folder but reports, legends, field data etc occur in other folder locations.
   
   )

#
EcoCatData %>% View()
EcoCatData %>% distinct(OLD_REFERENCE_SYSTEM) %>% View()
EcoCatData %>% filter(OLD_REFERENCE_SYSTEM != "Terrestrial Ecosystem Information (TEI)" & OLD_REFERENCE_SYSTEM != "FIA") %>% distinct(OLD_REFERENCE_NUMBER) %>% View()

BAPID_reports <- EcoCatData %>% filter(grepl("_rpt_", DOCUMENT_POINTER)) #%>% group_by(OLD_REFERENCE_NUMBER) %>% View()

#Are the document ids unique TRUE = Unique
!any(duplicated(BAPID_reports$DOCUMENT_ID)) #They are all unique

BAPID_summary <- BAPID_summary %>% mutate(Has_Report = case_when(

)
  
  
)
#===========================================================================================
#Review comparison
#===========================================================================================
#View files that were matched in both EcoCat and TEI based on file name, file size and file type
comparison %>% filter(Status != "Match") %>% View('Files that match in Ecocat and TEI')

#Check what file types are in "Review" category
comparison %>% filter(File_Category == "Review") %>% distinct(FileType) %>% View('Uncategorized file types')

#Rearrange columns before export
comparison <- comparison %>% select(FileName_TEI, FileName_EcoCat, TEI_Count, EcoCat_Count, FileType, Status, File_Category)

#===========================================================================================
### Export files
#===========================================================================================
#Export comparison final results
write.csv(comparison, file = "TEI_EcoCat_file_comparison.csv")

