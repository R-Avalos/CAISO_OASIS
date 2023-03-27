
# Oasis Testing -----------------------------------------------------------
# http://www.caiso.com/Documents/OASIS-InterfaceSpecification_v5_1_1Clean_Fall2017Release.pdf

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(dplyr, readr, tidyr, stringr, ggplot2, lubridate, httr, progress, jsonlite, xml2, XML)


base_url <- "http://oasis.caiso.com/oasisapi/"

# Hours from GMT
tz_hrs_from_gmt <- str_pad(string = paste0(round(as.numeric(force_tz(with_tz(Sys.time(), "GMT")) - Sys.time())), "00"), width = 4, side = "left", pad = "0")


# Convert date to OASIS date time format
# "20220101T07:00-0000"

caiso_datetime <- function(date = Sys.Date(), time = "00:00", timezone = tz_hrs_from_gmt) {
  
  # specific format for CAISO date times in queries
  paste0(str_remove_all(as.character(Sys.Date()), "-"), 
         "T",
         as.character("00:00"),
         "-",
         str_pad(tz_hrs_from_gmt, width = 4, side = "left", pad = "0")
  )
}

caiso_datetime()





# XML errors --------------------------------------------------------------

xml_error_reference <- tibble(error_code = 1000:1020,
                              error = c("No data returned for the specified selection",
                                        "No data returned for the specified selection",
                                        "Invalid datetime format, please use valid datetime format",
                                        "Timed out waiting for query response",
                                        "Data can be requested for period of 31 days only",
                                        "Report name does not exist, please use valid report name",
                                        "Validation exception during transformation of XML",
                                        "Required file for does not exist",
                                        "Out of memory exception",
                                        "Exceptions in reading and writing of XML files",
                                        "System Error",
                                        "Empty Query; Please Enter Report Name, Startdate, EndDate and Other Parameters",
                                        "Connection refused", 
                                        "Required Resources (xslt or xml or dir) Unavailable",
                                        "Start Date is beyond the limit, Please Use valid Start Date that falls within the prescribed limit",
                                        "GroupZip DownLoad is in Processing, Please Submit request after Sometime",
                                        "GROUPID Does Not Exist, Please Use Valid GROUPID Name",
                                        "Please select a maximum of 10 nodes or use the ALL option",
                                        "Invalid Selection, cannot select multiple hours for this query",
                                        "market_term=ALL not supported for this query",
                                        "Version parameter is missing or is invalid")
                              )
xml_error_reference


# Base Parameters ---------------------------------------------------------
# Base Path 
oasis_api <- function(path) {
  url <- modify_url(base_url, path = path)
  GET(url)
}
api_version = "1"



# http://www.caiso.com/Documents/OASIS-InterfaceSpecification_v5_1_1Clean_Fall2017Release.pdf


http_status(GET(paste0("http://oasis.caiso.com/oasisapi/SingleZip?",
                       "queryname=AS_REQ&",
                       "stardatetime=20210101T07:00-0700&",
                       "enddatetime=20210102T07:00-0700&",
                       "market_run_id=DAM&",
                       "version=1&",
                       "as_type=ALL&",
                       "as_region=ALL")))


# Atlas - Price Node Locations ----------------------------------------------------
#Pnode Listing
#APNode Listing (Aggregated pricing nodes)
# ATL_PNODE_MAP Map of all Pnodes to each Trading Hub APNode.
# ATL_HUB  All Trading Hub APNodes in CAISO.

http_status(GET(paste0("http://oasis.caiso.com/oasisapi/SingleZip?",
                       "queryname=ATL_PNODE",
                       "&",
                       "Pnode_id=12THST_6_N1010",
                       "&",
                       "Pnode_type=ALL", 
                       "stardatetime=20210101T07:00-0700",
                       "&",
                       "enddatetime=20210102T07:00-0700",
                       "&",
                       "version=1")))

# x <- GET(
#   paste0("http://oasis.caiso.com/oasisapi/SingleZip?",
#          "queryname=ATL_PNODE",
#          "&",
#          "Pnode_id=0096WD_7_N001",
#          "&",
#          # "Pnode_type=ALL", 
#          "stardatetime=20210101T07:00-0000",
#          "&",
#          "enddatetime=20210102T07:00-0000",
#          "&",
#          #"result_format=6&",
#          "version=1")
#   )

x <- GET(
  paste0("http://oasis.caiso.com/oasisapi/SingleZip?",
         "queryname=ATL_PNODE",
         "&",
         "APnode_type=ALL", 
         "&",
         "startdatetime=20210601T07:00-0000",
         "&",
         "enddatetime=20210601T07:00-0000",
         "&",
         "version=1")
  )
x
x$status_code
x$headers
#http://oasis.caiso.com/oasisapi/SingleZip?queryname=ATL_APNODE&APnode_type=ALL&startdatetime=20130919T07:00-0000&enddatetime=20130920T07:00-0000&version=1

xmlParseDoc(content(x, type = "text"))
unzip(content(x, type = "text"))
content(x, type="text")
fromJSON(content(x, type = "text"), simplifyVector = FALSE)
content(x)
http_type(x)

x$content
download.file(x$content, temp_file)

temp_file <- tempfile() # create temp file
writeBin(content(x), temp_file) # write zip to temp file
unzip(temp_file, exdir = getwd()) # unzip the file
file.remove(temp_file) # remove the temp file

list.files()
test <- xmlParse(file = "20210601_20210601_ATL_PNODE_N_20220205_14_12_18_v1.xml")
# test ### don't run this... :/
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
root <- xmlRoot(test)
xmlSize(root)
root$MessageHeader

payload <- root[2]

payload$MessagePayload[2]
test <- xmlToDataFrame(payload$MessagePayload)
test <- xmlToDataFrame(payload)

remove(test)
remove(test)

payload$MessagePayload

###################
######

datetime <- paste0("20210101",
                   "T00:00-",
                   tz_hrs_from_gmt)
datetime

enddatetime <- paste0("20210101",
                   "T00:00-",
                   tz_hrs_from_gmt)

enddatetime
base_url

http_status(GET(paste0(base_url)))
http_status(GET("http://oasis.casio.com/oasisapi/"))
paste0(base_url, "SingleZip")

#EMBRCDR_2_N104 sf price node
y <- GET(url = paste0(base_url, "SingleZip"),
         query = list(queryname = "ATL_PNODE",
                      Pnode_type = "ALL", 
                      startdatetime = I(datetime),
                      enddatetime= I(enddatetime),
                      version = "1"),
         verbose())
y
status_code(y)

# y <- GET(url = "http://oasis.caiso.com/oasisapi/SingleZip?queryname=ATL_PNODE&Pnode_id=12THST_6_N101&Pnode_type=ALL&startdatetime=20130919T00:00-0700&enddatetime=20130920T00:00-0700&version=1")
# y


y <- GET(url = "http://oasis.caiso.com/oasisapi/SingleZip?queryname=ATL_PNODE&Pnode_type=ALL&startdatetime=20210101T00:00-0700&enddatetime=20210101T00:00-0700&version=1")
y

#EMBRCDR_2_N104 sf price node
# y <- GET(url = "http://oasis.casio.com/oasisapi/SingleZip?queryname=ATL_PNODE&pnode_id=KEARNY_7_KY2D&startdatetime=20190919T07:00-0800&enddatetime=20190920T07:00-0800&version=1")
# y
# status_code(y)




x <- GET(url = paste0(base_url, "/SingleZip"),
         query = list(queryname = "ATL_PNODE_MAP",
                      pnode_id = "KEARNY_7_KY2D",
                      startdatetime = "20130919T07:00-0000",
                      enddatetime = "20130920T07:00-0000",
                      version = "1"))
x
status_code(x)

# Oasis DAM Group ---------------------------------------------------------

oasis_api_dam_group <- function(groupid = "DAM_LMP_GRP",
                               date_ymd = "2021-01-01",
                               save_to_folder = "./data",
                               version = "1",
                               return_csv = TRUE,
                               path = "/GrouZip"){
  
}
