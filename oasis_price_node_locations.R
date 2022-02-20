#### Price Node Locations ####
# CAISO Atlas price node location
# Pnode Listing (market price nodes)
# APNode Listing (Aggregated pricing nodes)
# ATL_PNODE_MAP Map of all Pnodes to each Trading Hub APNode
# ATL_HUB  All Trading Hub APNodes in CAISO



# Atlas Price Node --------------------------------------------------------
http_status(GET(paste0("http://oasis.caiso.com/oasisapi/", 
                       "SingleZip?",
                       "queryname=ATL_PNODE",
                       "&",
                       "Pnode_id=12THST_6_N1010",
                       "&",
                       "Pnode_type=ALL", 
                       "stardatetime=",
                       "20220101T07:00-0700",
                       "&",
                       "enddatetime=",
                       "20220102T07:00-0700",
                       "&",
                       "version=1")))




# Download zip
# Extract to temp file
# save xml extraction to local and remove temp

oasis_api_atlas <- function(
  query_name = c("ATL_PNODE", "ATL_APNODE"),
  AP_node_type = "ALL",
  start_date = "20220101T07:00-0000",
  end_date = "20220102T07:00-0000",
  api_version = "1"
  ) {
  
  # tidy inputs
  AP_node_type <- toupper(AP_node_type)
  match.arg(query_name)
  
  # query api
  GET(url = paste0(
    base_url,
    "SingleZip",
    "?",
    "queryname=",
    query_name, 
    "&",
    "APnode_type=",
    AP_node_type,
    "&",
    "startdatetime=",
    start_date,
    "&",
    "enddatetime=",
    end_date,
    "&",
    "version=",
    api_version
    )
    )
}




oasis_atlas_pnodes <- function(node_type = c("ATL_PNODE", "ATL_APNODE")) {
  # single or multiple
  # To do for specific location selection vs return all
  
  # query
  usethis::ui_info("Querying CAISO Oasis")
  zip_file <- oasis_api_atlas(query_name = node_type)
  
  
  # check that query return a zip file
  if (http_type(zip_file) != "application/x-zip-compressed") {
    stop("API did not return a zip file", call. = FALSE)
  }
  
  
  # create local folder to read xml file
  dir.create(file.path(getwd(), "atlas_temp_storage"), showWarnings = FALSE)
  
  # unzip and write results to local
  usethis::ui_info("Unzipping and writing XML to temp folder")
  temp_file <- tempfile() # create temp file
  writeBin(content(zip_file), temp_file) # write zip to temp file
  unzip(temp_file, exdir = paste0(getwd(), "/atlas_temp_storage")) # unzip the file
  file.remove(temp_file) # remove the temp file
  
  # Load XML
  usethis::ui_info(paste0("Reading XML Structure: ", list.files(path = "./atlas_temp_storage", full.names = TRUE)))
  pnode_xml <- as_list(read_xml(x = list.files(path = "./atlas_temp_storage", full.names = TRUE)))
  
  # Un-nesting Price Nodes from XML data and convert to data frame
  usethis::ui_info(paste0("Selecting Price Node Data from XML and Transforming to DataFrame: ", list.files(path = "./atlas_temp_storage", full.names = FALSE)))
  pnode <- tibble::as_tibble(pnode_xml$OASISMaster$MessagePayload) %>%
    unnest_longer(RTO) %>%
    filter(RTO_id == "ATLS_DATA") %>%
    unnest_wider('RTO') %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    readr::type_convert() 
  
  # remove local
  unlink("atlas_temp_storage", recursive = TRUE) 
  
  return(pnode)
}


# Return Price Nodes ------------------------------------------------------
# individual nodes
price_nodes <- oasis_atlas_pnodes(node_type = "ATL_PNODE")

# aggregate nodes / sub-load aggregation points
AP_nodes <- oasis_atlas_pnodes(node_type = "ATL_APNODE")

