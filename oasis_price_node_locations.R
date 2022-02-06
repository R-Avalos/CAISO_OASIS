#### Price Node Locations ####
# CAISO Atlas price node location
# Pnode Listing
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
    "queryname=ATL_PNODE",
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


test <- oasis_api_atlas()



oasis_atlas_pnodes <- function() {
  # single or multiple
  # query
  zip_file <- oasis_api_atlas()
  
  
  # create local folder to read xml file
  dir.create(file.path(getwd(), "atlas_temp_storage"), showWarnings = FALSE)
  # unzip and write results to local
  temp_file <- tempfile() # create temp file
  writeBin(content(zip_file), temp_file) # write zip to temp file
  unzip(temp_file, exdir = paste0(getwd(), "atlas_temp_storage")) # unzip the file
  file.remove(temp_file) # remove the temp file
  
  # Load XML
  print(paste0("Loading and Transforming XML ", list.files(path = "./atlas_temp_storage", full.names = TRUE)))
  pnode_xml <- as_list(read_xml(x = list.files(path = "./atlas_temp_storage", full.names = TRUE)))
  
  # Un-nesting Price Nodes from XML data and convert to data frame
  print("Transforming XML to Dataframe")
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


# query
zip_file <- oasis_api_atlas()

# create local folder to read xml file
dir.create(file.path(getwd(), "atlas_temp_storage"), showWarnings = FALSE)
# unzip and write results to local
temp_file <- tempfile() # create temp file
writeBin(content(zip_file), temp_file) # write zip to temp file
unzip(temp_file, exdir = paste0(getwd(), "atlas_temp_storage")) # unzip the file
file.remove(temp_file) # remove the temp file
remove(temp_file)



# Load XML
print(paste0("Loading and Transforming XML ", list.files(path = "./atlas_temp_storage", full.names = TRUE)))
pnode_xml <- as_list(read_xml(x = list.files(path = "./atlas_temp_storage", full.names = TRUE)))

# Un-nesting Price Nodes from XML data and convert to data frame
print("Transforming XML to Dataframe")
pnode <- tibble::as_tibble(pnode_xml$OASISMaster$MessagePayload) %>%
  unnest_longer(RTO) %>%
  filter(RTO_id == "ATLS_DATA") %>%
  unnest_wider('RTO') %>%
  unnest(cols = names(.)) %>%
  unnest(cols = names(.)) %>%
  readr::type_convert() 

unlink("atlas_temp_storage", recursive = TRUE)


#remove(pnode, pnode_xml, test, zip_file)

# test download, transform
# zip_file <- oasis_api_atlas()
# dir.create(file.path(getwd(), "atlas_temp_storage"), showWarnings = FALSE)
# temp_file <- tempfile() # create temp file
# writeBin(content(zip_file), temp_file) # write zip to temp file
# unzip(temp_file, exdir = paste0(getwd(), "/atlas_temp_storage"))
# file.remove(temp_file) # remove the temp file
# 
# # Load XML
# print(paste0("Loading and Transforming XML ", 
#              list.files(path = "./atlas_temp_storage", full.names = TRUE)
#              )
#       )
# 
# pnode_xml <- as_list(read_xml(x = list.files(path = "./atlas_temp_storage", full.names = TRUE)))
# ## Transform xml
# 
# unlink("atlas_temp_storage", recursive = TRUE) 



# temp_file <- tempfile() # create temp file
# writeBin(content(x), temp_file) # write zip to temp file
# unzip(temp_file, exdir = getwd()) # unzip the file


# transform xml
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/



pnode_xml <- as_list(read_xml(x = "20210601_20210601_ATL_PNODE_N_20220205_14_12_18_v1.xml"))

xml_df <- tibble::as_tibble(pnode_xml$OASISMaster$MessagePayload) %>%
  unnest_longer(RTO)


test <- xml_df %>%
  filter(RTO_id == "ATLS_DATA")

test2 <- test %>%
  unnest_wider('RTO')

test3 <- test2 %>% unnest(cols = names(.))

## Transform xml
pnode <- tibble::as_tibble(pnode_xml$OASISMaster$MessagePayload) %>%
  unnest_longer(RTO) %>%
  filter(RTO_id == "ATLS_DATA") %>%
  unnest_wider('RTO') %>%
  unnest(cols = names(.)) %>%
  unnest(cols = names(.)) %>%
  readr::type_convert() 

# Splitting areas will have to deal better with single letter prefixes...
pnode %>%
  unnest(cols = names(.)) %>%
  separate(col = PNODE_NAME, into = c("node_area", "node_area_point"), sep = "_", extra = "merge", remove = FALSE) %>%
  filter(node_area =="S")
  group_by(node_area) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = n)) +
  geom_histogram()
