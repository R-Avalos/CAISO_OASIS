######## Public Bid Information ######
# Available T+90 days after date
# PUB_CB_BID

# Note : version 2 will provide GHG product.
# OR
# http://oasis.caiso.com/oasisapi/GroupZip?groupid=PUB_DAM_GRP&startdatetime=201309
# 19T07:00-0000&version=1 (for DAM)


# check date > T+90
"20220101T07:00-0000"


get_bids <- function(bid_type = c("PUB_DAM_GRP"),
                             start_date = "20220101T07:00-0000",
                             end_date = "20220102T07:00-0000",
                             version = 2
) {
  # tidy inputs
  match.arg(bid_type)
  
  
  # check dates are > T+90
  
  
  # query
  usethis::ui_info("Querying CAISO Oasis")
  
  
  zip_file <- GET(
    url = paste0(
      base_url,
      "GroupZip", "?",
      "groupid=", bid_type, "&",
      "startdatetime=", start_date, "&",
      "version=", version
    )
  )

  

  # check that query return a zip file
  if (http_type(zip_file) != "application/x-zip-compressed") {
    stop("API did not return a zip file", call. = FALSE)
  }


  # create local folder to read xml file
  dir.create(file.path(getwd(), "bid_temp_storage"), showWarnings = FALSE)

  # unzip and write results to local
  usethis::ui_info("Unzipping and writing XML to temp folder")
  temp_file <- tempfile() # create temp file
  writeBin(content(zip_file), temp_file) # write zip to temp file
  unzip(temp_file, exdir = paste0(getwd(), "/bid_temp_storage")) # unzip the file
  file.remove(temp_file) # remove the temp file

  # Load XML
  usethis::ui_info(paste0("Reading XML Structure: ", list.files(path = "./bid_temp_storage", full.names = TRUE)))
  bidding_xml <- as_list(read_xml(x = list.files(path = "./bid_temp_storage", full.names = TRUE)))

  # Un-nesting Price Nodes from XML data and convert to data frame
  # usethis::ui_info(paste0("Selecting Price Node Data from XML and Transforming to DataFrame: ", list.files(path = "./bid_temp_storage", full.names = FALSE)))
  # bidding <- tibble::as_tibble(bidding_xml$OASISMaster$MessagePayload) %>%
  #   unnest_longer(RTO) %>%
  #   filter(RTO_id == "ATLS_DATA") %>%
  #   unnest_wider('RTO') %>%
  #   unnest(cols = names(.)) %>%
  #   unnest(cols = names(.)) %>%
  #   readr::type_convert()

  # remove local
  unlink("bid_temp_storage", recursive = TRUE)

  # return(bidding)
  return(bidding_xml)
}



test <- get_bids()

### Returns error if not processed ... error states to retry in a bit or something akin to that
# add statement to retry (retry in a bit, or wait)

# returns large list
tidyr::unnest()
p_load(tibble)
test2 <- test %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  unnest()

test3 <- test %>%
  bind_rows()
