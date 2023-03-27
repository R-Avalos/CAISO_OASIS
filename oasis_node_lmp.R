# Oasis Node LMP ----------------------------------------------------------
# Get single node locational marginal prices in Day Ahead, 15 Min and 5 Min Real Time

oasis_node_lmp_download <- function(
    node = "SLAP_PGCC-APND",
    market_run_id = c("DAM", "HASP", "RTM"),
    start_date_ymd = "2023-03-01",
    end_date_ymd  = "2023-03-23",
    save_to_folder = "./data", 
    return_csv = TRUE
) {
  ### Tidy inputs to CAISO expectations ####
  
  # API Path
  path = "SingleZip"
  
  # Time format
  start_datetime <- paste0(
    str_remove_all(as.character(ymd(start_date_ymd)), pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )
  
  end_datetime <- paste0(
    str_remove_all(as.character(ymd(end_date_ymd)), pattern = "-"),
    "T00:00-",
    tz_hrs_from_gmt
  )
  
  # trigger CSV return 
  data_format = ifelse(test = return_csv == TRUE, yes = "6", no = "")
  
  # market run id
  market_run_id <- toupper(market_run_id)
  match.arg(market_run_id)
  
  # Market Selection align to query ####
  queryname <- case_when(
    market_run_id == "DAM" ~ "PRC_LMP",
    market_run_id == "HASP" ~ "PRC_HASP_LMP",
    market_run_id == "RTM" ~ "PRC_INTVL_LMP"
  )
  
  # Call API ######
  usethis::ui_info("Querying CAISO Oasis")
  resp <- GET(url = paste0(base_url, path),
              query = list(queryname = queryname,
                           node = node,
                           market_run_id = market_run_id,
                           startdatetime = start_datetime,
                           enddatetime = end_datetime,
                           version = api_version,
                           resultformat = data_format
                           )
              )

  # Errors with call ###
  if(http_type(resp) != "application/x-zip-compressed") {
    stop("API did not return zip file", call.=FALSE)
  }

  if(resp$headers$`content-disposition` == "inline; filename=INVALID_REQUEST.xml.zip;") {
    stop("API returned invalid request. Please check parameters", call. =FALSE)
  }

  if(http_error(resp)) {
    stop("CAISO Oasis API request failed", status_code(resp), call.=FALSE)
  }

  # Unzip file and save to folder ####
  # create folder if it does not exist
  temp_file <- tempfile() # create temp file
  writeBin(content(resp), temp_file) # write contents of response to temp file
  unzip(temp_file, exdir = save_to_folder) # unzip contents to folder
  file.remove(temp_file)
  usethis::ui_info(paste0("Results uznipped to ", save_to_folder))

  # Class for Oasis API
  structure(
    list(
      file_name = resp$headers$`content-disposition`,
      path = paste0(base_url, path)
    ),
    class = "oasis_api"
  )
}

print.oasis_node_lmp_download <- function(x, ...) {
  cat("<OASIS ", x$path, ">\n", sep = "")
  str(x$headers$`content-disposition`)
  invisible(x)
}


## Example
oasis_node_lmp_download(
  node = "SLAP_PGCC-APND",
  market_run_id = "DAM",
  start_date_ymd = "20230301",
  end_date_ymd = "20230303"
  )

