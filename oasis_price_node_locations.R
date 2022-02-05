#### Price Node Locations ####
# CAISO Atlas price node location
# Pnode Listing
# APNode Listing (Aggregated pricing nodes)
# ATL_PNODE_MAP Map of all Pnodes to each Trading Hub APNode
# ATL_HUB  All Trading Hub APNodes in CAISO


# Atlas Price Node --------------------------------------------------------
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

# Download zip
# Extract to temp file
# save xml extraction to local and remove temp

# transform xml
# https://urbandatapalette.com/post/2021-03-xml-dataframe-r/

library(xml2)
library(tidyverse)
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
  unnest(cols = names(.))
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
