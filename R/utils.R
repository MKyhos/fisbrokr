# misc -------------------------------------------------------------------------

#' Helper function: retrieve Service type from URL. 
infer_service <- function(url) {
  if (stringr::str_detect(url, "wms")) {
    service <- "WMS"
  } else if (stringr::str_detect(url, "wfs")) {
    service <- "WFS"
  }
  return(service)
}





# XML handling -----------------------------------------------------------------


#' XML helper function:
#' 
#' retrieve the layer information for a WMS GetCapabilities response content.
#' @param xml An xml object
#' @param tag <string> Denoting the XML element to be extracted.
extract_feat <- function(xml, tag) {
  xml %>%
    xml2::xml_find_all(paste0(".//Layer/Layer/", tag)) %>%
    xml2::as_list() %>%
    unlist()
} 

