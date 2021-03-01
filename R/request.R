
# Request ----------------------------------------------------------------------


#' Fisbroker Request objects
#' 
#' @name Request
#' @description 
#' @section Methods: `Request` objects have the following associated
#'  methods
#' 
#' | **Method** | **Description** |
#' | `$get_capability()` | Retrieve the capability of an endpoint |
#' | `$get_dta()` | Retrieve selected data from the service |
#' | `$print()` | Print information about the request, status etc. |
#' 
#' @export
Request <- R6::R6Class(
  classname = "Request",
  private = list(
    parsed = NULL,
    service = NULL
  ),
  public = list(
    url = NULL,
    capability = NULL,
    data = NULL
  )
)


# Request methods --------------------------------------------------------------

#' Intialize / Contructor
#' 
#' @param url <string> The URL pointing to the service / endpoint of
#'  FIS-Broker that you wish to utilize.
request_initialize <- function(url) {
  #TODO check url validity
  assertthat::is.string(url) 
      self$url <- url
      private$service <- infer_service(url)
      private$get_capability(url)
    }

Request$set("public", name = "initialize", value = request_initialize)


#' Get capabilities of an endpoint
#' 
#' @param url
get_capability <- function(url) {

  cap <- list()
  service <- infer_service(url)

  url_p <- httr::parse_url(url)
  url_p$query <- list(
    request = "GetCapabilities",
    service = service
  )

  res <- crul::HttpClient$new(
    url = httr::build_url(url_p),
    opts = list(timeout = 10))$
    get()

  # Raise status error, if applicable  
  res$raise_for_status()

  res <- res$parse(encoding = "UTF-8") %>%
    xml2::read_xml()

  if (service == "WMS") {
    #TODO Implement correct XML/HTML Parsing of capability response
    cap$layers <- tibble::tibble(
      position = as.numeric(extract_feat(res, "Name")),
      title = extract_feat(res, "Title"),

  } else if (service == "WFS") {
    #TODO Implement parsing for capability response from WFS service

  }

  self$capability <- cap
  #TODO: Printing the capabilities.
  invisible(self)
}

Request$set("private", name = "get_capability", value = get_capability)


#' Retrieve defined data from service
#' 
#' @param layers <character> 
get_data <- function(layers = NULL) {
  browser()
  url_p <- httr::parse_url(self$url)
  
  # Fork process, based on service type.

  # Query building:
  if (private$service == "WMS") {
    query <- list(
      version = "1.1.1",
      request = "GetMap",
      format = "image/png",
      styles = "default",
      layers = layers,
      SRS = "EPSG:4258"
    )

  } else if (private$service == "WFS") {
    type_name <- basename(url)
    query <- list(
      version = "2.0.0",
      request = "GetFeature",
      srsName = "EPSG:4258",
      typeNames = type_name
    )
  }
  
  url_p$query <- query

  f <- tempfile()
  client <- crul::HttpClient$new(
    url = httr::build_url(url_p),
    opts = list(timeout = 60),
    progress = httr::progress(type = "down")
    )

  res <- client$get("get", disk = f)
  res$raise_for_status()

  if (private$service == "WMS") {
    data <- raster::brick(f)
  } else if (private$service == "WFS") {
    data <- sf::read_sf(f)
  }

  self$data <- data

}

Request$set("public", name = "get_data", value = get_data)