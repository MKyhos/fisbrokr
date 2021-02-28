
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
    initialize = function(url) {
      self$url <- url
      private$service <- infer_service(url)
      private$get_capability(url)
    }
  )
)


# Request methods --------------------------------------------------------------

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
    cap$layers <- tibble::tibble(
      position = as.numeric(extract_feat(res, "Name")),
      title = extract_feat(res, "Title"),
      abstract = extract_feat(res, "Abstract")) %>%
      dplyr::arrange(position)
  }

  self$capability <- cap
  #TODO: Printing the capabilities.
  invisible(self)
}

Request$set("private", name = "get_capability", value = get_capability)
