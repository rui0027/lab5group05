#' OpenStreetMaps via Stamen Tiles
#'
#' The 'stamen_map' contains a function that converts name of a location to map in one of three tiles using ggmap.
#'
#' @field location_data A data frame contains name, coordinates, latitude, longtitude.
#' @field map A list contains data of Stamen map
#'
#' @import methods
#' @importFrom ggmap get_stamenmap ggmap
#' @importFrom httr GET content
#'
get_data<-function(){
  print("Please enter the name of location:")
  name <<- readline()
  print("Please entert a zoom level")
  zoom_l <<- as.numeric(readline())
  cat("Please enter a maptype:")
  map_type <<- readline()
}
openmap_by_name <-function() {
    n_url <- paste0("https://nominatim.openstreetmap.org/search.php?q=",
                   name,
                   "&format=jsonv2")
    url1 <- httr::GET(url = n_url)
    url_text <-  httr::content(url1, "text")
    json <- rjson::fromJSON(url_text)
    location_data <<-  data.frame(
      name = json[[1]]$display_name,
      bbox = json[[1]]$boundingbox,
      lat = json[[1]]$lat,
      lon = json[[1]]$lon
    )
    bbox0 <- as.numeric(
      c(
        left = location_data$bbox[3],
        bottom = location_data$bbox[1],
        right = location_data$bbox[4],
        top = location_data$bbox[2]
      )
    )
    map <<- ggmap(get_stamenmap(
      bbox = bbox0,
      zoom = zoom_l,
      maptype = map_type
    ))
    return(map)
  }
