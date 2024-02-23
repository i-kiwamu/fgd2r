deg2num <- function(lat_deg, lon_deg, zoom) {
  lat_rad <- lat_deg * pi/180
  n <- 2^zoom
  xtile <- floor((lon_deg+180) / 360 * n)
  ytile <- floor((1-asinh(tan(lat_rad)) / pi) / 2 * n)
  return(c(xtile, ytile))
}


#' @importFrom stringr str_glue
#' @importFrom geojsonsf geojson_sf
get_landform_xyz <- function(xtile, ytile, zoom) {
  url <- str_glue(
    "https://cyberjapandata.gsi.go.jp/xyz/experimental_landformclassification1/{zoom}/{xtile}/{ytile}.geojson"
  )
  sf_landform <- tryCatch({
    return(suppressWarnings(geojson_sf(url)))
  }, error = \(e) {
    return(NULL)
  })
}


#' get_fgd_landform
#' @description To get FGD landform vector.
#' @param geom sf object, bbox object, or 4-elements numeric vector of bounding box.
#' @param zoom An integer of zoom level to get (default 14).
#' @return A sf object
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom dplyr case_when nest_by ungroup select left_join
#' @importFrom sf st_bbox st_union st_as_sf st_crs st_transform st_crop
#' @importFrom tidyr expand_grid
#' @importFrom purrr pmap reduce discard
#' @export
#' @examples
#' bbox <- c("xmin" = 143.042, "ymin" = 42.908,
#'           "xmax" = 143.086, "ymax" = 42.924)
#' \dontrun{get_fgd_landform(bbox)}
get_fgd_landform <- function(geom, zoom = 14) {
  bbox <- numeric(4)
  if (is(geom, "bbox")) {
    if (st_crs(geom)$epsg != 4326) {
      warning("CRS of geom is not WGS84. Force converting...")
      geom <- st_transform(geom, 4326)
    }
    bbox <- geom
  } else if (is.vector(geom) & is.atomic(geom) & is.numeric(geom) & length(geom) == 4) {
    if (all(sort(names(geom)) != c("xmax", "xmin", "ymax", "ymin"))) {
      stop("ERROR: Please add names of elements: xmin, xmax, ymin, ymax")
    }
    bbox <- st_bbox(geom, crs = 4326)
  } else if (is(geom, "sf")) {
    if (st_crs(geom)$epsg != 4326) {
      warning("CRS of geom is not WGS84. Force converting...")
      geom <- st_transform(geom, 4326)
    }
    bbox <- st_bbox(geom)
  }

  if (all(bbox == 0)) {
    stop("ERROR: geom object is not valid! Only bbox, sf, and numeric vector of 4 elements are available.")
  }

  xy_ll <- deg2num(bbox["ymin"], bbox["xmin"], zoom)  # (xtile, ytile) at lower left
  xy_ur <- deg2num(bbox["ymax"], bbox["xmax"], zoom)  # (xtile, ytile) at upper right

  list_landform <-
    expand_grid(xs = xy_ll[1L]:xy_ur[1L],
                ys = xy_ll[2L]:xy_ur[2L]) %>%
    pmap(\(xs, ys) get_landform_xyz(xs, ys, zoom)) %>%
    discard(is.null)

  if(length(list_landform) > 0) {
    list_landform %>%
      reduce(rbind) %>%
      nest_by(.data$code) %>%
      mutate(geometry = st_union(.data$data)) %>%
      ungroup() %>%
      dplyr::select(-.data$data) %>%
      left_join(df_landform, by = "code") %>%  # df_landform is internal tibble object stored in R/sysdata.rda.
      st_as_sf()
    st_agr(sf_landform) <- "constant"
    return(st_crop(sf_landform, bbox))
  } else {
    print("ERROR: Any landform is not provided in the target area.")
    return(NULL)
  }
}
