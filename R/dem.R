#' @importFrom stringr str_split_1 str_length str_sub
as_floatr <- function(num_str, min_repeat = 6) {
  split_floating_point <- str_split_1(num_str, "\\.")
  n_split <- length(split_floating_point)
  if (n_split == 2) {
    right <- split_floating_point[2L]
    n_right <- str_length(right)
    if (n_right >= min_repeat) {
      last_char <- str_sub(right, -1)
      last_char_repeat <- paste0(rep(last_char, min_repeat), collapse = "")
      last_str <- str_sub(right, n_right-min_repeat+1, n_right)
      if (last_str == last_char_repeat) {
        return(as.numeric(paste0(c(num_str, rep(last_char, 10)), collapse = "")))
      } else {
        return(as.numeric(num_str))
      }
    } else {
      return(as.numeric(num_str))
    }
  } else {
    return(as.numeric(num_str))
  }
}


#' @importFrom xml2 xml_find_all xml_contents xml_text
#' @importFrom stringr str_split_1
anatomy <- function(xml_doc, xpath) {
  xml_doc %>%
    xml_find_all(xpath) %>%
    xml_contents() %>%
    xml_text() %>%
    str_split_1(" ")
}


#' @importFrom rlang .data
#' @importFrom brio read_file
#' @importFrom stringr str_split_1 str_split_fixed str_trim
#' @importFrom xml2 read_xml
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename mutate if_else
#' @importFrom terra rast values values<-
xml2terra <- function(xml_path) {
  contents <- read_file(xml_path)
  header_body <- str_split_1(contents, "<gml:tupleList>")
  body_footer <- str_split_1(header_body[2L], "</gml:tupleList>")
  squid <- paste0(header_body[1L], body_footer[2L])
  xml_doc <- read_xml(squid)

  lower_corner <- anatomy(xml_doc, "//gml:lowerCorner")
  upper_corner <- anatomy(xml_doc, "//gml:upperCorner")
  xl <- as_floatr(lower_corner[2L])
  xu <- as_floatr(upper_corner[2L])
  yl <- as_floatr(lower_corner[1L])
  yu <- as_floatr(upper_corner[1L])

  size <- anatomy(xml_doc, "//gml:high")
  nx <- as.integer(size[1L]) + 1
  ny <- as.integer(size[2L]) + 1

  start_point <- anatomy(xml_doc, "//gml:startPoint")
  x0 <- as.integer(start_point[1L])
  y0 <- as.integer(start_point[2L])

  data <- body_footer[1L] %>%
    str_trim() %>%
    str_split_1("\n") %>%
    str_split_fixed(",", 2) %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename(Type = .data$V1, Elev = .data$V2) %>%
    mutate(Elev = if_else((.data$Type == "\u30c7\u30fc\u30bf\u306a\u3057") | (.data$Elev < -99), NA, as.numeric(.data$Elev)))

  trr_elev <-
    rast(ncols = nx, nrows = ny, xmin = xl, xmax = xu, ymin = yl, ymax = yu,
         crs = "EPSG:6668", vals = NA)
  values(trr_elev)[(1:nrow(data))+x0+nx*y0, 1] <- data$Elev
  return(trr_elev)
}

#' @importFrom stringr str_split_1 str_subset
#' @importFrom purrr map
#' @importFrom terra sprc merge
#' @importFrom utils unzip
zip2terra <- function(zip_path) {
  zip_path_split <- str_split_1(zip_path, "(/|\\.)")
  zip_name <- zip_path_split[length(zip_path_split)-1]
  tmp_dir <- tempdir()
  out_dir <- file.path(tmp_dir, zip_name)
  unzip(zip_path, exdir = out_dir)
  list_xml <- list.files(out_dir, full.names = TRUE) %>%
    str_subset("\\.xml$")
  result <- list_xml %>%
    map(xml2terra) %>%
    sprc() %>%
    merge()
  return(result)
}

#' read_fgd_dem
#' @description To read FGD DEM file.
#' @param file_path A file path to read (xml or zip).
#' @return An object of \code{SpatRaster}.
#' @importFrom stringr str_extract str_glue
#' @export
#' @examples
#' dem_644320 <- system.file("extdata", "FG-GML-6443-20-DEM5A.zip", package = "fgd2r")
#' trr_644320 <- read_fgd_dem(dem_644320)
read_fgd_dem <- function(file_path) {
  ext <- tolower(str_extract(file_path, "(?<=\\.)[:alpha:]+$"))
  if(ext == "zip") {
    return(zip2terra(file_path))
  } else if(ext == "xml") {
    return(xml2terra(file_path))
  } else {
    stop(str_glue("File extension {ext} is not supported! Only zip or xml are available."))
  }
}
