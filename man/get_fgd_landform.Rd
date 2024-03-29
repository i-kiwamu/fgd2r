% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landform.R
\name{get_fgd_landform}
\alias{get_fgd_landform}
\title{get_fgd_landform}
\usage{
get_fgd_landform(geom, zoom = 14)
}
\arguments{
\item{geom}{sf object, bbox object, or 4-elements numeric vector of bounding box.}

\item{zoom}{An integer of zoom level to get (default 14).}
}
\value{
A sf object
}
\description{
To get FGD landform vector.
}
\examples{
bbox <- c("xmin" = 143.042, "ymin" = 42.908,
          "xmax" = 143.086, "ymax" = 42.924)
\dontrun{get_fgd_landform(bbox)}
}
