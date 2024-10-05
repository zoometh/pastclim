#' Reconstruct biomes based on Köppen-Geiger classification for specific dates in BP, and convert to SpatRaster for export as GeoTiff.
#'
#' @param from_to Numeric vector of dates in BP for generating Köppen-Geiger classification maps.
#' @param dataset Name of the dataset to use, defaulting to "Beyer2020".
#' @param verbose Logical; if TRUE, prints progress messages (default: FALSE).
#' @return A list of terra::SpatRaster objects representing Köppen-Geiger classifications.
#' @examples
#' koeppen_geiger_tif_list <- koeppen_geiger_to_tif()
#' ## Write the first SpatRaster map as a TIF
#' outDir <- "C:/Rprojects/neonet/doc/src/temp/"
#' output_file <- paste0(outDir, names(koeppen_geiger_tif_list[1]))
#' terra::writeRaster(koeppen_geiger_tif_list[[1]], output_file, overwrite=TRUE)
#' 
#' @export
koeppen_geiger_to_tif <- function(from_to = c(-9000, -8000, -7000, -6000),
                                  dataset = "Beyer2020",
                                  verbose = FALSE) {
  lmaps <- list()
  # Create a temporary directory for data storage instead of a temporary file
  temp_dir <- tempdir()
  pastclim::set_data_path(path_to_nc = temp_dir)
  pastclim::download_dataset(dataset = dataset, annual = FALSE, monthly = TRUE)
  
  # Validate 'from_to' to ensure it is numeric
  if (!is.numeric(from_to)) {
    stop("'from_to' must be a numeric vector")
  }
  
  for (i in seq_along(from_to)) {
    when <- from_to[i]
    ky <- paste0(abs(when) / 1000, "k")
    if (verbose) {
      print(paste("* read:", ky))
    }
    
    # Extract monthly temperature and precipitation
    tavg <- pastclim::region_slice(
      time_bp = when,
      bio_variables = c(paste0("temperature_0", 1:9), paste0("temperature_", 10:12)),
      dataset = dataset
    )
    prec <- pastclim::region_slice(
      time_bp = when,
      bio_variables = c(paste0("precipitation_0", 1:9), paste0("precipitation_", 10:12)),
      dataset = dataset
    )
    
    # Create the Köppen classification
    koeppen <- pastclim::koeppen_geiger(
      prec = prec,
      tavg = tavg
    )
    
    lmaps[[length(lmaps) + 1]] <- koeppen
    output_file <- paste0("koeppen_", ky, ".tif")
    names(lmaps)[length(lmaps)] <- output_file
    if (verbose) {
      print(paste0("* map '", output_file, "' has been added to the list"))
    }
  }
  
  if (verbose) {
    print("Done")
  }
  # Unlink the temporary directory to clean up
  unlink(temp_dir, recursive = TRUE)
  return(lmaps)
}