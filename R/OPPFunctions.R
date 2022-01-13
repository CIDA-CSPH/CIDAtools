#'Downloads OPP tracking data from movebank with options to export .csv and .shp files
#'
#'This function downloads OPP tracking data from Movebank and returns a dataframe,
#'sf points, and sf lines. Optional argument to save outputs as .csv and .shp files
#'
#'@param study Movebank project id.
#'@param login Stored Movebank login credentials if provided, otherwise function
#'will prompt users to enter credentials.
#'@param start_month Earliest month (1-12) to include in output.
#'@param end_month Latest month (1-12) to include in output.
#'@param export_csv Logical, should a .csv file be written.
#'@param export_points Logical, should a .shp point file be written.
#'@param export_tracks Logical, should a .shp line file be written.
#'@param export_location Path to folder where results should be saved, defaults
#'to working directory
#'@param export_name File name for saved outputs.
#
#'@export


opp_download_data <- function(study,
                              login = NULL,
                              start_month = NULL,
                              end_month = NULL,
                              export_csv = T,
                              export_points = T,
                              export_tracks = T,
                              export_location = NULL,
                              export_name = NULL
) {

  # Check that export file name was provided
  if (is.null(export_name) & (export_csv == T | export_points == T | export_tracks == T)) {
    stop("An export_name must be provided if data are being exported")
  }

  # Ask for movebank credentials if not provided
  if (is.null(login)) login <- move::movebankLogin()

  # Download data from movebank
  mb_data <- move::getMovebankData(study = study, login = login,
                             removeDuplicatedTimestamps = TRUE,
                             includeExtraSensors = FALSE,
                             deploymentAsIndividuals = FALSE,
                             includeOutliers = FALSE)

  # Extract the minimal fields required
  gps_data <- as(mb_data, 'data.frame') %>%
    dplyr::select(timestamp, location_long, location_lat, sensor_type,
                  ring_id,
                  deployment_id, taxon_canonical_name) %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp), # make times POSIXct for compatibility with OGR
      month = as.numeric(strftime(timestamp, '%m')) # add numeric month field
    )

  # Subset data to months if provided
  if (is.null(start_month) == FALSE) gps_data <- subset(gps_data, gps_data$month >= start_month)
  if (is.null(end_month) == FALSE) gps_data <- subset(gps_data, gps_data$month <= end_month)

  # Make sf points object
  gps_points <- sf::st_as_sf(gps_data,
                         coords = c('location_long', 'location_lat'),
                         crs = mb_data@proj4string@projargs # use movebank proj
  )

  # Make sf lines object
  gps_tracks <- gps_points %>%
    dplyr::group_by(sensor_type,ring_id,deployment_id,taxon_canonical_name) %>%
    dplyr::summarise(
      start_time = min(timestamp),
      end_time = max(timestamp),
      number_locations = dplyr::n(),
      do_union = FALSE,
      .groups = 'drop') %>%
    sf::st_cast("LINESTRING")

  # Make a list of objects to return
  out <- list(
    data = gps_data,
    points = gps_points,
    tracks = gps_tracks
  )

  # Create dsn for writeOGR
  dsn <- ifelse(is.null(export_location), getwd(), export_location)

  # Export requested files
  if (export_csv == T) write.csv(gps_data, paste0(export_location,'/',export_name, "_data.csv"), row.names = F)
  if (export_points == T) rgdal::writeOGR(as(gps_points, 'Spatial'), dsn = dsn, layer = paste0(export_name, "_points.shp"), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  if (export_tracks == T) rgdal::writeOGR(as(gps_tracks, 'Spatial'), dsn = dsn, layer = paste0(export_name, "_tracks.shp"), driver = 'ESRI Shapefile', overwrite_layer = TRUE)

  # return list with [1] raw dataframe, [2] sf points, [3] sf lines
  out

}
