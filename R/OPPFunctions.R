#' Download OPP tracking data from Movebank with options
#' to export .csv and .shp files.
#'
#' This function downloads OPP tracking data from Movebank
#' and returns a dataframe, sf points, and sf lines.
#' Optional argument to save outputs as .csv and .shp files
#'
#'@param study List of Movebank project ids.
#'@param login Stored Movebank login credentials if provided, otherwise function
#'will prompt users to enter credentials.
#'@param start_month Earliest month (1-12) to include in output.
#'@param end_month Latest month (1-12) to include in output.
#'@param season Vector describing the season data can be applied to, eg. 'Breeding (Jun-Jul)
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
                              season = NULL,
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
  if (is.null(season)) season <- NA

  out_data <- out_points <- out_tracks <- data.frame()

  for (ss in study) {

    # Download data from movebank
    mb_data <- move::getMovebankData(study = ss, login = login,
                                     removeDuplicatedTimestamps = TRUE,
                                     includeExtraSensors = FALSE,
                                     deploymentAsIndividuals = TRUE,
                                     includeOutliers = FALSE)

    # Extract the minimal fields required
    gps_data <- as(mb_data, 'data.frame') %>%
      dplyr::select(timestamp, location_long, location_lat, sensor_type,
                    local_identifier, ring_id, taxon_canonical_name, sex,
                    animal_life_stage, animal_reproductive_condition, number_of_events,
                    study_site, deploy_on_longitude, deploy_on_latitude,
                    deployment_id, tag_id, individual_id) %>%
      dplyr::mutate(
        timestamp = as.POSIXct(timestamp), # make times POSIXct for compatibility with OGR
        year = as.numeric(strftime(timestamp, '%Y')),
        month = as.numeric(strftime(timestamp, '%m')), # add numeric month field
        season = season,
        sex = ifelse(sex == ' ' | is.na(sex), 'u', sex)
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
      dplyr::group_by(local_identifier, ring_id, taxon_canonical_name, sex,
                      animal_life_stage, animal_reproductive_condition, number_of_events,
                      study_site, deploy_on_longitude, deploy_on_latitude,
                      deployment_id, tag_id, individual_id) %>%
      dplyr::summarise(
        start_time = min(timestamp),
        end_time = max(timestamp),
        number_locations = dplyr::n(),
        do_union = FALSE,
        .groups = 'drop') %>%
      sf::st_cast("LINESTRING")

    out_data <- rbind(out_data, gps_data)
    out_points <- rbind(out_points, gps_points)
    out_tracks <- rbind(out_tracks, gps_tracks)
  }

  # Make a list of objects to return
  out <- list(
    data = out_data,
    points = out_points,
    tracks = out_tracks
  )

  # Create dsn for writeOGR
  dsn <- ifelse(is.null(export_location), getwd(), export_location)

  # Export requested files
  if (export_csv == T) write.csv(out_data, paste0(export_location,'/',export_name, "_data.csv"), row.names = F)
  if (export_points == T) rgdal::writeOGR(as(out_points, 'Spatial'), dsn = dsn, layer = paste0(export_name, "_points"), driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  if (export_tracks == T) rgdal::writeOGR(as(out_tracks, 'Spatial'), dsn = dsn, layer = paste0(export_name, "_tracks"), driver = 'ESRI Shapefile', overwrite_layer = TRUE)

  # return list with [1] raw dataframe, [2] sf points, [3] sf lines
  out

}

# -----

#' Prepare raw Ecotone data for Movebank upload.
#'
#' This function modifies raw Ecotone GPS data to remove
#' any records without lat/long values, inserts a "behavior"
#' column to indicate when a tagged bird is at the colony,
#' adds a timestamp column ("Date_2") if not already there,
#' and removes any duplicate detections. The function also
#' inserts lat/long coordinates for the colony location for
#' periods when the bird is at the colony.
#'
#'
#'@param data Input Ecotone data to be modified.
#'@param colony_lon Longitude of home colony of tagged bird.
#'@param colony_lat Latitude of home colony of tagged bird.
#'@param tz Timezone of GPS timestamps. Default "UTC".
#
#'@export

prep_ecotone <- function(data,
                         colony_lon,
                         colony_lat,
                         tz = "UTC") {
  data$Latitude[data$In.range == 1] <- colony_lat
  data$Longitude[data$In.range == 1] <- colony_lon
  data$Behaviour <- ifelse(data$In.range == 1, 'At colony', NA)
  data <- subset(data, !is.na(data$Latitude))

  if(any(grepl("Date_2", names(data))) == FALSE){
    data$Date_2 <- as.POSIXct(paste0(data$Year, "-",
                                     data$Month, "-",
                                     data$Day, " ",
                                     data$Hour, ":",
                                     data$Minute, ":",
                                     data$Second),
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = tz)
  }

  data <- data[duplicated(data[,c('Logger.ID','Date_2')]) == F,]
  data
}
