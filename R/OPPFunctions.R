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

# -----

#' Prepare Pathtrack data for Movebank upload.
#'
#' This simple function processes Pathtrack data that has
#' been exported from Pathtrack Host software for Movebank
#' upload. It removes any records in Pathtrack data that
#' have `null` latitude or longitude values.
#' Unlike `prep_ecotone()`, this function makes no assumptions
#' on the bird's location when latitude/longitude are null.
#'
#'
#'@param data Input Pathtrack data to be modified.
#
#'@export

prep_pathtrack <- function(data) {
  data <- data[!is.na(data$Lat > 0) & !is.na(data$Long > 0),]
  data
  }

# -----

#' Interpolate GPS locations at a set time interval using a continuous time correlated
#' random walk (ctcrw) model
#'
#' This function is a wrapper for momentuHMM::crawlWrap(), which uses the crawl
#' package to fit ctcrw model to GPS tracks at a user-defined time interval. The
#' function is currently designed to handle GPS data from centra place foraging birds.
#' It takes tracking data downloaded from Movebank using track2KBA::move2KBA().
#' Initial data processing identifies foraging trips away from the colony using
#' track2KBA::tripSplit(). The function returns a list with three SpatialPoints objects:
#' (1) original tracking data, (2) colony location, and (3) interpolated locations. All
#' spatial objects are in the same custom Lambert equal area projection centered on the
#' colony.
#'
#'
#'@param data Tracking data downloaded directly from movebank using track2KBA::move2kba.
#'@param innerBuff  minimum distance (km)  from the colony to be considered in a trip,
#'value passed to track2KBA::tripSplit()
#'@param returnBuff outer buffer (km)  to capture incomplete return trips to be considered in a trip,
#'value passed to track2KBA::tripSplit()
#'@param duration minimum time (hrs) away from colony  to be considered in a trip,
#'value passed to track2KBA::tripSplit()
#'@param timestep string indicating time step for track interpolation, eg. '10 min', '1 hour', '1 day'
#'@param showPlots TRUE/FALSE should plots of interpolated tracks against original data be produced
#'@param theta starting values for ctcrw parameter optimization, see ?crawl::crwMLE for details
#'
#'@examples
#'dataset <- track2KBA::move2KBA(movebankID = 1895716931,
#'                               user = rstudioapi::askForPassword(prompt = 'Movebank username:'),
#'                               password = rstudioapi::askForPassword(prompt = 'Movebank password:'),
#'                               filename = NULL)
#'
#'interp_data <- ctcrw_interpolation(data = dataset, innerBuff  = 5, returnBuff = 10,
#'                            duration   = 2,timestep = '60 min',show_plots = T)
#'@export
#'

ctcrw_interpolation <- function(data, # list of 2 data frames returned by track2KBA::move2kba()
                         innerBuff,      # (km) minimum distance from the colony to be in a trip
                         returnBuff,     # (km) outer buffer to capture incomplete return trips
                         duration,      # (hrs) minimum trip duration
                         timestep,
                         showPlots = F,
                         theta = c(8,2) # Theta parameters passed to crawl
) {

  tracks <- data$data
  colony <- data$site

  # Generate custom laea projection centered on colony
  myCRS <- paste0(
    '+proj=laea',
    ' +lat_0=', colony$Latitude,
    ' +lon_0=', colony$Longitude
  )

  # Create SpatialPoints object for colony
  col_loc <- sp::SpatialPoints(colony, proj4string = sp::CRS('+proj=longlat'))
  col_loc <- sp::spTransform(col_loc, myCRS)

  # Create SpatialPoints object of raw tracking data
  orig_loc <- sp::SpatialPointsDataFrame(coords = tracks[,c('Longitude', 'Latitude')],
                                         data = tracks, proj4string = sp::CRS('+proj=longlat'))
  orig_loc <- sp::spTransform(orig_loc, myCRS)
  orig_loc$ColDist <- sp::spDistsN1(orig_loc, col_loc)

  trips <- track2KBA::tripSplit(
    dataGroup  = tracks, # data formatted using formatFields()
    colony     = colony, # data on colony location - can be extracted from movebank data using move2KBA()
    innerBuff  = innerBuff,      # (km) minimum distance from the colony to be in a trip
    returnBuff = returnBuff,     # (km) outer buffer to capture incomplete return trips
    duration   = duration,      # (hrs) minimum trip duration
    rmNonTrip  = T    # T/F removes times when not in trips
  )

  trips <- subset(trips, trips$ColDist > ifelse(innerBuff < 1, innerBuff * 1000, 1000))
  trips <- sp::spTransform(trips, sp::CRS(myCRS))
  trips$time <- trips$DateTime
  trips$Bird <- trips$ID
  trips$ID <- trips$tripID
  trips <- trips[,c('Bird', 'ID', 'time', 'ColDist')]

  crwOut <- momentuHMM::crawlWrap(obsData=trips, timeStep=timestep,
                      theta=theta, fixPar=c(NA,NA),
                      method = 'Nelder-Mead')

  pred <- data.frame(crwOut$crwPredict) %>%
    dplyr::filter(locType == 'p') %>%
    dplyr::select(Bird, ID, time, ColDist, mu.x, mu.y, se.mu.x, se.mu.y) %>%
    dplyr::mutate(Bird = substr(ID, 1, 10)) %>%
    dplyr::rename(tripID = ID, ID = Bird, DateTime = time)

  pred <- sp::SpatialPointsDataFrame(coords = pred[,c('mu.x', 'mu.y')],
                                 data = pred[,c('ID', 'tripID', 'DateTime', 'ColDist',
                                                'se.mu.x', 'se.mu.y')],
                                 proj4string = sp::CRS(myCRS)
  )

  # re-calculate distance from colony for all interpolated locations
  pred$ColDist <- sp::spDistsN1(pred, col_loc)

  out <- list(
    data = orig_loc,
    site = col_loc,
    interp = pred,
    crawl_fit = crwOut$crwFits
  )

  if (showPlots == T) {
    bb <- unique(pred$ID)
    idx <- seq(1,length(bb), by = 4)

    for (i in idx) {

      intdat <- pred[pred$ID %in% bb[i:(i+3)],]@data
      obsdat <- orig_loc[orig_loc$ID %in% bb[i:(i+3)],]@data

      p <- ggplot(obsdat, aes(x = DateTime, y = ColDist/1000)) +
        geom_line(linetype = 3) +
        geom_point(size = 0.9)  +
        geom_line(data = intdat, aes(x = DateTime, y = ColDist/1000, group = tripID), linetype = 3, col = 'red') +
        geom_point(data = intdat, aes(x = DateTime, y = ColDist/1000), size = 0.9, col = 'red') +
        geom_hline(yintercept = innerBuff, linetype = 2, col = 'blue') +
        facet_wrap(facets = . ~ ID, nrow = 2, scales = 'free') +
        labs(x = 'Time', y = 'Distance from colony (km)') +
        scale_x_datetime(date_labels = '%b-%d') +
        theme(
          text = element_text(size = 8)
        )

      print(p)
      readline('Next plot [enter]')

    }
  }

  return(out)
}


#'
