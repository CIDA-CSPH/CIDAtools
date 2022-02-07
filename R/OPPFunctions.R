#' Download OPP tracking data from Movebank
#'
#' @description This function downloads OPP tracking data from Movebank and returns a
#' dataframe with combined tracking and reference data for all deployments.
#'
#' @param study List of Movebank project ids.
#' @param login Stored Movebank login credentials if provided, otherwise function
#'will prompt users to enter credentials.
#' @param start_month Earliest month (1-12) to include in output.
#' @param end_month Latest month (1-12) to include in output.
#' @param season Vector describing the season data can be applied to, eg. 'Breeding (Jun-Jul)'
#'
#' @details The function can be passed a list of movebank study IDs and will append
#'data from all studies.
#'
#' @examples
#'# download ANMU project data from two studies, for May only
#'my_data <- opp_download_data(study = c(1895716931, 1897273090),
#'                             login = NULL, start_month = 5, end_month = 5,
#'                             season = 'Incubation')
#'
#'@export

opp_download_data <- function(study,
                              login = NULL,
                              start_month = NULL,
                              end_month = NULL,
                              season = NULL
) {

  # Ask for movebank credentials if not provided
  if (is.null(login)) login <- move::movebankLogin()
  if (is.null(season)) season <- NA

  out_data <- data.frame()

  for (ss in study) {

    # Download data from movebank
    mb_data <- suppressMessages(move::getMovebankData(study = ss, login = login,
                                     removeDuplicatedTimestamps = TRUE,
                                     includeExtraSensors = FALSE,
                                     deploymentAsIndividuals = TRUE,
                                     includeOutliers = FALSE))

    # Extract the minimal fields required
    loc_data <- as(mb_data, 'data.frame') %>%
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
    if (is.null(start_month) == FALSE) loc_data <- subset(loc_data, loc_data$month >= start_month)
    if (is.null(end_month) == FALSE) loc_data <- subset(loc_data, loc_data$month <= end_month)

    if (mb_data@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs") {
      warning(paste('CRS for', ss, 'is not longlat. be careful if joining data from multiple studies in different coordinate systems'), call. = FALSE)
    }

    out_data <- rbind(out_data, loc_data)

  }

  out_data

}

# -----

#' Converts movebank data to format required for track2KBA
#'
#' @description Takes tracking data downloaded from Movebank using OPPTools::opp_download_data
#' and converts it to the format needed for track2KBA
#'
#' @param data A dataframe obtained using OPPTools::opp_download_data
#'
#' @details This extracts location, timestamp and deployment data from movebank data
#' and returns a list of dataframes that can be passed to functions in track2KBA.
#' This is useful if the user wants to filter the data downloaded from movebank based on
#' fields contained within the reference data (e.g. sex, animal_reproductive_condition)
#'
#' @returns Returns a list object of length two, containing tracking data
#' (accessed using: dataset$data) and study site location information
#' (accessed using: dataset$site).
#' @examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#' @export

opp2KBA <- function(data
) {
  locs <- data %>%
    dplyr::select(deployment_id, timestamp, location_lat, location_long) %>%
    dplyr::rename(ID = deployment_id,
                  DateTime = timestamp,
                  Latitude = location_lat,
                  Longitude = location_long)

  sites <- data %>%
    dplyr::select(deployment_id, deploy_on_latitude, deploy_on_longitude) %>%
    dplyr::rename(ID = deployment_id,
                  Latitude = deploy_on_latitude,
                  Longitude = deploy_on_longitude) %>%
    unique()
  row.names(sites) <- 1:nrow(sites)

  out <- list(data = locs, site = sites)

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

#' Define a custom equal-area CRS centered on your study site
#'
#' @description This function takes a Movebank data object and
#' creates an equal-area projection ceneterd on the Movebank
#' study site. In the case of central-place foraging seabirds,
#' this effectively equates to a CRS centered on the seabird
#' colony. The function returns a proj4 string.
#'
#' @param data Movebank data as returned by opp_download_data.
#' @param interactive Logical (T/F), do you want to explore tracks with an interative map? Default FALSE.
#'
#' @examples
#' data(murres)
#' colCRS(murres)
#'
#' @export

colCRS <- function(
  data # Movebank data object
  ) {
  return(paste0(
    '+proj=laea',
    ' +lat_0=', mean(data$site$Latitude),
    ' +lon_0=', mean(data$site$Longitude)
  ))
}

# -----

#' Plot raw tracks from Movebank download
#'
#' @description Quickly plot Movebank data downloaded
#' using opp_download_data to visualize tracks.
#'
#' @param data Movebank data as returned by opp_download_data.
#'
#' @examples
#' data(murres)
#' opp_map(murres)

opp_map <- function(data, # Data as downloaded from Movebank
                    interactive = FALSE) {

  # Check if maps installed
  # maps is used to add simple land features to map
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("Packages \"maps\"is needed. Please install it.",
         call. = FALSE)
  }
  # Check if mapview is installed
  # mapview is used for interactive mode
  if (interactive == TRUE){
    if (!requireNamespace("mapview", quietly = TRUE)) {
      stop("Packages \"mapview\"is needed. Please install it.",
           call. = FALSE)
    }
  }
  # Make ID factor so it plots w appropriate color scheme
  data$data$ID <- as.factor(data$data$ID)

  # Convert Movebank data df to sf object
  raw_tracks <- sf::st_as_sf(data$data,
                             coords = c("Longitude", "Latitude"),
                             crs = '+proj=longlat')

  # Extract bounds
  coordsets <- sf::st_bbox(raw_tracks)

  trackplot <- ggplot2::ggplot(raw_tracks) +
    ggplot2::geom_sf(data = raw_tracks,
                     ggplot2::aes(col = ID),
                     fill = NA) +
    ggplot2::coord_sf(xlim = c(coordsets$xmin, coordsets$xmax),
                      ylim = c(coordsets$ymin, coordsets$ymax),
                      expand = TRUE) +
    ggplot2::borders("world", colour = "black", fill = NA) +
    ggplot2::geom_point(data = data$site,
                        ggplot2::aes(x = .data$Longitude,
                                     y = .data$Latitude),
                        fill = "dark orange",
                        color = "black",
                        pch = 21,
                        size = 2.5) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "black"),
                   legend.position = "none",
                   panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA,
                                                        size = 1)) +
    ggplot2::ylab("Latitude") +
    ggplot2::xlab("Longitude")

  if(interactive == FALSE){
    print(trackplot)
  } else {
    mapview::mapview(raw_tracks, zcol = "ID")
  }
}

# -----

#' Identify foraging trips in tracking data

#' @description Uses criteria related to distance from colony, trip duration, and size of gaps
#' in tracking data to identify and classify trips from a nest or colony. It is
#' a wrapper for track2KBA::tripSplit that applies custom criteria for classifying
#' trips.
#'
#' @param data Tracking data formated using track2KBA or opp2KBA
#' @param innerBuff Minimum distance (km) from the colony to be in a trip.
#' Used to label trips as 'Non-trip'. Defaults to 5
#' @param returnBuff Outer distance (km) to capture trips that start and end
#' away from the colony. Used to label trips as 'Incomplete'. Defaults to 20.
#' @param duration Minimum trip duration (hrs)
#' @param gapLimit Maximum time between points to be considered too large to be
#' a contiguous tracking event. Can be used to ensure that deployments on the
#' same animal in different years do not get combined into extra long trips.
#' Defaults to 100 days.
#' @param missingLocs Proportion (0-1) of trip duration that a gap in consecutive
#' locations should not exceed. Used to label trips as 'Gappy'. Defaults to 0.2.
#' @param showPlots Logical (T/F), should plots showing trip classification by generated?
#'
#' @examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#'my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 5, returnBuff = 20,
#'                          duration  = 2, gapLimit = 100, missingLocs = 0.2,
#'                          showPlots = TRUE)
#' @export


opp_get_trips <- function(data,
                          innerBuff  = 5, # (km) minimum distance from the colony to be in a trip
                          returnBuff = 20, # (km) outer buffer to capture incomplete return trips
                          duration  = 2, # (hrs) minimum trip duration
                          gapLimit = 100,
                          missingLocs = 0.2, # Percentage of trip duration that a gap in consecutive locations should not exceed
                          showPlots = TRUE
) {

  trips <- track2KBA::tripSplit(
    dataGroup  = data$data, # data formatted using formatFields()
    colony     = data$site, # data on colony location - can be extracted from movebank data using move2KBA()
    innerBuff  = innerBuff,      # (km) minimum distance from the colony to be in a trip
    returnBuff = returnBuff,     # (km) outer buffer to capture incomplete return trips
    duration   = duration,      # (hrs) minimum trip duration
    gapLimit = gapLimit, # (days) time between points to be considered too large to be a contiguous tracking event
    rmNonTrip  = F,    # T/F removes times when not in trips
    nests = ifelse(nrow(data$site) > 1, TRUE, FALSE)
  )

  trips <- trips[order(trips$ID, trips$DateTime),]
  trips$tripID[trips$ColDist <= innerBuff * 1000] <- -1

  trips_type <- trips@data %>%
    dplyr::group_by(ID, tripID) %>%
    dplyr::mutate(
      dt = as.numeric(difftime(DateTime, dplyr::lag(DateTime), units = 'hour')),
      dt = ifelse(is.na(dt), 0, dt),
      n = dplyr::n(),
      tripTime = as.numeric(difftime(max(DateTime), min(DateTime), units = 'hour')),
      Type = NA,
      Type = ifelse(ColDist[1] > returnBuff * 1000 | ColDist[dplyr::n()] > returnBuff * 1000, 'Incomplete', Type),
      Type = ifelse(max(dt, na.rm = T) > tripTime * missingLocs, 'Gappy', Type),
      Type = ifelse(tripID == -1, 'Non-trip', Type),
      Type = ifelse(n < 3, 'Non-trip', Type),
      Type = ifelse(is.na(Type), 'Complete', Type)
    )

  trips$Type <- trips_type$Type

  bb <- unique(trips_type$ID)
  idx <- seq(1,length(bb), by = 4)
  dummy <- data.frame(Type = c('Non-trip', 'Incomplete', 'Gappy', 'Complete'))

  if (showPlots == TRUE) {
    for (i in idx) {

      intdat <- trips_type[trips_type$ID %in% bb[i:(i+3)],]

      p <- ggplot2::ggplot(intdat) +
        ggplot2::geom_line(ggplot2::aes(x = DateTime, y = ColDist/1000), linetype = 3) +
        ggplot2::geom_point(size = 0.9, ggplot2::aes(x = DateTime, y = ColDist/1000, col = Type))  +
        ggplot2::geom_hline(yintercept = c(innerBuff, returnBuff), linetype = 2, col = 'black') +
        ggplot2::facet_wrap(facets = . ~ ID, nrow = 3, scales = 'free') +
        ggplot2::labs(x = 'Time', y = 'Distance from colony (km)', col = 'Trip type') +
        ggplot2::geom_blank(data = dummy, ggplot2::aes(col = Type)) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::theme_light() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 8)
        )

      print(p)
      readline('Next plot [enter]')

    }
  }
  return(trips)
}

# -----

#' Interpolate GPS locations at a set time interval using a continuous time correlated
#' random walk (ctcrw) model
#'
#' @description This function is a wrapper for momentuHMM::crawlWrap(), which
#' uses the crawl package to fit ctcrw model to GPS tracks at a user-defined
#' time interval. The function is currently designed to handle GPS data from
#' central place foraging birds. It takes tracking data, where trips have been
#' identified and classified using OPPTools::opp_get_trips(). The function
#' returns a list with four objects: (1) original tracking data (as SPDF),
#' (2) colony location (as SPDF), (3) interpolated locations (as SPDF), and (4)
#' a list of CRAWL fits for each trip. All spatial objects are in the same custom
#' Lambert equal area projection centered on the colony.
#'
#'
#'@param data Trip data ouptut from OPPTools::opp_get_trips().
#'@param type List indicating the types of trips to include in interpolation.
#'Possible values are: 'Complete', 'Incomplete', 'Gappy', and 'Non-trip'. Default is 'Complete'.
#'@param timestep string indicating time step for track interpolation, eg. '10 min', '1 hour', '1 day'
#'@param showPlots TRUE/FALSE should plots of interpolated tracks against original data be produced
#'@param theta starting values for ctcrw parameter optimization, see ?crawl::crwMLE for details
#'
#'@examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#'my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 5, returnBuff = 20,
#'                          duration  = 2, gapLimit = 100, missingLocs = 0.2,
#'                          showPlots = TRUE)
#'
#'my_interp <- ctcrw_interpolation(data = my_trips, site = my_track2kba$site,
#'                                 type = c('Complete', 'Incomplete', 'Gappy'),
#'                                 timestep = '10 min', showPlots = T,
#'                                 duration   = 2,timestep = '60 min',
#'                                 showPlots = T)
#'@export
#'
#'
ctcrw_interpolation <- function(data,
                                site,
                                type,
                                timestep = '20 min',
                                showPlots = T,
                                theta = c(8,2)
) {

  # Generate custom laea projection centered on colony
  myCRS <- paste0(
    '+proj=laea',
    ' +lat_0=', mean(site$Latitude),
    ' +lon_0=', mean(site$Latitude)
  )

  # Create SpatialPoints object for colony
  site_loc <- sp::SpatialPointsDataFrame(site[,c('Longitude','Latitude')], data = site,
                                         proj4string = sp::CRS('+proj=longlat'))
  site_loc <- sp::spTransform(site_loc, myCRS)

  # Create SpatialPoints object of raw tracking data
  orig_loc <- sp::spTransform(data, myCRS)
  # re-calculate distance from colony for all original locations
  if (nrow(site_loc) == 1)  orig_loc$ColDist <- sp::spDistsN1(orig_loc, site_loc)
  if (nrow(site_loc) > 1) {
    orig_loc$ColDist <- NA
    for (id in site_loc$ID) {
      orig_loc$ColDist[orig_loc$ID == id] <- sp::spDistsN1(orig_loc[orig_loc$ID == id,], site_loc[site_loc$ID == id,])
    }
  }

  interp_loc <- subset(orig_loc, orig_loc$Type %in% type)
  interp_loc$time <- interp_loc$DateTime
  interp_loc$Bird <- interp_loc$ID
  interp_loc$ID <- interp_loc$tripID
  interp_loc <- interp_loc[,c('Bird', 'ID', 'time', 'ColDist')]

  crwOut <- momentuHMM::crawlWrap(obsData = interp_loc,
                                  timeStep = timestep,
                                  theta = theta,
                                  fixPar = c(NA,NA),
                                  method = 'Nelder-Mead')

  pred <- data.frame(crwOut$crwPredict) %>%
    dplyr::filter(locType == 'p') %>%
    dplyr::select(Bird, ID, time, ColDist, mu.x, mu.y, se.mu.x, se.mu.y) %>%
    tidyr::separate('ID', c('Bird', NA), sep = '_', remove = FALSE) %>%
    dplyr::rename(tripID = ID, ID = Bird, DateTime = time)

  pred <- sp::SpatialPointsDataFrame(coords = pred[,c('mu.x', 'mu.y')],
                                     data = pred[,c('ID', 'tripID', 'DateTime', 'ColDist',
                                                    'mu.x', 'mu.y',
                                                    'se.mu.x', 'se.mu.y')],
                                     proj4string = sp::CRS(myCRS)
  )

  pred_longlat <- sp::spTransform(pred, sp::CRS('+proj=longlat'))
  pred$Longitude <- sp::coordinates(pred_longlat)[,1]
  pred$Latitude <- sp::coordinates(pred_longlat)[,2]

  # re-calculate distance from colony for all interpolated locations
  if (nrow(site_loc) == 1)  pred$ColDist <- sp::spDistsN1(pred, site_loc)
  if (nrow(site_loc) > 1) {
    pred$ColDist <- NA
    for (i in 1:nrow(site_loc)) {
      pred$ColDist[pred$ID == site_loc$ID[i]] <- sp::spDistsN1(pred[pred$ID == site_loc$ID[i],], site_loc[site_loc$ID == site_loc$ID[i],])
    }
  }

  out <- list(
    data = orig_loc,
    site = site_loc,
    interp = pred,
    crawl_fit = crwOut$crwFits
  )

  if (showPlots == T) {
    bb <- unique(pred$ID)
    idx <- seq(1,length(bb), by = 4)
    pal <- hcl.colors(4, "viridis")

    for (i in idx) {

      intdat <- pred[pred$ID %in% bb[i:(i+3)],]@data
      obsdat <- orig_loc[orig_loc$ID %in% bb[i:(i+3)],]@data

      p <- ggplot2::ggplot(obsdat, ggplot2::aes(x = DateTime, y = ColDist/1000)) +
        ggplot2::geom_line(linetype = 3, col = pal[1]) +
        ggplot2::geom_point(size = 1.5, col = pal[1])  +
        ggplot2::geom_line(data = intdat, ggplot2::aes(x = DateTime, y = ColDist/1000, group = tripID), linetype = 3, col = pal[3]) +
        ggplot2::geom_point(data = intdat, ggplot2::aes(x = DateTime, y = ColDist/1000), size = 0.9, col = pal[3], shape = 1) +
        ggplot2::facet_wrap(facets = . ~ ID, nrow = 2, scales = 'free') +
        ggplot2::labs(x = 'Time', y = 'Distance from colony (km)') +
        ggplot2::scale_x_datetime(date_labels = '%b-%d') +
        ggplot2::theme_light() +
        ggplot2::theme(
          text = ggplot2::element_text(size = 8)
        )

      print(p)
      readline('Next plot [enter]')

    }
  }

  return(out)
}
