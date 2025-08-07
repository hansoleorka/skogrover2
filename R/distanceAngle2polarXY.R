#' Convert Distance and Angle to Polar Coordinates with Magnetic Declination Correction
#'
#' This function calculates polar coordinates (x, y) from distance and angle, 
#' with a correction for magnetic declination. This correction allows for conversion 
#' from compass readings to map (true) north. The angle is given in degrees or gradiance.
#'
#' @param Distance Numeric. The distance (in meter) from the origin.
#' @param Angle Numeric. The angle in degrees or gradians from the center point.
#' @param magnetic_declination Numeric. The magnetic declination (in degrees) to adjust the compass angle to true north. See eg. https://www.magnetic-declination.com/ 
#' @param deg Numeric. The unit measure of the input angle; typically 360 for degrees or 400 for gradians.
#'
#' @details
#' To be added 
#' @examples
#' # Example usage
#' distanceAngle2polarXY(Distance = 7.5, Angle = 380, magnetic_declination = 4.8333, deg = 400)
#' @export
distanceAngle2polarXY <- function(Distance,Angle,magnetic_declination,deg){
  # Magnetic declination in Vaaler ~ 4 deg 50' = 4.8333 deg
  eranto <- magnetic_declination * (2 * pi / 360) # magnetic declination (Finnish "eranto") in rad
  
  # phi (Greek letter to denote angle) = corrected angle in rad (all plots are in 400-units/full circle)
  phi <- (Angle * 2 * pi / deg + eranto) # Direction in rads from true north (map north) clockwise
  
  xcoo_md <- Distance * sin(phi)
  ycoo_md <- Distance * cos(phi)
  out <- data.frame(xcoo_md,ycoo_md)
  return(out)
}


