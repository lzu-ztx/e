#' @title Calculate the solar elevation angles.
#' @description  Calculate the solar elevation angles from the input data.
#' @details Calculate the solar elevation angles of the location based on the input date and dimension.
#' @param date datetime-object or String vector
#' @param lat Numeric vector
#' @return Numeric vector of solar elevation angles
#' @import lubridate
#' @export
#' @examples
#' SolarAltitudeAngle("2021/4/15 14:00:00",36.05)
#' SolarAltitudeAngle("2021/6/22 12:00:00",40)
#' \dontrun{
#' SolarAltitudeAngle("2021/6/22",40)
#' }

SolarAltitudeAngle <- function(date, lat){

  lat = lat * pi / 180
  hour = hour(date)
  n = yday(date)
  t = 15 * (hour - 12) * pi / 180  # 太阳时角 弧度制
  delta = 23.45 * sin((2 * pi * (284 + n)) / 365) * pi / 180  # 太阳赤纬角  弧度制

  H = asin(sin(lat) * sin(delta) + cos(lat) * cos(delta) * cos(t)) * 180 / pi

  return(H)

}
