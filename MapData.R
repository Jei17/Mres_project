lon <- c(-2.23, -0.98, -0.25, -1.6, 0.46, -1.76, -2.33, -2.1, -0.03, -2.48, -1.06, -0.85, -1.1, -0.16, 0.63, 0)
lat <- c(4.86, 5.93, 6.08, 6.71, 6.6, 4.88, 7.33, 7.75, 7.81, 9.03, 5.2, 8.5, 10.9, 5.6, 5.78, 5.61)
station <- c("AXM", "ODA", "KDA","KSI", "HO", "TDI", "SUN", "WEN", "KRA", "BOL", "SAL", "TLE", "NAV1", "ACC", "ADA", "TEM")
pos.obs <- c(4282, 4180, 3756, 3586, 3409, 3312, 3187, 3188, 2991, 2688, 2679, 2599, 2184, 2130, 2037, 1840)/10958
pos.obsn <- c(3210, 3181, 2851, 2733, 2674, 2215, 2504, 2633, 2348, 2182, 1916, 2121, 1877, 1509, 1521, 1317)/10958
annual.mean <- c(1847, 1407, 1293, 1347, 1276, 1079, 1191, 1249, 1366, 1101, 931, 1017, 988, 747, 790, 659)
annual.meann <- c(1855, 1391,1278, 1333,1264, 1062, 1180, 1240, 1255, 1092, 919, 1009, 982, 736, 782, 651)
day.mean <- c(13.13, 10.10, 10.33,11.27, 11.23, 9.77, 11.22, 11.75, 13.70, 12.29, 10.43, 11.74, 13.57, 10.52, 11.63, 10.75)
day.meann <- c(17.34, 13.12, 13.45, 14.64, 14.18, 14.39, 14.14, 14.12, 17.31, 15.01, 14.39, 14.28, 15.70, 14.64, 15.42, 14.84)
rainmode <- c("Semi-bi", "Bi", "Bi", "Bi", "Bi", "Semi-bi", "Bi", "Bi", "Uni", "Uni", "Semi-bi", "Uni", "Uni", "Semi-bi", "Semi-bi", "Semi-bi")

map.data <-data.frame(station, lon, lat, pos.obs, annual.mean, day.mean, rainmode)

sbbox <- make_bbox(lon=map.data$lon, lat=map.data$lat, f=0.45)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")

ggmap(sq_map) + 
  geom_point(data = map.data, mapping = aes(x = lon, y = lat, color = rainmode), size = 9) + geom_text(data = map.data, aes(x = lon, y = lat, label = station, angle = 320, hjust = 0))

scale_colour_gradient(low = 'brown1', high = "blue") + 