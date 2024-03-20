contour(elev,
        frame.plot = F,
        col = terrain.colors(n=10),
        xlab = "Latitude",
        ylab = "Longitude")
# Plot cities
points(
  x = cities$x,
  y = cities$y,
  col = "black",
  pch = 19,
  xlim = c(23, 24.5),
  ylim = c(37.5, 38.5)
)

# Define x-points for iso
h_2nd_x <- c(
  ((cities[6,1] + cities[7,1])/2)+.05, # Tanagra v Oropos
  (cities[6,1] + cities[2,1])/2, # Tanagra v Eleusis
  (cities[5,1] + cities[2,1])/2, # Aigosthena v Eleusis
  (cities[4,1] + cities[2,1])/2, # Pagai v Eleusis
  (cities[3,1] + cities[2,1])/2, # Megara v Eleusis
  ((cities[3,1] + cities[1,1])/2)-.05  # Megara v Athens
)

# Define y-points for iso
h_2nd_y <- c(
  ((cities[6,2] + cities[7,2])/2)+.05, # Tanagra v Oropos
  (cities[6,2] + cities[2,2])/2, # Tanagra v Eleusis
  (cities[5,2] + cities[1,2])/2, # Aigosthena v Eleusis
  (cities[4,2] + cities[1,2])/2, # Pagai v Eleusis
  (cities[3,2] + cities[1,2])/2, # Megara v Eleusis
  ((cities[3,2] + cities[1,2])/2)-.05  # Megara v Athens
)

# Draw iso line
lines(x = h_2nd_x, y = h_2nd_y, col = "blue", lwd = 2, lty = 3)

# Define x-points for iso
h_2nd_x_2 <- c(
  ((cities[5,1] + cities[7,1])/2), # Aigosthena v Oropos
  (cities[5,1] + cities[7,1])/2, # Aigosthena v Oropos
  (cities[5,1] + cities[2,1])/2, # Aigosthena v Eleusis
  (cities[3,1] + cities[2,1])/2, # Megara v Eleusis
  ((cities[3,1] + cities[2,1])/2)-.05  # Control
)

# Define y-points for iso
h_2nd_y_2 <- c(
  ((cities[5,2] + cities[7,2])/2)+.05, # Aigosthena v Oropos
  (cities[5,2] + cities[7,2])/2, # Aigosthena v Oropos
  (cities[5,2] + cities[2,2])/2, # Aigosthena v Eleusis
  (cities[3,2] + cities[2,2])/2, # Megara v Eleusis
  ((cities[3,2] + cities[2,2])/2)-.05  # Control
)

# Draw iso line
lines(x = h_2nd_x_2, y = h_2nd_y_2, col = "blue", lwd = 2)

# Label cities
# Moved to end so that iso lines wont overlap text
text(x = cities[-c(5,6),]$x,
     y = cities[-c(5,6),]$y,
     labels = cities[-c(5,6),]$names,
     col = "black",
     pos = 1)
text(x = cities[-c(1,2,3,4,7),]$x-.05,
     y = cities[-c(1,2,3,4,7),]$y,
     labels = cities[-c(1,2,3,4,7),]$names,
     col = "black",
     pos = 3
)