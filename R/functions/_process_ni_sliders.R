#### PROCESS NATURAL INFRASTRUCTURE CUSTOM SLIDERS #############################

process_ni_sliders <- function(x, y, z) {
  x1 <- ni_helper(x)
  y1 <- ni_helper(y)
  z1 <- ni_helper(z)
  
  if (x1 == y1 && x1 == z1) return(c(1, 1, 1))
  if (x1 == 0 && y1 == 0)   return(c(0, 0, 1))
  if (x1 == 0 && z1 == 0)   return(c(0, 1, 0))
  if (y1 == 0 && z1 == 0)   return(c(1, 0, 0))
  if (x1 == 0 && y1 == z1)  return(c(0, 1, 1))
  if (y1 == 0 && x1 == z1)  return(c(1, 0, 1))
  if (z1 == 0 && x1 == y1)  return(c(1, 1, 0))
  if (x1 == 0 && y1 == 0.5 && z1 == 1) return(c(0, 1, 2))
  if (x1 == 0 && y1 == 1 && z1 == 0.5) return(c(0, 2, 1))
  if (x1 == 0.5 && y1 == 0 && z1 == 1) return(c(1, 0, 2))
  if (x1 == 0.5 && y1 == 1 && z1 == 0) return(c(1, 2, 0))
  if (x1 == 0.5 && y1 == 0.5 && z1 == 1) return(c(1, 1, 2))
  if (x1 == 0.5 && y1 == 1 && z1 == 0.5) return(c(1, 2, 1))
  if (x1 == 1 && y1 == 0.5 && z1 == 0.5) return(c(2, 1, 1))
  if (x1 == 1 && y1 == 0 && z1 == 0.5) return(c(2, 0, 1))
  if (x1 == 1 && y1 == 0.5 && z1 == 0) return(c(2, 1, 0))
  if (x1 == 0.5 && y1 == 1 && z1 == 1) return(c(1, 2, 2))
  if (x1 == 1 && y1 == 0.5 && z1 == 1) return(c(2, 1, 2))
  if (x1 == 1 && y1 == 1 && z1 == 0.5) return(c(2, 2, 1))
  return(c(x1, y1, z1))
  
}

ni_helper <- function(x) {
  if (x == "Not important" || x == "Pas important") return(0)
  if (x == "Somewhat important" || x == "Peu important") return(0.5)
  if (x == "Important" || x == "Important") return(1)
  if (x == "Very important" || x == "Très important") return(1.5)
  if (x == "Extremely important" || x == "Extrêmement important") return(2)
}