# Inverse log
# Antilog function from: http://r.789695.n4.nabble.com/Searching-for-antilog-function-td4721348.html
antilog <- function(lx, base) {
  lbx <- lx / log(exp(1), base = base)
  result <- exp(lbx)
  result
} 
