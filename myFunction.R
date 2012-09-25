# construct a matrix with start/end character positions
my.function <- function(foo, bar){	
	start <- head(c(1, bar + 1), -1)	# delete last one
  sel <- cbind(start = start, end = bar)
	strings <- apply(sel, 1, function(x) substr(foo, x[1], x[2]))
  
	paste(strings, collapse = ',')
	}