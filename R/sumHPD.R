sumHPD <-
function(HPD, plot.list = FALSE, tex = FALSE){
	
# Function to summarize objects of S3 class 'HivePlotData'
# Part of HiveR package
# Bryan Hanson, DePauw Univ, Oct 2011


	chkHPD(HPD) # verify it's legit
	
	cat("\n\t", HPD$desc, "\n")
	cat("\tThis hive plot data set contains ",
		length(HPD$nodes$id), " nodes on ",
		length(unique(HPD$nodes$axis)), " axes and ",
		length(HPD$edges$id1), " edges.\n", sep = "")
	cat("\tIt is a  ", HPD$type, " data set.\n\n", sep = "")

	if (plot.list) {
		# Create a list of edges to be drawn in a helpful format
		n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()
		for (n in 1:(length(HPD$edges$id1))) {
			pat1 <- HPD$edges$id1[n]
			pat2 <- HPD$edges$id2[n]
			pat1 <- paste("\\b", pat1, "\\b", sep = "") # ensures exact match
			pat2 <- paste("\\b", pat2, "\\b", sep = "")
			i1 <- grep(pat1, HPD$nodes$id)
			i2 <- grep(pat2, HPD$nodes$id)
			n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
			n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
			n1.rad <- c(n1.rad, HPD$nodes$rad[i1])
			n2.rad <- c(n2.rad, HPD$nodes$rad[i2])
			n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
			n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
			}

		fd <- data.frame(
			n1.id = HPD$edges$id1,
			n1.ax,
			n1.lab,
			n1.rad,
			n2.id = HPD$edges$id2,
			n2.ax,
			n2.lab,
			n2.rad,
			e.wt = HPD$edges$weight,
			e.col = HPD$edges$color)		
		}
	if (tex) {
		fd <- xtable(fd, hline.after = c(1), include.rownames = FALSE)
		align(fd) <- "|r|rrlr|rrlr|rl|"
		}	
	if (plot.list) return(fd)
	}

