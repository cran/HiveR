

plotHive <- function(HPD, dr.nodes = TRUE,
	method = "abs", ...) {
	
	# Function to plot hive plots using grid graphics
	# Inspired by the work of Martin Kryzwinski
	# Bryan Hanson, DePauw Univ, Feb 2011 onward
	
	# This function is intended to draw in 2D for nx from 2 to 6
	# The results will be similar to the original hive plot concept
	# Currently only nx = 2 or 3 is working.

##### Set up some common parameters

	if (!HPD$type == "2D") stop("This is not a 2D hive data set: use plot3dHive instead")		
	chkHPD(HPD)
	nx <- length(unique(HPD$nodes$axis))

	if (nx == 1) stop("Something is wrong: only one axis seems to be present")

	# Send out for ranking/norming if requested
	
	if ((method == "rank") | (method == "norm")) HPD <- manipAxis(HPD, method)

	nodes <- HPD$nodes
	edges <- HPD$edges
	axis.cols <- HPD$axis.cols

	# Get dimension for center hole
	
	if (method == "abs") m <- centerHole(HPD)
	if (method == "norm") m <- HPD$center.hole
	if (method == "rank") {m <- centerHole(HPD); m <- floor(m)}
	nodes$radius <- nodes$radius + m
	HPD$nodes$radius <- nodes$radius # important, as HPD is passed
	# to drawHiveSpline for nx > 3
	
	# Set up a scaling factor for the nodes
	# (needed since there are two different graphics systems in use
	# and different methods args affect rgl size/appearance)
	# Also a few other things set up here:
	# nsf = node scaling factor
	# a couple of utility functions

	nsf <- 0.25
	p2cX <- function(r, theta) x <- r*cos(theta*2*pi/360)
	p2cY <- function(r, theta) y <- r*sin(theta*2*pi/360)

##### Two dimensional case  (using grid graphics)

	# Prep axes first
	
	if (nx == 2) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
	
		r.st <- c(min1, min2) # in polar coordinates
		axst <- c(0, 180)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2)
		axend <- c(0, 180)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
		
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.2 # max dimension
		
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md),
			name = "3DHivePlot")

		pushViewport(vp)


	# Mark the center
#	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))
	
	# Now draw edges
		
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if (nodes$axis[id1] == 1) { # set up edge start params 1st
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1])
				}
			if (nodes$axis[id1] == 2) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1])
				}

			if (nodes$axis[id2] == 1) { # now edge end params
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2])
				}
			if (nodes$axis[id2] == 2) {
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
				
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 8),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius) 
			theta <- c(rep(0, length(n1$radius)),
				rep(180, length(n2$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

		} # end of 2D
	
##### Three dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 3) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)

		r.st <- c(min1, min2, min3) # in polar coordinates
		axst <- c(90, 210, 330)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3)
		axend <- c(90, 210, 330)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.2 # max dimension
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)


	# Mark the center
#	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}
	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(210, length(n2$radius)),
				rep(330, length(n3$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

		} # end of 3D
	

##### Four dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 4) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)

		r.st <- c(min1, min2, min3, min4) # in polar coordinates
		axst <- c(90, 180, 270, 0)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4)
		axend <- c(90, 180, 270, 0)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.2 # max dimension
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Mark the center
#	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(180, length(n2$radius)),
				rep(270, length(n3$radius)),
				rep(0, length(n4$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

		} # end of 4D
	
##### Five dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 5) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
		n5 <- subset(nodes, axis == 5)
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		max5 <- max(n5$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)
		min5 <- min(n5$radius)

		r.st <- c(min1, min2, min3, min4, min5) # in polar coordinates
		axst <- c(90, 162, 234, 306, 18)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4, max5)
		axend <- c(90, 162, 234, 306, 18)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.2 # max dimension
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Mark the center
#	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 162)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 162)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 234)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 234)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 306)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 5)) {
				th.st <- c(th.st, 306)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 18)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 5 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 5) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 18)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}


	# Axis 1 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 5)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 18)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 5 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 5) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 18)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 306)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 306)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 234)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 234)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 162)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 162)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(162, length(n2$radius)),
				rep(234, length(n3$radius)),
				rep(306, length(n4$radius)),
				rep(18, length(n5$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

		} # end of 5D

##### Six dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 6) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
		n5 <- subset(nodes, axis == 5)
		n6 <- subset(nodes, axis == 6)
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		max5 <- max(n5$radius)
		max6 <- max(n6$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)
		min5 <- min(n5$radius)
		min6 <- min(n6$radius)

		r.st <- c(min1, min2, min3, min4, min5, min6) # in polar coordinates
		axst <- c(90, 150, 210, 270, 330, 390)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4, max5, max6)
		axend <- c(90, 150, 210, 270, 330, 390)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.2 # max dimension
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Mark the center
#	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 150)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 150)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 5)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 5 -> 6
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 5) & (nodes$axis[id2] == 6)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 390)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 6 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 6) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 390)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 6
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 6)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 390)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 6 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 6) & (nodes$axis[id2] == 5)) {
				th.st <- c(th.st, 390)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 5 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 5) & (nodes$axis[id2] == 4)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 4) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 150)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 150)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Draw axes
	
		# grid.segments(x0a, y0a, x1a, y1a,
			# gp = gpar(col = "black", lwd = 7),
			# default.units = "native") # more like linnet

		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius,
				n5$radius, n6$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(150, length(n2$radius)),
				rep(210, length(n3$radius)),
				rep(270, length(n4$radius)),
				rep(330, length(n5$radius)),
				rep(390, length(n6$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

		} # end of 6D

	
	} # closing brace, this is the end!