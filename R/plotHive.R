

plotHive <- function(HPD, dr.nodes = TRUE,
	method = "abs", ...) {
	
	# function to plot hive plots using grid graphics
	# inspired by the work of Martin Kryzwinski
	# Bryan Hanson, DePauw Univ, Feb 2011 onward
	
	# Spherical coordinates will be used, even for the trivial cases
	
	# This function is intended to draw in 2D for nx from 2 to 6
	# The results will be similar to the original hive plot concept
	# Currently only nx = 2 or 3 is working.

##### Set up some common parameters
		
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
		x0 = p2cX(r.st, axst)
		y0 = p2cY(r.st, axst)

		r.end <- c(max1, max2)
		axend <- c(0, 180)
		x1 = p2cX(r.end, axend)
		y1 = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
		
		md <- max(abs(c(x0, y0, x1, y1)))*1.2 # max dimension
		
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md),
			name = "3DHivePlot")

		pushViewport(vp)

	# Draw axes
	
		grid.segments(x0, y0, x1, y1,
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

	# Mark the center
	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))
	
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
		
		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)

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
		axst <- c(0, 120, 240)
		x0 = p2cX(r.st, axst)
		y0 = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3)
		axend <- c(0, 120, 240)
		x1 = p2cX(r.end, axend)
		y1 = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0, y0, x1, y1)))*1.2 # max dimension
		grid.newpage()
		grid.rect(gp=gpar(fill="black"))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Draw axes
	
		grid.segments(x0, y0, x1, y1,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius) 
			theta <- c(rep(0, length(n1$radius)),
				rep(120, length(n2$radius)),
				rep(240, length(n3$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = nodes$size*nsf, col = nodes$color))
			}

	# Mark the center
	grid.points(0, 0, pch = 20, gp = gpar(col = "gray"))

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 120)
				r.end <- c(r.end, nodes$radius[id2])
				}
				
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

			grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 120)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 240)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)

	# Axis 3 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 240)
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

		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)

	# Axis 1 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 1) & (nodes$axis[id2] == 3)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 240)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 3) & (nodes$axis[id2] == 2)) {
				th.st <- c(th.st, 240)
				r.st <- c(r.st, nodes$radius[id1])
				th.end <- c(th.end, 120)
				r.end <- c(r.end, nodes$radius[id2])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
			pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
			id1 <- grep(pat1, nodes$id)
			id2 <- grep(pat2, nodes$id)
			
			if ((nodes$axis[id1] == 2) & (nodes$axis[id2] == 1)) {
				th.st <- c(th.st, 120)
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

		grid.curve(x0, y0, x1, y1,
			default.units = "native", ncp = 5, square = FALSE,
			gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)

		} # end of 3D
	
# ##### Four dimensional case (grid graphics/2D)

	# # Draw axes first
	
	# if (nx == 4) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)
		# max1 <- max(n1$radius)
		# max2 <- max(n2$radius)
		# max3 <- max(n3$radius)
		# max4 <- max(n4$radius)
		# min1 <- min(n1$radius)
		# min2 <- min(n2$radius)
		# min3 <- min(n3$radius)
		# min4 <- min(n4$radius)

		# r <- c(min1, max1, min2, max2, min3, max3, min4, max4) # in polar coordinates
		# theta <- c(45, 45, -45, -45, 135, 135, -135, -135)  # start, end, start, end
		# phi <- c(54.7, 54.7, 125.3, 125.3, 125.3, 125.3, 54.7, 54.7)
		# ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		# ax.coord <- sph2cart(ax.df)
		# segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)

	# # now add nodes

	# if (dr.nodes) {		
		# r <- c(n1$radius, n2$radius, n3$radius, n4$radius) 
		# phi <- c(rep(54.7, length(n1$radius)),
			# rep(125.3, length(n2$radius)),
			# rep(125.3, length(n3$radius)),
			# rep(54.7, length(n4$radius)))
		# theta <- c(rep(45, length(n1$radius)),
			# rep(-45, length(n2$radius)),
			# rep(135, length(n3$radius)),
			# rep(-135, length(n4$radius)))
		# n.df <- data.frame(radius = r, theta = theta, phi = phi)
		# n.coord <- sph2cart(n.df)
		# spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		# }
		
	# # now draw edges
		
		# tmp <- drawHiveSpline(HPD, ...)
	
	# # add a center sphere
	
		# spheres3d(0, 0, 0, col = "gray", radius = cs)
		
		# } # end of 4D
			
# ##### Five dimensional case (grid graphics/2D)

	# # Draw axes first
	
	# if (nx == 5) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)
		# n5 <- subset(nodes, axis == 5)
		# max1 <- max(n1$radius)
		# max2 <- max(n2$radius)
		# max3 <- max(n3$radius)
		# max4 <- max(n4$radius)
		# max5 <- max(n5$radius)
		# min1 <- min(n1$radius)
		# min2 <- min(n2$radius)
		# min3 <- min(n3$radius)
		# min4 <- min(n4$radius)
		# min5 <- min(n5$radius)
	
		# r <- c(min1, max1, min2, max2, min3, max3,
			# min4, max4, min5, max5) # in polar coordinates
		# theta <- c(0, 0, 120, 120, 240, 240, 0, 0, 0, 0)  # start, end, start, end
		# phi <- c(90, 90, 90, 90, 90, 90, 0, 0, 180, 180)
		# ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		# ax.coord <- sph2cart(ax.df)
		# segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[9:10,], col = axis.cols[5], line_antialias = TRUE, lwd = 4)
	
	# # now add nodes
		
	# if (dr.nodes) {		
		# r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius) 
		# phi <- c(rep(90, length(n1$radius)),
			# rep(90, length(n2$radius)),
			# rep(90, length(n3$radius)),
			# rep(0, length(n4$radius)),
			# rep(180, length(n5$radius)))
		# theta <- c(rep(0, length(n1$radius)),
			# rep(120, length(n2$radius)),
			# rep(240, length(n3$radius)),
			# rep(0, length(n4$radius)),
			# rep(0, length(n5$radius)))
		# n.df <- data.frame(radius = r, theta = theta, phi = phi)
		# n.coord <- sph2cart(n.df)
		# spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		# }
		
	# # now draw edges
		
		# tmp <- drawHiveSpline(HPD, ...)

	# # add a center sphere
	
		# spheres3d(0, 0, 0, col = "gray", radius = cs)

		# } # end of 5D
	
# ##### Six dimensional case (grid graphics/2D)

	# # Draw axes first
	
	# if (nx == 6) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)
		# n5 <- subset(nodes, axis == 5)
		# n6 <- subset(nodes, axis == 6)
		# max1 <- max(n1$radius)
		# max2 <- max(n2$radius)
		# max3 <- max(n3$radius)
		# max4 <- max(n4$radius)
		# max5 <- max(n5$radius)
		# max6 <- max(n6$radius)
		# min1 <- min(n1$radius)
		# min2 <- min(n2$radius)
		# min3 <- min(n3$radius)
		# min4 <- min(n4$radius)
		# min5 <- min(n5$radius)
		# min6 <- min(n6$radius)
	
		# r <- c(min1, max1, min2, max2, min3, max3,
			# min4, max4, min5, max5, min6, max6) # in polar coordinates
		# theta <- c(0, 0, 90, 90, 180, 180, 270, 270, 0, 0, 0, 0)  # start, end, start, end
		# phi <- c(90, 90, 90, 90, 90, 90, 90, 90, 0, 0, 180, 180)
		# ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		# ax.coord <- sph2cart(ax.df)
		# segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[9:10,], col = axis.cols[5], line_antialias = TRUE, lwd = 4)
		# segments3d(ax.coord[11:12,], col = axis.cols[6], line_antialias = TRUE, lwd = 4)
	
		# # now add nodes
		
	# if (dr.nodes) {		
		# r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius, n6$radius) 
		# phi <- c(rep(90, length(n1$radius)),
			# rep(90, length(n2$radius)),
			# rep(90, length(n3$radius)),
			# rep(90, length(n4$radius)),
			# rep(0, length(n5$radius)),
			# rep(180, length(n6$radius)))
		# theta <- c(rep(0, length(n1$radius)),
			# rep(90, length(n2$radius)),
			# rep(180, length(n3$radius)),
			# rep(270, length(n4$radius)),
			# rep(0, length(n5$radius)),
			# rep(0, length(n6$radius)))
		# n.df <- data.frame(radius = r, theta = theta, phi = phi)
		# n.coord <- sph2cart(n.df)
		# spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		# }
		
	# # now draw edges
		
		# tmp <- drawHiveSpline(HPD, ...)	

	# # add a center sphere
	
		# spheres3d(0, 0, 0, col = "gray", radius = cs)
		
		# } # end of 6D
	
	
	} # closing brace, this is the end!