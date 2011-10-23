

plot3dHive <- function(HPD, dr.nodes = TRUE,
	method = "abs", ...) {
	
	# Function to plot 3D hive plots
	# inspired by the work of Martin Kryzwinski
	# Bryan Hanson, DePauw Univ, Feb 2011 onward
	
	# Spherical coordinates will be used, even for the trivial cases

	chkHPD(HPD)
	nx <- length(unique(HPD$nodes$axis))
	if (nx == 1) stop("Something is wrong: only one axis seems to be present")
	if ((nx == 2) | (nx == 3)) stop("Use plotHive for hive plots with 2 or 3 axes")

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
	# to drawHiveSpline so it must be updated here
	
	# Set up a scaling factor for the nodes
	# Also a few other things set up here:
	# nsf = node scaling factor
	# cs = center sphere size (nx > 3)

	bg3d("black") # black background to rgl graphics
	if (method == "norm") {nsf <- 0.01; cs <- m}
	if (method == "abs") {nsf <- 1.0; cs <- m}
	if (method == "rank") {nsf <- 0.1; cs <- 1}		

	
##### Four dimensional case (nx = 4, 5, 6 with rgl graphics)

	# Draw axes first
	
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

		r <- c(min1, max1, min2, max2, min3, max3, min4, max4) # in polar coordinates
		theta <- c(45, 45, -45, -45, 135, 135, -135, -135)  # start, end, start, end
		phi <- c(54.7, 54.7, 125.3, 125.3, 125.3, 125.3, 54.7, 54.7)
		ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		ax.coord <- sph2cart(ax.df)
		segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)

	# now add nodes

	if (dr.nodes) {		
		r <- c(n1$radius, n2$radius, n3$radius, n4$radius) 
		phi <- c(rep(54.7, length(n1$radius)),
			rep(125.3, length(n2$radius)),
			rep(125.3, length(n3$radius)),
			rep(54.7, length(n4$radius)))
		theta <- c(rep(45, length(n1$radius)),
			rep(-45, length(n2$radius)),
			rep(135, length(n3$radius)),
			rep(-135, length(n4$radius)))
		n.df <- data.frame(radius = r, theta = theta, phi = phi)
		n.coord <- sph2cart(n.df)
		spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		}
		
	# now draw edges
		
		tmp <- drawHiveSpline(HPD, ...)
	
	# add a center sphere
	
		spheres3d(0, 0, 0, col = "gray", radius = cs)
		
		} # end of 4D
			
##### Five dimensional case

	# Draw axes first
	
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
	
		r <- c(min1, max1, min2, max2, min3, max3,
			min4, max4, min5, max5) # in polar coordinates
		theta <- c(0, 0, 120, 120, 240, 240, 0, 0, 0, 0)  # start, end, start, end
		phi <- c(90, 90, 90, 90, 90, 90, 0, 0, 180, 180)
		ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		ax.coord <- sph2cart(ax.df)
		segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[9:10,], col = axis.cols[5], line_antialias = TRUE, lwd = 4)
	
	# now add nodes
		
	if (dr.nodes) {		
		r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius) 
		phi <- c(rep(90, length(n1$radius)),
			rep(90, length(n2$radius)),
			rep(90, length(n3$radius)),
			rep(0, length(n4$radius)),
			rep(180, length(n5$radius)))
		theta <- c(rep(0, length(n1$radius)),
			rep(120, length(n2$radius)),
			rep(240, length(n3$radius)),
			rep(0, length(n4$radius)),
			rep(0, length(n5$radius)))
		n.df <- data.frame(radius = r, theta = theta, phi = phi)
		n.coord <- sph2cart(n.df)
		spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		}
		
	# now draw edges
		
		tmp <- drawHiveSpline(HPD, ...)

	# add a center sphere
	
		spheres3d(0, 0, 0, col = "gray", radius = cs)

		} # end of 5D
	
##### Six dimensional case

	# Draw axes first
	
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
	
		r <- c(min1, max1, min2, max2, min3, max3,
			min4, max4, min5, max5, min6, max6) # in polar coordinates
		theta <- c(0, 0, 90, 90, 180, 180, 270, 270, 0, 0, 0, 0)  # start, end, start, end
		phi <- c(90, 90, 90, 90, 90, 90, 90, 90, 0, 0, 180, 180)
		ax.df <- data.frame(radius = r, theta = theta, phi = phi)
		ax.coord <- sph2cart(ax.df)
		segments3d(ax.coord[1:2,], col = axis.cols[1], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[3:4,], col = axis.cols[2], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[5:6,], col = axis.cols[3], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[7:8,], col = axis.cols[4], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[9:10,], col = axis.cols[5], line_antialias = TRUE, lwd = 4)
		segments3d(ax.coord[11:12,], col = axis.cols[6], line_antialias = TRUE, lwd = 4)
	
		# now add nodes
		
	if (dr.nodes) {		
		r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius, n6$radius) 
		phi <- c(rep(90, length(n1$radius)),
			rep(90, length(n2$radius)),
			rep(90, length(n3$radius)),
			rep(90, length(n4$radius)),
			rep(0, length(n5$radius)),
			rep(180, length(n6$radius)))
		theta <- c(rep(0, length(n1$radius)),
			rep(90, length(n2$radius)),
			rep(180, length(n3$radius)),
			rep(270, length(n4$radius)),
			rep(0, length(n5$radius)),
			rep(0, length(n6$radius)))
		n.df <- data.frame(radius = r, theta = theta, phi = phi)
		n.coord <- sph2cart(n.df)
		spheres3d(n.coord$x, n.coord$y, n.coord$z, col = nodes$color, radius = nodes$size*nsf)
		}
		
	# now draw edges
		
		tmp <- drawHiveSpline(HPD, ...)	

	# add a center sphere
	
		spheres3d(0, 0, 0, col = "gray", radius = cs)
		
		} # end of 6D
	
	
	} # closing brace, this is the end!