

ranHiveData <- function(nx = 4, nn = nx*15, ne = nx*15,
	rad = 1:100, ns = 1:3, ew = 1:3,
	nc = brewer.pal(5, "Set1"),
	ec = brewer.pal(5, "Set1"),
	axis.cols = brewer.pal(nx, "Set1"),
	desc = NULL,
	center.hole = 0.1,
	verbose = FALSE) {
	
# Function to generate random data for testing/demonstrating HiveR
# Bryan Hanson, DePauw Univ, June 2011 onward

# Defaults make small hives that draw fast and are not too cluttered

# nx = no. axes
# nn = no. nodes
# ne = no. edges
# nc = node color
# ec = edge color
# rad = possible values for radii
# ns = node size
# ew = edge weight/width
# desc = description
		
# Create a set of labels/names to choose from

	Labs <-rep(NA, nn)
	for (n in 1:nn) {
		Labs[n] <- rep(paste(letters[runif(1, 1, 26)],
		letters[runif(1, 1, 26)],
		letters[runif(1, 1, 26)],
		letters[runif(1, 1, 26)],
		letters[runif(1, 1, 26)], sep = ""))
		}
		
##### Create nodes df
	
	ndf <- data.frame(
		id = 1:nn,
		lab = as.character(Labs),
		axis = sample(1:nx, nn, replace = TRUE),
		radius = sample(rad, nn, replace = TRUE),
		size = sample(ns, nn, replace = TRUE),
		color = sample(nc, nn, replace = TRUE)
		)
	ndf$color <- as.character(ndf$color)
	
	# Clean up ndf by removing duplicates
	# (do before the creation of edf calls on these points)
	# Important: this means that nodes$id is not continuous!

	dup <- duplicated(ndf[,c(3,4)])
	if (any(dup)) {
		ndf <- ndf[-dup,]
		if (verbose) cat(length(any(!dup)), "duplicate nodes were removed\n\n")
		}

##### Create edges df
	
	edf <- data.frame(
		id1 = sample(ndf$id, ne, replace = TRUE),
		id2 = sample(ndf$id, ne, replace = TRUE),
		weight = sample(ew, ne, replace = TRUE),
		color = as.character(sample(ec, ne, replace = TRUE)))
	edf$color <- as.character(edf$color)

# Clean up edf

	# remove edges that start & end on the same point
	same.pt <- which(edf$id1 == edf$id2)
	if (length(!same.pt == 0)) {
		edf <-edf[-same.pt,]
		if (verbose) cat("Removing an edge (same.pt) = ", same.pt, "\n\n")
		}
	
	# remove edges that start & end on the same axis
	
	same.axis <- c()
	
	if (nx >= 2) { # going to use these values later too when checking colinearity
		one <- which(ndf$axis == 1) # row indices
		one <- ndf$id[one] # id values
		two <- which(ndf$axis == 2) # row indices
		two <- ndf$id[two] # id values
		for (n in 1:nrow(edf)) {
			if ((edf$id1[n] %in% one) && (edf$id2[n] %in% one)) same.axis <- c(same.axis, n)			
			if ((edf$id1[n] %in% two) && (edf$id2[n] %in% two)) same.axis <- c(same.axis, n)
			}
		}
		
	if (nx >= 3) {
		three <- which(ndf$axis == 3) # row indices
		three <- ndf$id[three] # id values
		for (n in 1:nrow(edf)) if ((edf$id1[n] %in% three) && (edf$id2[n] %in% three)) same.axis <- c(same.axis, n)			
		}
		
	if (nx >= 4) {
		four <- which(ndf$axis == 4) # row indices
		four <- ndf$id[four] # id values	
		for (n in 1:nrow(edf)) if ((edf$id1[n] %in% four) && (edf$id2[n] %in% four)) same.axis <- c(same.axis, n)			
		}
		
	if (nx >= 5) {
		five <- which(ndf$axis == 5)
		five <- ndf$id[five]		
		for (n in 1:nrow(edf)) if ((edf$id1[n] %in% five) && (edf$id2[n] %in% five)) same.axis <- c(same.axis, n)			
		}
		
	if (nx == 6) {
		six <- which(ndf$axis == 6) # row indices
		six <- ndf$id[six] # id values
		for (n in 1:nrow(edf)) if ((edf$id1[n] %in% six) && (edf$id2[n] %in% six)) same.axis <- c(same.axis, n)			
		}

	if (length(!same.axis == 0)) {
		edf <- edf[-same.axis,]
		if (verbose) cat("Removing an edge (same.axis) = ", same.axis, "\n\n")
		}
			
# For nx = 5 and 6, we need to remove edges that start and end on colinear axes

	colin <- c()
	
	if (nx == 5) {  # axes 4 & 5 are colinear
			
		for (n in 1:nrow(edf)) {
			if ((edf$id1[n] %in% four) && (edf$id2[n] %in% five)) colin <- c(colin, n)			
			if ((edf$id1[n] %in% five) && (edf$id2[n] %in% four)) colin <- c(colin, n)
			}
		
		if (length(!colin == 0)) {
			edf <- edf[-colin,] # remove the colinear edges
			if (verbose) cat("Removing colinear edges (nx = 5): ", colin, "\n\n")
			}
		}

	if (nx == 6) {
		
		# axes 1 & 3, 2 & 4, 5 & 6 are colinear
				
		for (n in 1:nrow(edf)) {
			if ((edf$id1[n] %in% one) && (edf$id2[n] %in% three)) colin <- c(colin, n)			
			if ((edf$id1[n] %in% two) && (edf$id2[n] %in% four)) colin <- c(colin, n)
			if ((edf$id1[n] %in% five) && (edf$id2[n] %in% six)) colin <- c(colin, n)
			}
		
		if (length(!colin == 0)) {
			edf <- edf[-colin,] # remove the colinear edges
			if (verbose) cat("Removing colinear edges (nx = 5): ", colin, "\n\n")
			}
		}
		
	# Finally, remove nodes that are not part of an edge
	# Note: another reason that nodes$id is not continous
		
	draw <- ndf$id %in% unique(c(edf$id1, edf$id2))
	if (any(!draw)) {
		ndf1 <- nrow(ndf)
		ndf <- ndf[draw,]
		ndf2 <- nrow(ndf)
		if (verbose) cat(ndf1 - ndf2, "nodes did not have any edges and have been removed\n\n")
		}

# Report results (also creates desc if needed):

	msg1 <- paste(nx, "D -- ", dim(ndf)[1], " nodes -- ", dim(edf)[1], " edges", sep = "")
	msg2 <- paste("Data set is", msg1)
	if (verbose) cat(msg2, "\n")
	
	if (!is.null(desc)) desc = paste(desc, " (", msg1, ")", sep = "")
	if (is.null(desc)) desc = msg1
	
# Fix up classes to meet definition

	ndf$lab <- as.character(ndf$lab)
	ndf$radius <- as.numeric(ndf$radius)
	edf$weight <- as.numeric(edf$weight)
	res <- list(nodes = ndf, edges = edf,
		desc = desc, dim = as.integer(nx),
		axis.cols = axis.cols, center.hole = center.hole)
	class(res) <- "HivePlotData"
	chkHPD(res)
	
	res
	}


