

centerHole <- function(HPD) {
	
	# Function to compute the center hole of a Hive Plot Object
	# HPD$center.hole is a fraction; this is converted
	# to an absolute number based upon the shortest axis
	# Part of Hive3dR
	# Bryan Hanson, DePauw Univ, July 2011 onward

	chkHPD(HPD)
	nodes <- HPD[[1]]
	nx <- length(unique(nodes$axis))
	f <- HPD$center.hole
	
	if (nx == 2) {
		n1 <- which(nodes$axis == 1)
		n2 <- which(nodes$axis == 2)
		l1 <- diff(range(nodes$radius[n1])) # these could all be simplifed later?
		l2 <- diff(range(nodes$radius[n2]))
		ch <- min(l1, l2)*f
		}

	if (nx == 3) {
		n1 <- which(nodes$axis == 1)
		n2 <- which(nodes$axis == 2)
		n3 <- which(nodes$axis == 3)
		l1 <- diff(range(nodes$radius[n1]))
		l2 <- diff(range(nodes$radius[n2]))
		l3 <- diff(range(nodes$radius[n3]))
		ch <- min(l1, l2, l3)*f
		}

	if (nx == 4) {
		n1 <- which(nodes$axis == 1)
		n2 <- which(nodes$axis == 2)
		n3 <- which(nodes$axis == 3)
		n4 <- which(nodes$axis == 4)
		l1 <- diff(range(nodes$radius[n1]))
		l2 <- diff(range(nodes$radius[n2]))
		l3 <- diff(range(nodes$radius[n3]))
		l4 <- diff(range(nodes$radius[n4]))
		ch <- min(l1, l2, l3, l4)*f
		}

	if (nx == 5) {
		n1 <- which(nodes$axis == 1)
		n2 <- which(nodes$axis == 2)
		n3 <- which(nodes$axis == 3)
		n4 <- which(nodes$axis == 4)
		n5 <- which(nodes$axis == 5)
		l1 <- diff(range(nodes$radius[n1]))
		l2 <- diff(range(nodes$radius[n2]))
		l3 <- diff(range(nodes$radius[n3]))
		l4 <- diff(range(nodes$radius[n4]))
		l5 <- diff(range(nodes$radius[n5]))
		ch <- min(l1, l2, l3, l4, l5)*f
		}

	if (nx == 6) {
		n1 <- which(nodes$axis == 1)
		n2 <- which(nodes$axis == 2)
		n3 <- which(nodes$axis == 3)
		n4 <- which(nodes$axis == 4)
		n5 <- which(nodes$axis == 5)
		n6 <- which(nodes$axis == 6)
		l1 <- diff(range(nodes$radius[n1]))
		l2 <- diff(range(nodes$radius[n2]))
		l3 <- diff(range(nodes$radius[n3]))
		l4 <- diff(range(nodes$radius[n4]))
		l5 <- diff(range(nodes$radius[n5]))
		l6 <- diff(range(nodes$radius[n6]))
		ch <- min(l1, l2, l3, l4, l5, l6)*f
		}
		
	ch
	}
	