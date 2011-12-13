

mineHPD <- function(HPD, option = "rad <- tot.edge.count") {
	
# Function to process HPD objects in various ways
# to dig out additional hidden info.
# dot2HPD can only use the attribute tags in the dot file.
# However, the graph intrinsically contains additional info
# which can be mapped into a Hive Plot
# This function can dig that info out.
# Additional methods are easily added to this function.

# Bryan Hanson, DePauw Univ, July 2011 onward

	edges <- HPD$edges
	nodes <- HPD$nodes
	nn <- length(nodes$id)

	if (option == "rad <- tot.edge.count") {

# This option assigns a radius value to a node
# based upon the total number of edges in which the node participates.

		for (n in 1:nn) {
			pat <- paste("\\b", nodes$id[n], "\\b", sep = "")
			p <- length(grep(pat, edges$id1))
			q <- length(grep(pat, edges$id2))
			nodes$radius[n] <- p + q			
			}
		
		}  ##### end of option == "rad <- tot.edge.count"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "axis <- source.man.sink") {

# This option assigns a node to an axis
# based upon whether it is a source, manager or sink
# by examining the edges.
# Note that this option assumes a directed
# input graph, as a source node only has "outgoing" edges etc.
# Thus all edges start at edges$id1 and end on edges$id2.

# source = axis 1, manager = axis 3, sink = axis  2
# Basic procedure is that a node only listed in
# edges$id2 is a sink etc.
# Do things affirmatively, not assuming a default:
# Ensures that things are done correctly.

		# if (!length(unique(nodes$axis)) == 3) {
			# stop("This option requires 3 unique axes")
			# }
		
		done <- FALSE # a check to make sure all nodes get an axis
		
		for (n in 1:nn) {
			pat <- paste("\\b", nodes$id[n], "\\b", sep = "")
			id1 <- grep(pat, edges$id1)
			id2 <- grep(pat, edges$id2)
			
			if ((length(id1) == 0) & (length(id2) > 0 )) {
				nodes$axis[n] <- 2
				done <- TRUE
				next
				} # these are sinks, as they only receive an edge
			
			# note that set operations below drop duplicate values
				
			if ((length(id1) > 0) & (length(id2) > 0 )) {
				common <- union(id1, id2)
				source <- setdiff(id1, common)
				if (length(source) == 1) {
					nodes$axis[n] <- 1
					done <- TRUE
					next		
					} # these are sources

				if (length(common) >= 1) {
					nodes$axis[n] <- 3
					done <- TRUE
					next		
					} # these are managers
				} 

			if (!done) {
				msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
				warning(msg)
				}  # alert the user there was a problem
			
			} # end of loop inspecting nodes

		nodes$axis <- as.integer(nodes$axis)
		
		}  ##### end of option == "axis <- source.man.sink

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "remove orphans") {

# This option removes orphaned nodes (which have no edges)
# Almost the same code as over in sumHPD

		e.ids <- union(HPD$edges$id1, HPD$edges$id2)
		n.ids <- HPD$nodes$id
		prob <- setdiff(n.ids, e.ids)
		prob <- match(prob, HPD$nodes$id)
		if (length(prob) == 0) cat("\n\t No orphaned nodes were found\n")
		if (length(prob) > 0) {
			cat("\n\tThe following", length(prob), "nodes are orphaned (degree = 0):\n\n")
			nodes <- nodes[-prob,]
			}
		
		}  ##### end of option == "remove orphans"


### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	# Final assembly and checking...
	
	HPD$edges <- edges
	HPD$nodes <- nodes
	chkHPD(HPD)
	HPD
	}