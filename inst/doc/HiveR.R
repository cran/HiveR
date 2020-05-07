## ----SetUp, echo = FALSE, eval = TRUE-----------------------------------------
rm(list = ls())

suppressPackageStartupMessages(library("HiveR"))
suppressPackageStartupMessages(library("grid"))
suppressPackageStartupMessages(library("FuncMap"))
suppressPackageStartupMessages(library("sna"))
suppressPackageStartupMessages(library("xtable"))
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("bipartite"))

desc <- packageDescription("HiveR")
vers <- paste("version", desc$Version)

set.seed(123)
# use pdfcrop if it exists
if (Sys.which("pdfcrop") != "") knit_hooks$set(crop = hook_pdfcrop)
opts_chunk$set(echo = FALSE, fig.path = "./graphics/")

## ----PPNdata------------------------------------------------------------------
data(Safariland)

## ----PPNA, fig.cap = "Safariland data set plotted with function \\texttt{bipartite::visweb}."----
visweb(Safariland)

## ----PPN4, fig.cap = "Safariland data set plotted with function \\texttt{bipartite::plotweb}.", fig.width = 5, fig.height = 5----
plotweb(Safariland, text.rot = 90, adj.high = 0, adj.low = 1, y.lim = c(0, 2), labsize = 0.8)

## ----PPN5, fig.cap = "Safariland data set plotted with function \\texttt{sna::gplot} (mode = circle).", warning = FALSE, fig.height = 5, fig.width = 5----
gplot(Safariland, gmode = "graph", edge.lwd = 0.05,
	vertex.col = c(rep("green", 9), rep("red", 27)),
	mode = "circle")

## ----PPN6, fig.cap = "Safariland data set plotted with function \\texttt{sna::gplot} (mode = Fruchterman-Reingold).", warning = FALSE, fig.height = 5, fig.width = 5----
gplot(Safariland, gmode = "graph", edge.lwd = 0.05,
	vertex.col = c(rep("green", 9), rep("red", 27)))

## ----PPN2, fig.cap = "Hive Panel comparing Safari with Arroyo.", fig.height = 5, fig.width = 5----
data(Safari)
Safari$nodes$size <- 0.5
data(Arroyo)
Arroyo$nodes$size <- 0.5

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
#
pushViewport(vplayout(1, 1)) # upper plot
plotHive(Safari, ch = 0.1, axLabs = c("plants", "pollinators"), axLab.pos = c(0.15, 0.15), rot = c(-90, 90), np = FALSE, axLab.gpar = gpar(fontsize = 16, col = "white"))
grid.text("Safari (undisturbed)", x = 0.5, y = 0.95, default.units = "npc", gp = gpar(fontsize = 20, col = "white"))
popViewport(2)
#
pushViewport(vplayout(2, 1)) # lower plot
plotHive(Arroyo, ch = 0.1, axLabs = c("plants", "pollinators"), axLab.pos = c(0.15, 0.15), rot = c(-90, 90), np = FALSE, axLab.gpar = gpar(fontsize = 16, col = "white"))
grid.text("Arroyo (disturbed)", x = 0.5, y = 0.95, default.units = "npc", gp = gpar(fontsize = 20, col = "white"))

## ----E_coli_1aa---------------------------------------------------------------
tmp <- readLines("network_tf_gene.parsed.dot")[1595:1605]
# format = "latex" is needed to keep caption in margin
# booktabs = TRUE gives better formatting of the table
kable(tmp, caption = "Portion of a DOT file.\\label{DOT}", format = "latex", booktabs = TRUE)

## ----EI-----------------------------------------------------------------------
tab <- read.csv(file = "EdgeInst.csv")
kable(tab, caption = "Contents of edge instruction file.\\label{EI}", format = "latex", booktabs = TRUE)

## ----E_coli_1a, echo = TRUE, tidy = FALSE-------------------------------------
EC1 <- dot2HPD(file = "network_tf_gene.parsed.dot",
	node.inst = NULL,
	edge.inst = "EdgeInst.csv",
	desc = "E coli gene regulatory network (RegulonDB)",
	axis.cols = rep("grey", 3))

## ----E_coli_1b, echo = TRUE---------------------------------------------------
sumHPD(EC1)

## ----E_coli_1c, echo = TRUE---------------------------------------------------
EC2 <- mineHPD(EC1, option = "rad <- tot.edge.count")
sumHPD(EC2)

## ----E_coli_1d, echo = TRUE---------------------------------------------------
EC3 <- mineHPD(EC2, option = "axis <- source.man.sink")
sumHPD(EC3)

## ----E_coli_1e, echo = TRUE---------------------------------------------------
EC4 <- mineHPD(EC3, option = "remove zero edge")
sumHPD(EC4)

## ----E_coli_1f, echo = TRUE---------------------------------------------------
edges <- EC4$edges
edgesR <- subset(edges, color == 'red')
edgesG <- subset(edges, color == 'green')
edgesO <- subset(edges, color == 'orange')

edges <- rbind(edgesO, edgesG, edgesR)
EC4$edges <- edges

EC4$edges$weight <- 0.5

## ----E_coli_2, fig.cap = "Hive panel of E. coli gene regulatory network.", fig.width = 2, fig.height = 6----
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
#
pushViewport(vplayout(1, 1)) # upper plot

plotHive(EC4, dr.nodes = FALSE, ch = 20,
axLabs = c("source", "sink", "manager"),
axLab.pos = c(40, 75, 35),
axLab.gpar = gpar(fontsize = 6, col = "white", lwd = 2),
arrow = c("degree", 150, 100, 180, 70), np = FALSE)
grid.text("native units", x = 0.5, y = 0.05, default.units = "npc", gp = gpar(fontsize = 8, col = "white"))

popViewport(2)
#
pushViewport(vplayout(2, 1)) # middle plot

plotHive(EC4, dr.nodes = FALSE, method = "rank", ch = 100,
#axLabs = c("source", "sink", "manager"),
#axLab.pos = c(100, 125, 180),
#axLab.gpar = gpar(fontsize = 10, col = "white"),
np = FALSE)
grid.text("ranked units", x = 0.5, y = 0.05, default.units = "npc", gp = gpar(fontsize = 8, col = "white"))

popViewport(2)
#
pushViewport(vplayout(3, 1)) # lower plot

plotHive(EC4, dr.nodes = FALSE, method = "norm", ch = 0.1, axLabs = c("source", "sink", "manager"),
axLab.pos = c(0.1, 0.2, 0.2), axLab.gpar = gpar(fontsize = 6, col = "white"), np = FALSE)
grid.text("normed units", x = 0.5, y = 0.05, default.units = "npc", gp = gpar(fontsize = 8, col = "white"))

