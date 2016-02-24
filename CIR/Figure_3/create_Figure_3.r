require (ggplot2)
require (gridExtra)

mut.data <- read.table ("mut_profile_CDH11.txt", sep = "\t", header = TRUE)
text.p1.cdh11 <- data.frame (x = 1.5, y = 3300, label = c ("p < 0.001"))
text.p2.cdh11 <- data.frame (x = 2, y = 4000, label = c ("p < 0.001"))
mut.data$Group <- factor (mut.data$Group, levels = c ("Feat", "Rest", "WT"), labels = c ("IDR\n703-762", "Other CDH11\nregions", "WT"))
scatter.cdh11 <- ggplot (subset (mut.data, Pos > 0), aes (x = Pos, y = Score)) + geom_point (aes (color = Group)) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), legend.position = "none", panel.background = element_blank()) + ylab ("ESTIMATE score\n") + scale_color_manual (values = c ("#FF6600", "#E1D4C0")) + geom_vline (xintercept = 703, linetype = 2) + geom_vline (xintercept = 762, linetype = 2) + xlab ("\nCDH11 position")
box.plot <- ggplot (mut.data, aes (x = Group, y = Score)) + geom_boxplot (aes(color = Group), outlier.shape = NA, position = position_dodge ()) + geom_point (position=position_jitter(width = 0.2), aes (fill = Group, color = Group, alpha = Group, size = Group), shape = 21) + scale_alpha_manual (values = c (1,1,0.1)) + scale_fill_manual (values = c ("#FF6600", "#E1D4C0", "lightgray")) + scale_color_manual (values = c ("#FF6600", "#E1D4C0", "lightgray")) + scale_size_manual (values = c (2,2,0.5)) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank(), legend.position = "none", axis.title.x = element_blank()) + ylab ("ESTIMATE score\n") + geom_text (data = text.p1.cdh11, aes (x = x, y = y, label = label), fontface = "italic", size = 3) + geom_text (data = text.p2.cdh11, aes (x = x, y = y, label = label), fontface = "italic", size = 3) + geom_segment (aes (x = 1, xend = 2, y = 3000, yend = 3000), size = 0.15) + geom_segment (aes (x = 1, xend = 3, y = 3700, yend = 3700), size = 0.15)

fig.top <- arrangeGrob (scatter.cdh11, box.plot, widths = c (3/5, 2/5), nrow = 1)

exp.data <- read.table ("correlations_CTNNB1.txt", sep = "\t", header = TRUE)

text.mrna <- data.frame (x = 500, y = 9.5, label = c ("p > 0.2"))
plot.mrna <- ggplot (exp.data, aes (x = ESTIMATE, y = CTNNB1)) + geom_point (alpha = 0.05) + geom_smooth (color = "#F67851", method = "lm") + theme (axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), axis.text = element_text (color = "black"), panel.background = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) + ylab (expression (paste (beta, "-catenin mRNA"))) + geom_text (data = text.mrna, aes (x = x, y = y, label = label), fontface = "italic", size = 4)
text.prot <- data.frame (x = 500, y = -3, label = c ("p < 1e-10"))
plot.prot <- ggplot (exp.data, aes (x = ESTIMATE, y = Protein)) + geom_point (alpha = 0.05) + geom_smooth (color = "#F67851", method = "lm") + theme (axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), axis.text = element_text (color = "black"), panel.background = element_blank()) + ylab (expression (paste (beta, "-catenin protein"))) + geom_text (data = text.prot, aes (x = x, y = y, label = label), fontface = "italic", size = 4) + xlab ("\nESTIMATE score")
fig.c <- arrangeGrob (plot.mrna, plot.prot, ncol = 1, heights = c (9/20, 11/20))

text.cd3 <- data.frame (x = 2, y = 7.2, label = c ("CD3E\np < 1e-10"))
text.cd8 <- data.frame (x = -2, y = 7, label = c ("CD8A\np < 1e-10"))
text.cdh11 <- data.frame (x = 2.2, y = 10.3, label = c ("CDH11\np < 2e-5"))
text.cdh1 <- data.frame (x = -2.2, y = 12.3, label = c ("CDH1\np < 1e-10"))

correlation.plot <- ggplot (exp.data, aes (x = Protein, y = CD3E)) + geom_smooth (method = "lm", color = "darkblue") + geom_smooth (aes (y = CD8A), method = "lm", color = "blue") + geom_smooth (aes (y = CDH1), color = "orange", method = "lm") + geom_smooth (aes (y = CDH11), color = "red", method = "lm") + xlab (expression (paste (beta, "-catenin protein"))) + ylab ("Gene level\n") + theme (axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), axis.text = element_text (color = "black"), panel.background = element_blank()) + theme (plot.margin = unit (c (-0.5,-0.5,1,1), "lines")) + geom_text (data = text.cd3, aes (x = x, y = y, label = label), fontface = "italic", size = 3, color = "darkblue") + geom_text (data = text.cd8, aes (x = x, y = y, label = label), color = "blue", fontface = "italic", size = 3) + geom_text (data = text.cdh11, aes (x = x, y = y, label = label), fontface = "italic", size = 3, color = "red") + geom_text (data = text.cdh1, aes (x = x, y = y, label = label), size = 3, fontface = "italic", color = "orange")
histo.top <- ggplot (exp.data, aes (x = Protein)) + geom_density (color = "black", fill = "black", alpha = 0.2) + theme (axis.ticks = element_blank(), axis.line = element_line (color = "white"), line = element_blank(), axis.text = element_text (color = "white"), panel.background = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text (color = "white")) + ylab ("\n") + theme (plot.margin = unit (c (1,-0.5,0,1), "lines"), panel.grid = element_blank())
histo.right <- ggplot (exp.data, aes (x = CD8A)) + geom_density (color = "blue", fill = "blue", alpha = 0.2) + geom_density (aes (x = CD3E), color = "darkblue", fill = "darkblue", alpha = 0.2) + theme (axis.ticks = element_blank(), axis.line = element_line (color = "white"), line = element_blank(), axis.text = element_text (color = "white"), panel.background = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_text (color = "white"), axis.ticks.y = element_blank()) + ylab ("Samples") + coord_flip () + geom_density (aes (x = CDH11), color = "red", fill = "red", alpha = 0.2) + geom_density (aes (x = CDH1), color = "orange", fill = "orange", alpha = 0.2) + theme (plot.margin = unit (c (1,1,1,-0.5), "lines"), panel.grid = element_blank())
empty.plot <- ggplot (exp.data, aes (x = CDH1, y = CDH11)) + geom_blank() + theme (axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), panel.background = element_blank(), axis.title = element_blank(), line = element_blank())
fig.d <- arrangeGrob (histo.top, empty.plot, correlation.plot, histo.right, heights = c (4/20, 16/20), widths = c (16/20, 4/20))

fig.middle <- arrangeGrob (fig.c, fig.d, nrow = 1, widths = c (2/5, 5/5))

mech.data <- read.table ("table_correlation_MECHISMO_ESTIMATE.csv", sep = "\t", header = TRUE)
mech.text <- data.frame (x = 500, y = 2.3, text = c ("R > 0.9"))
mech.plot <- ggplot (mech.data, aes (x = ESTIMATE, y = Mechismo)) + geom_point (color = "orange") + geom_smooth (method = "lm") + theme (axis.text = element_text (color = "black"), axis.line = element_line (color = "black"), axis.ticks = element_line (color = "black"), panel.background = element_blank()) + xlab ("\nESTIMATE score") + ylab ("Mechismo score\n") + geom_text (data = mech.text, aes (x = x, y = y, label = text), size = 4, fontface = "italic")
fig.bottom <- arrangeGrob (empty.plot, mech.plot, nrow = 1, widths = c (2/3, 1/3))

figure.3 <- arrangeGrob (fig.top, fig.middle, fig.bottom, ncol = 1, heights = c (2/5, 3/5, 2/5))

ggsave (file = "Figure_3.pdf", figure.3)