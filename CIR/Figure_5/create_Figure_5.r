require (ggplot2)
require (gridExtra)
require (reshape)

pol.data <- read.table ("expression_POLR3B_typeI_IFNS.txt", sep = "\t", header = TRUE)
pol.data$Group <- factor (pol.data$Group, levels =c  ("Feat", "CTD", "Rest", "WT"), labels = c ("Hybrid\nbinding", "Clamp", "Other", "WT"))
scatter.polr3b <- ggplot (subset (pol.data, Group != "WT"), aes (x = Pos, y = Score)) + geom_point (aes (color = Group)) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), legend.position = "none", panel.background = element_blank(), axis.title.y = element_text (face = "italic")) + ylab ("ESTIMATE\nimmune score\n") + xlab ("\nPOLR3B position") + scale_color_manual (values = c ("#FF6600", "#6699FF", "#E1D4C0")) + geom_vline (xintercept = 667, linetype = 2) + geom_vline (xintercept = 1038, linetype = 2) + geom_vline (xintercept = 1045, linetype = 2) + geom_vline (xintercept = 1128, linetype = 2)
group.plot <- ggplot (pol.data, aes (x = Group, y = Score)) + geom_boxplot (aes(color = Group), outlier.shape = NA, position = position_dodge ()) + geom_point (position=position_jitter(width = 0.2), aes (fill = Group, color = Group, alpha = Group, size = Group), shape = 21) + scale_alpha_manual (values = c (1,1,1,0.1)) + scale_fill_manual (values = c ("#FF6600", "#6699FF", "#E1D4C0", "lightgray")) + scale_color_manual (values = c ("#FF6600", "#6699FF", "#E1D4C0", "lightgray")) + scale_size_manual (values = c (2,2,2,0.5)) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank(), legend.position = "none", axis.title = element_blank(), axis.title.y = element_text (face = "italic")) + ylim (-2500, 3500)

exp.melt <- melt (pol.data)
exp.melt$variable <- factor (exp.melt$variable, levels = c ("POLR3B", "POLR3A", "DDX58", "TMEM173", "IRF3", "IFNA1", "IFNA2", "IFNB1", "IFNE", "IFNAR1", "IFNAR2", "IFNG"), labels = c ("POLR3B", "POLR3A", "RIG-1", "STING", "IRF3", "IFNA1", "IFNA2", "IFNB1", "IFNE", "IFNAR1", "IFNAR2", "IFNG"))
exp.plot <- ggplot (subset (exp.melt, variable %in% c ("IRF3", "STING", "IFNG", "RIG-1", "POLR3A", "POLR3B",  "IFNA1", "IFNA2", "IFNE", "IFNAR1", "IFNAR2", "IFNB1")), aes (x = variable, y = value)) + geom_boxplot (aes (color = Group), outlier.shape = NA) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank(), axis.text.x = element_text (angle = 45, hjust = 1), legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_text (face = "italic")) + ylab ("\nExpression\n") + geom_point (position = position_jitterdodge (jitter.width = 0.2), aes (fill = Group, color = Group, alpha = Group, size = Group)) + scale_color_manual (values = c ("#FF6600", "#6699FF", "#E1D4C0", "lightgray")) + scale_alpha_manual (values = c (1, 1, 1, 0.1)) + scale_size_manual (values = c (1.5,1.5,1.5,1))

figure.5 <- arrangeGrob (arrangeGrob (scatter.polr3b, group.plot, ncol = 2, widths = c(3/5,2/5)), exp.plot, ncol = 1)

ggsave (file = "Figure_5.pdf", figure.5)