require (survival)
require (ggplot2)
require (gridExtra)
require (scales)

#THIS IS THE COMMAND FOR THE BOX/JITTER PLOT SHOWING THE DISTRIBUTION OF ESTIMATE SCORES

imm.data <- read.table ("clinical_cyt_estimate_together.txt", sep = "\t", header = TRUE)
data.plot <- subset (imm.data, Tissue != "ucec")
data.plot$Tissue <- factor (data.plot$Tissue, levels = c ("gbm",  "lihc", "cesc", "brca", "kirc", "thca", "lgg",  "pcpg", "kirp", "read", "prad", "hnsc", "blca", "skcm", "luad", "coad", "lusc", "ucs",  "kich", "paad", "acc"), labels = c ("Gliblastoma",  "Liver", "Cervical", "Breast", "Kidney clear cell", "Thyroid", "Lower grade glioma",  "Pheochromocytoma", "Kidney papillary cell", "Rectal adenocarcinoma", "Prostate", "Head and neck", "Bladder", "Melanoma", "Lung adenocarcinoma", "Colon adenocarcinoma", "Lung squamous cell", "Uterine carcinosarcoma", "Kidney chromophobe", "Pancreatic", "Adrenal"))
cor.plot <- ggplot (data.plot, (aes (y = Score, x = log10(Cyt)))) + geom_point (aes (color = Tissue), alpha = 0.2) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank(), legend.position = "none", axis.title.y = element_text (color = "black", face = "italic")) + xlab ("\nlog10(Cyt)") + ylab ("ESTIMATE immune score\n") + geom_smooth (color = "black", method = "lm") + ylim (-2000, 2000)
box.jit.plot <- ggplot (data.plot, aes (x = reorder (Tissue, -Score, mean), y = Score)) + geom_boxplot (outlier.shape = NA) + geom_point (position = position_jitter (width = 0.1), aes (color = Tissue)) + theme (axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank(), legend.position = "none", axis.title.y = element_text (color = "black", face = "italic"), axis.text.x = element_text (angle = 45, hjust = 1)) + xlab ("\nTCGA project") + ylab ("ESTIMATE immune score\n")

#THIS IS FOR THE SURVIVAL PART

source ("createSurvFrame.r")
sub.clean <- subset (imm.data, Tissue != "ucec")

tissues <- unique (sub.clean$Tissue)

sub.clean$Group <- "Low"

for (t in tissues) {
	sub.t <- subset (sub.clean, Tissue == as.character (t))
	sub.clean$Group <- ifelse (sub.clean$Tissue == as.character(t), ifelse (sub.clean$Score < quantile (sub.t$Score)[4], "Low", "High"), sub.clean$Group)
}

acc.surv <- survfit (Surv (Time, Status) ~ Group, data = subset (sub.clean, Tissue == "acc"))
acc.surv.frame <- createSurvivalFrame (acc.surv)
acc.surv.frame$strata <- factor (acc.surv.frame$strata, levels = c ("Group=Low", "Group=High"), labels = c ("Q < 75%", "75% < Q"))
acc.plot <- ggplot(data = acc.surv.frame, aes(colour = strata, group = strata)) + geom_step(aes(x = time, y = surv), direction = "hv") + geom_point(data = subset(acc.surv.frame, n.censor == 1), aes(x = time, y = surv), shape = 20) + theme(legend.position = "none", axis.text = element_text (color = "black"), panel.background = element_blank(), axis.line = element_line (color = "black"), axis.ticks = element_line (color = "black")) + xlab ("\nTime (days)") + scale_color_manual (values = c ("black", "red"), guide_legend (title = "Immune infiltrate quantile")) + ylab ("Survival\n") + ggtitle ("Adrenal") + scale_y_continuous(labels = percent, limits = c (0,1))

skcm.surv <- survfit (Surv (Time, Status) ~ Group, data = subset (sub.clean, Tissue == "skcm"))
skcm.surv.frame <- createSurvivalFrame (skcm.surv)
skcm.surv.frame$strata <- factor (skcm.surv.frame$strata, levels = c ("Group=Low", "Group=High"), labels = c ("Q < 75%", "75% < Q"))
skcm.plot <- ggplot(data = skcm.surv.frame, aes(colour = strata, group = strata)) + geom_step(aes(x = time, y = surv), direction = "hv") + geom_point(data = subset(skcm.surv.frame, n.censor == 1), aes(x = time, y = surv), shape = 20) + theme(legend.position = "none", axis.text = element_text (color = "black"), panel.background = element_blank(), axis.line = element_line (color = "black"), axis.ticks = element_line (color = "black")) + xlab ("\nTime (days)") + scale_color_manual (values = c ("black", "red"), guide_legend (title = "Immune infiltrate quantile")) + ylab ("Survival\n") + ggtitle ("Melanoma") + scale_y_continuous(labels = percent, limits = c (0,1))

hnsc.surv <- survfit (Surv (Time, Status) ~ Group, data = subset (sub.clean, Tissue == "hnsc"))
hnsc.surv.frame <- createSurvivalFrame (hnsc.surv)
hnsc.surv.frame$strata <- factor (hnsc.surv.frame$strata, levels = c ("Group=Low", "Group=High"), labels = c ("Q < 75%", "75% < Q"))
hnsc.plot <- ggplot(data = hnsc.surv.frame, aes(colour = strata, group = strata)) + geom_step(aes(x = time, y = surv), direction = "hv") + geom_point(data = subset(hnsc.surv.frame, n.censor == 1), aes(x = time, y = surv), shape = 20) + theme(legend.position = "none", axis.text = element_text (color = "black"), panel.background = element_blank(), axis.line = element_line (color = "black"), axis.ticks = element_line (color = "black")) + xlab ("\nTime (days)") + scale_color_manual (values = c ("black", "red"), guide_legend (title = "Immune infiltrate quantile")) + ylab ("Survival\n") + ggtitle ("Head and neck") + scale_y_continuous(labels = percent, limits = c (0,1))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

hnsc.plot.leg <- ggplot(data = hnsc.surv.frame, aes(colour = strata, group = strata)) + geom_step(aes(x = time, y = surv), direction = "hv") + geom_point(data = subset(hnsc.surv.frame, n.censor == 1), aes(x = time, y = surv), shape = 20) + theme(legend.position = "bottom", axis.text = element_text (color = "black"), panel.background = element_blank(), axis.line = element_line (color = "black"), axis.ticks = element_line (color = "black")) + xlab ("\nTime (days)") + scale_color_manual (values = c ("black", "red"), guide_legend (title = "Immune infiltrate quantile")) + ylab ("Survival\n") + ggtitle ("Head and neck") + scale_y_continuous(labels = percent, limits = c (0,1))
mylegend <- g_legend(hnsc.plot.leg)

figure.1 <- arrangeGrob (arrangeGrob (cor.plot, box.jit.plot, ncol = 2, widths = c (4, 7)), 
			arrangeGrob (acc.plot + theme (axis.title = element_text (size = 10), plot.title=element_text(size = 10)), 
						skcm.plot + theme (axis.title = element_text (size = 10), plot.title=element_text(size = 10), axis.text.y = element_blank(), axis.title.y = element_blank()), 
						hnsc.plot + theme (axis.title = element_text (size = 10), plot.title=element_text(size = 10), axis.text.y = element_blank(), axis.title.y = element_blank()), 
						nrow = 1), 
			mylegend,
			nrow = 3, 
			heights = c (10, 5, 1)
)

ggsave (file = "Figure_1.pdf", figure.1)