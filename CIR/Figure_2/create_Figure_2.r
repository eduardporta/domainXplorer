require (scales)
require (ggrepel)
require (scales)

#NOTE THAT IT WON'T PUT THE ACTUAL NAMES OF THE PROTEINS, BUT THEIR ENSEMBLIDs INSTEAD
#IT WON'T PUT IN THE NETWORK EITHER, AS IT WASN'T GENERATED WITH R BUT GEPHI

#GOT IT FROM http://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
}

#THIS CALCULATES THE P VALUES - CHECK THAT SCRIPT IF YOU WANT TO REPEAT THAT ANALYSIS
source ("recalculate_adjusting_neoags.r")

#THIS DOES THE ACTUAL PLOTTING
cor.data <- read.table ("results_adjusting_neoags.txt", sep = "\t", header = TRUE)
cor.data$Label <- ifelse (cor.data$Region %in% c ("ENSP00000299367-PF00092-254-452", "ENSP00000268603-IDR-703-762", "ENSP00000378752-IDR-508-589", "ENSP00000308541-PF00089-364-613", "ENSP00000308938-PF00089-582-803", "ENSP00000228347-PF04560-1043-1128", "ENSP00000237172-IDR-147-255", "ENSP00000407586-PF00009-45-162", "ENSP00000271450-IDR-257-317"), 1, 0)
text.cor.p <- data.frame (x = 0.3, y = 0.0005, label = c ("R > 0.9"))
figure.2 <- ggplot (cor.data, aes (x = domainXplorer, y = AdjNeo)) + geom_point (aes (color = as.factor(Label), size = as.factor (Label))) + geom_vline (xintercept = 0.1, linetype = 2) + geom_hline (yintercept = 0.1, linetype = 2) + geom_smooth (method = "lm", color = "black", size = 0.5) + geom_label_repel (data = subset (cor.data, Label == 1), aes (label = Region)) + theme (legend.position = "none", axis.text = element_text (color = "black"), axis.ticks = element_line (color = "black"), axis.line = element_line (color = "black"), panel.background = element_blank()) + scale_color_manual (values = c ("darkgray", "black")) + scale_size_manual (values = c (1, 2)) + scale_x_continuous (trans=reverselog_trans(10)) + scale_y_continuous (trans=reverselog_trans(10)) + xlab ("\nP domainXplorer") + ylab ("P domainXplorer and neoantigens\n") + geom_text (data = text.cor.p, aes (x = x, y = y, label = label), fontface = "italic", size = 5)

ggsave (file = "Figure_2.pdf", figure.2)
