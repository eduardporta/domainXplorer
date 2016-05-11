args <- commandArgs(trailingOnly = TRUE)

m.data <- read.table (file = args[1], sep = "\t", header = TRUE, row.names = 1)
m.data <- as.matrix (m.data)
m.data <- t (m.data)

res.table <- matrix (NA, nrow = ncol (m.data), ncol = 6)
rownames (res.table) <- colnames (m.data)
colnames (res.table) <- c ("p.linear.model", "p.feat.wt", "p.feat.rest", "mean.feat", "mean.rest", "mean.wt")

for (i in 3:ncol (m.data)) {
	
	if (args[3] == 1) {
		res <- lm(m.data[,1] ~ m.data[,i])	
		res.sum <- anova (res)
		res.table[i,1] <- res.sum$'Pr(>F)'[1]
	} else {
		res <- anova(lm (m.data[,1] ~  m.data[,2] + m.data[,i]))
		res.table[i,1] <- res$'Pr(>F)'[2]
	}
	
	sub.feat <- subset (as.numeric(as.character(m.data[,1])), m.data[,i] == "Feat")
	sub.rest <- subset (as.numeric(as.character(m.data[,1])), m.data[,i] == "Rest")
	sub.wt <- subset (as.numeric(as.character(m.data[,1])), m.data[,i] == "WT")
	
	if (is.finite(mean (sub.feat))) {
		res.table[i,4] <- mean (sub.feat)
	}
	
	if (is.finite(mean (sub.rest))) {
		res.table[i,5] <- mean (sub.rest)
	}
	
	if (is.finite(mean (sub.wt))) {
		res.table[i,6] <- mean (sub.wt)
	}
	
	if (length (sub.feat) > 2 & length (sub.wt) > 2) {
		res.p <- wilcox.test (sub.feat, sub.wt)
		res.table[i,2] <- res.p$p.value
	}
	
	if (length (sub.feat) > 2 & length (sub.rest) > 2) {
		res.p <- wilcox.test (sub.feat, sub.rest)
		res.table[i,3] <- res.p$p.value
	}
}

res.table <- as.data.frame (res.table)
res.table$pcorr <- p.adjust (res.table$p.linear.model, method = "fdr")

write.table (res.table, file = args[2], sep = "\t", col.names = FALSE, row.names = TRUE, quote = FALSE)