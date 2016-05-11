imm.data <- read.table ("ESTIMATE_and_neo_antigens.txt", sep = "\t", header = TRUE, check.names = FALSE)

nrow.df <- ncol (imm.data) - 6
mat.res <- data.frame ()

for (i in 1:nrow.df) {
	
	res1 <- anova (lm (ESTIMATE ~ Tissue + imm.data[,i+6], data = imm.data))
	res2 <- anova (lm (ESTIMATE ~ Tissue + Neo + imm.data[,i+6], data = imm.data))
		
	mat.res[i,1] <- colnames (imm.data)[i+6]
	mat.res[i,2] <- as.numeric(as.character(res1$'Pr(>F)'[2]))
	mat.res[i,3] <- as.numeric(as.character(res2$'Pr(>F)'[3]))
}

colnames (mat.res) <- c ("Region", "domainXplorer", "AdjNeo")

write.table (mat.res, file = "results_adjusting_neoags.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
