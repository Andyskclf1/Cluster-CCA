# some interaction plots
# load results table
# results_table = readRDS("results_table.rds")

interaction.plot(x.factor = results_table$noise, 
                 trace.factor = results_table$correlation, 
                 response = results_table$ari,
                 trace.label = "Correlation",
                 ylab = "ARI",
                 xlab = "Noise",
                 lty = 1,
                 lwd = 1,
                 pch = 19,
                 col = c("blue", "red", "green"),
                 bty = "n")

par(mfrow=c(1,2))
interaction.plot(x.factor = results_table$subject_count, 
                 trace.factor = results_table$correlation, 
                 response = results_table$tuckers,
                 trace.label = "Correlation",
                 ylab = "Tucker's congruence",
                 xlab = "# Subjects per cluster",
                 lty = 1,
                 lwd = 2,
                 xpd = T,
                 col = c("blue", "red", "green"),
                 bty ="n",
                 legend = F)

interaction.plot(x.factor = results_table$variable_b_count, 
                 trace.factor = results_table$correlation, 
                 response = results_table$tuckers,
                 trace.label = "Correlation",
                 ylab = "Tucker's congruence",
                 xlab = "# Variables per modality",
                 lty = 1,
                 lwd = 2,
                 col = c("blue", "red", "green"),
                 bty = "n")
