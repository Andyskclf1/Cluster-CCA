# some interaction plots
# load results table
# results_table = readRDS("results_table.rds")

library(dae)

par(mfrow=c(1,2))
interaction.plot(x.factor = results_table$variable_count, 
                 trace.factor = results_table$correlation, 
                 response = results_table$ari,
                 trace.label = "Correlation",
                 ylab = "ARI",
                 xlab = "Variable Count",
                 lty = 1,
                 lwd = 2,
                 col = c("blue", "red", "green"),
                 legend = F,
                 bty = 'n')

interaction.plot(x.factor = results_table$noise, 
                 trace.factor = results_table$correlation, 
                 response = results_table$ari,
                 trace.label = "Correlation",
                 ylab = "ARI",
                 xlab = "Noise",
                 lty = 1,
                 lwd = 2,
                 col = c("blue", "red", "green"),
                 bty = 'n')

interaction.plot(x.factor = results_table$variable_count, 
                 trace.factor = results_table$correlation, 
                 response = results_table$tuckers,
                 trace.label = "Correlation",
                 ylab = "Tucker's congruence",
                 xlab = "# Variables per modality",
                 lty = 1,
                 lwd = 2,
                 col = c("blue", "red", "green"),
                 legend = F,
                 bty = 'n')

interaction.plot(x.factor = results_table$noise, 
                 trace.factor = results_table$correlation, 
                 response = results_table$tuckers,
                 trace.label = "Correlation",
                 ylab = "Tucker's congruence",
                 xlab = "Noise",
                 lty = 1,
                 lwd = 2,
                 col = c("blue", "red", "green"),
                 bty = 'n')

# three way interaction
interaction.ABC.plot(response = ari,
                     x.factor = variable_count,
                     groups.factor = noise,
                     trace.factor = correlation,
                     data = results_table,
                     title = "",
                     ylab = "ARI",
                     xlab = "# Variables per modality",
                     key.title = "Noise")


