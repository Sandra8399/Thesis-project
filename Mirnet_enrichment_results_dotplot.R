library(ggplot2)
mirnet_results <- read.csv("mirnet_enrichment.csv")
head(mirnet_results)


# Prepare the data (top 10 by FDR)
top10 <- mirnet_results[order(mirnet_results$FDR), ][1:10, ]

# Plot using ggplot2
ggplot(top10, aes(x = -log10(FDR), y = reorder(Pathway, FDR), size = Hits, color = FDR)) +
  geom_point() +
  labs(
    title = "Dotplot ORA",
    x = "-log10(FDR)",
    y = "Pathway"
  ) +
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold.italic"))
