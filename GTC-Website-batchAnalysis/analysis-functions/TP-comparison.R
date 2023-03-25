TP_comparison <- function(table, type, colors = list(), font) {
  table <- table %>%
    rename(value = paste0("_", type, " Percentile"))
  plot <- 
    ggplot(table, aes(x = Group, y = value)) +
    geom_boxplot(aes(group = Group)) +
    geom_point(aes(color = Group)) +
    labs(x = "\nGroup", y = "Trajectory Percentile", title = "Trajectory Percentile by Group") +
    scale_y_continuous(limits = c(0, 100), breaks = 10* c(0:10)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black", family = font),
          axis.title.x = element_text(size = 12, face = "bold", family = font),
          axis.title.y = element_text(size = 12, face = "bold", family = font),
          strip.background = element_rect(fill = colors$color3),
          strip.text = element_text(size = 14, color = "white", family = font),
          plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"),
          legend.position = "none")
  
  if (length(unique(table$Group)) == 1) {
    pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>Only 1 group.</em></span>")
    method = ""
  } else if (length(unique(table$Group)) == 2) {
    dataNumber <- 
      table %>% group_by(Group) %>%
      summarise(n=n())
    if (all(dataNumber$n > 1)) {
      pvalue = t.test(value ~ Group, table)$p.value
      method = "Student's t test"
    } else {
      pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method = ""
    }
  } else if (length(unique(table$Group)) > 2) {
    dataNumber <- 
      table %>% group_by(Group) %>%
      summarise(n=n())
    if (all(dataNumber$n > 1)) {
      pvalue = oneway.test(value ~ Group, table)$p.value
      method = "ANOVA"
    } else {
      pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method = ""
    }
  }
  
  summary <- table %>%
    group_by(Group) %>%
    summarise(
      `Number of infants` = n(),
      `mean ± sd` = paste0(round(digits = 1, mean(value, na.rm = T)), " ± ", round(digits = 1, sd(value, na.rm = T))),
      `median (IQR)` = paste0(round(digits = 1, median(value, na.rm = T)), " (", paste0(collapse = ", ", round(digits = 1, quantile(value, probs = c(0.25, 0.75), na.rm = T))) , ")"),
    ) %>% column_to_rownames(var = "Group") %>%
    t(.) %>% as.data.frame(.) %>%
    rownames_to_column(var = "Summary") %>%
    gt::gt() %>%
    tab_source_note(HTML(paste0("P value<sup>1</sup>: ", ifelse(is.na(as.numeric(pvalue)), pvalue, round(digits = 3, pvalue))))) %>%
    tab_source_note(HTML(paste0("<sup>1</sup>Method: ", method))) %>%
    tab_header("Trajectory Percentile Summary by Group") %>%
    tab_options(
      table.font.size = 12
    )

  return(list(summary,plot))
}