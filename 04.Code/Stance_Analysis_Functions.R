graph_stance <- function(input = "02.Intermediate/CleanedComments.rds",
                         strength_graph = "05.Figures/stance_strength_graph.pdf",
                         bar_graph = "05.Figures/stance_bar_graph.pdf",
                         strength_unique = "05.Figures/stance_strength_unique.pdf",
                         bar_unique = "05.Figures/stance_bar_unique.pdf"){
  comments <- readRDS(input)
  comments <- comments[!is.na(Approve_Score),]
  
  ggplot(comments, aes(Approve_Score)) + 
    geom_histogram() +  
    labs(x = "Probability Approve", y = "Number of Observations") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10500))
  
  ggsave(strength_graph, units = "in", width = 5, height = 3)
  
  comments[, Stance := factor(x = Stance, levels = c("approves", "disapproves"),
                              labels = c("Approves of Merger", "Disapproves of Merger"))]
  
  ggplot(comments, aes(Stance)) +
    geom_bar() + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 10500))
  
  ggsave(bar_graph, units = "in", width = 5, height = 3)
  
  # Now, Unique Comments Only
  comments.unique <- unique(comments[,.(Comment, Stance, Approve_Score)])
  
  ggplot(comments.unique, aes(Approve_Score)) + 
    geom_histogram() +  
    labs(x = "Probability Approve", y = "Number of Observations") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 800))
  
  ggsave(strength_unique, units = "in", width = 5, height = 3)
  
  
  ggplot(comments.unique, aes(Stance)) +
    geom_bar() + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 800))
  
  ggsave(bar_unique, units = "in", width = 5, height = 3)
  
}

comment_submitted_graph <- function(input = "02.Intermediate/CleanedComments.rds",
                                   output = "05.Figures/stance_submission_timeline.pdf"){
  comment_data <- readRDS(input)
  
  # Remove Withdrawn Comment
  comment_data <- comment_data[Comment != "",]
  
  comment_data[, Day_Posted := pmin(as.numeric(Date - min(Date)),33)]
  comment_data[Stance == "approves of", Stance := "Approves of Merger"]
  comment_data[Stance == "disagrees with", Stance := "Disapproves of Merger"]
  
  ggplot(comment_data, aes(x = Day_Posted, fill = Stance)) +
    geom_histogram(binwidth = 1, center = 0.5) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + 
    labs(y = "Number of Observations", x = "Day Posted") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 4000)) +
    scale_fill_manual(values = c("black", "lightgrey"))
  
  ggsave(output, units = "in", width = 5, height = 3)
}

stance_summary_statistics <- function(input = "02.Intermediate/CleanedComments.rds",
                                      stance_table = "06.Tables/Stance_Taken_Summary.tex"){
  mean_sd <- function(var){
    mean <- signif(mean(var, na.rm = TRUE), digits = 2);
    sd <- paste("(", signif(sd(var, na.rm = TRUE), digits = 2), ")", sep = "")
    return(paste(mean, sd))
  }
  
  comment_data <- readRDS(input)
  comment_app <- comment_data[Stance == "approves",]
  comment_dis <- comment_data[Stance == "disapproves",]
  
  duplicate_less <- comment_data[!(duplicated(Comment) | duplicated(Comment,
                                                                    fromLast = TRUE)),]
  duplicate_less_app <- duplicate_less[Stance == "approves",]
  duplicate_less_dis <- duplicate_less[Stance == "disapproves",]
  
  # Column Names
  colNames <- c("Variable", "Approve", "Disapprove", "Approve", "Disapprove")
  
  # First - Information On Stance  
  stance.prob <- c("Stance Probability", mean_sd(comment_app$Approve_Score),
                   mean_sd(comment_dis$Disapprove_Score),
                   mean_sd(duplicate_less_app$Approve_Score),
                   mean_sd(duplicate_less_dis$Disapprove_Score))
  # Length
  length.min <- c("Min", min(nchar(comment_app$Comment)),
                  min(nchar(comment_dis$Comment)),
                  min(nchar(duplicate_less_app$Comment)),
                  min(nchar(duplicate_less_dis$Comment)))
  length.mean <- c("Mean (SD)", mean_sd(nchar(comment_app$Comment)),
                   mean_sd(nchar(comment_dis$Comment)),
                   mean_sd(nchar(duplicate_less_app$Comment)),
                   mean_sd(nchar(duplicate_less_dis$Comment)))
  length.max <- c("Max", max(nchar(comment_app$Comment)),
                  max(nchar(comment_dis$Comment)),
                  max(nchar(duplicate_less_app$Comment)),
                  max(nchar(duplicate_less_dis$Comment)))
  
  # Sentiment
  sent.pos <- c("Positive", sum(comment_app$Sentiment == "positive"),
                sum(comment_dis$Sentiment == "positive"),
                sum(duplicate_less_app$Sentiment == "positive"),
                sum(duplicate_less_dis$Sentiment == "positive"))
  sent.neg <- c("Negative", sum(comment_app$Sentiment == "negative"),
                sum(comment_dis$Sentiment == "negative"),
                sum(duplicate_less_app$Sentiment == "negative"),
                sum(duplicate_less_dis$Sentiment == "negative"))
  sent.neu <- c("Neutral", sum(comment_app$Sentiment == "neutral"),
                sum(comment_dis$Sentiment == "neutral"),
                sum(duplicate_less_app$Sentiment == "neutral"),
                sum(duplicate_less_dis$Sentiment == "neutral"))
  sent.maxP <- c("Sentiment Assigned Probability", mean_sd(comment_app$Sentiment_Score),
                 mean_sd(comment_dis$Sentiment_Score), 
                 mean_sd(duplicate_less_app$Sentiment_Score),
                 mean_sd(duplicate_less_dis$Sentiment_Score))
  
  # State Sent
  state.FL <- c("Florida", sum(comment_app$`State/Province` == "FL"),
               sum(comment_dis$`State/Province` == "FL"),
               sum(duplicate_less_app$`State/Province` == "FL"),
               sum(duplicate_less_dis$`State/Province` == "FL"))
  state.NY <- c("New York", sum(comment_app$`State/Province` == "NY"),
               sum(comment_dis$`State/Province` == "NY"),
               sum(duplicate_less_app$`State/Province` == "NY"),
               sum(duplicate_less_dis$`State/Province` == "NY"))
  state.MA <- c("Massachusetts", sum(comment_app$`State/Province` == "MA"),
               sum(comment_dis$`State/Province` == "MA"),
               sum(duplicate_less_app$`State/Province` == "MA"),
               sum(duplicate_less_dis$`State/Province` == "MA"))
  state.PR <- c("Puerto Rico", sum(comment_app$`State/Province` == "PR"),
               sum(comment_dis$`State/Province` == "PR"),
               sum(duplicate_less_app$`State/Province` == "PR"),
               sum(duplicate_less_dis$`State/Province` == "PR"))
  
  # Number of Observations
  numObs <- c("Number of Comments", nrow(comment_app),
              nrow(comment_dis),
              nrow(duplicate_less_app),
              nrow(duplicate_less_dis))

  out_tab <- rbind(stance.prob, sent.pos, sent.neu, sent.neg, sent.maxP,
                   length.min, length.mean, length.max,
                   state.FL, state.PR, state.NY, state.MA,
                   numObs)
  rownames(out_tab) <- NULL
  
  # Make Table
  kbl(out_tab, 
      format = "latex", col.names = colNames,
      escape = FALSE, booktabs = TRUE,
      keep_tex = TRUE) %>%
    add_header_above(c(" " = 1, "All Comments" = 2, "Unique Comments" = 2)) %>%
    row_spec(row = 12, hline_after = TRUE) %>% 
    pack_rows(group_label = "Sentiment", start_row = 2, end_row = 5) %>%
    pack_rows(group_label = "Comment Length", start_row = 6, end_row = 8) %>%
    pack_rows(group_label = "State Submitted From", start_row = 9, end_row = 12) %>%
    pack_rows(group_label = "Summary Statistics", start_row = 13, end_row = 13) %>%
    save_kable(file = stance_table)
}
