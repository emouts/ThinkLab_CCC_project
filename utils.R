responses_reformat <- function(df){
  question_header <- "Please.mark.how.much.you.agree.or.disagree.with.the.following.statements..."
  answers <- c("Strongly Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Strongly Agree")
  n_questions <- paste0(c(rep(1, 6), rep(2, 7)), ".", c(1:6, 1:7))
  n_answers <- nrow(df)
  
  df %>%
    select(starts_with("Please.mark")) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "question", values_to = "answer") %>%
    mutate(question = gsub("\\.", " ", sub("..$", "", sub(question_header, "", question))),
           num = match(answer, answers) - 3,
           answer = factor(answer, levels = answers)) %>%
    mutate(short = rep(n_questions, n_answers), .after = question) %>%
    mutate(id = rep(1:n_answers, each = 13), .before = 1)
}

wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

histogram_individual <- function(df, questions = "1.1"){
  cols <- RColorBrewer::brewer.pal(5, "RdYlGn")
  answers <- c("Strongly Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Strongly Agree")
  
  df <- filter(df, short %in% questions)
  present_answers <- answers %in% df$answer
  
  ggplot(df) +
    theme_minimal() +
    geom_bar(aes(x = answer, fill = answer)) +
    ggtitle(wrapper(df$question[1], width = 60)) +
    xlab("") +
    ylab("Number of answers") +
    ylim(0, nrow(df)) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = cols[present_answers]) +
    theme(axis.text.x = element_blank())
}

histogram_faceted <- function(df){
  cols <- RColorBrewer::brewer.pal(5, "RdYlGn")
  answers <- c("Strongly Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Strongly Agree")
  
  present_answers <- answers %in% df$answer
  
  ggplot(df) +
    theme_minimal() +
    geom_bar(aes(x = answer, fill = answer)) +
    ylim(0, nrow(df) / 13) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(values = cols[present_answers]) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    facet_wrap(~ short, ncol = 3)
}

pair_plot <- function(df, pair = c("1.1", "1.2")){
  cols <- RColorBrewer::brewer.pal(9, "RdYlGn")
  cols[5] <- "cornflowerblue"
  answers <- c("-2" = "Strongly Disagree", "-1" = "Somewhat Disagree", "0" = "Neutral", 
               "1" = "Somewhat Agree", "2" = "Strongly Agree")
  
  
  df <- filter(df, short %in% pair)
  wide <- tidyr::pivot_wider(df, id_cols = id, names_from = short, values_from = num)
  colnames(wide) <- c("id", "q1", "q2")
  wide <- mutate(wide, total = as.factor(q1 + q2))
  present_answers <- (-4):4 %in% wide$total
  
  ggplot(wide) +
    theme_minimal() +
    geom_jitter(aes(x = q1, y = q2, colour = total), size = 5, show.legend = FALSE) +
    ggtitle(paste("Question", pair[1], "against", pair[2])) +
    xlab(wrapper(df$question[1], width = 100)) +
    ylab(wrapper(df$question[2], width = 50)) +
    xlim(-2.5, 2.5) +
    ylim(-2.5, 2.5) +
    scale_x_continuous(breaks = -2:2, labels = answers) +
    scale_y_continuous(breaks = -2:2, labels = answers) +
    scale_colour_manual(values = cols[present_answers]) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(angle = 45)) +
    geom_hline(yintercept = (-2.5):2.5) +
    geom_vline(xintercept = (-2.5):2.5)
}
