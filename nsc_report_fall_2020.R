library(dplyr)

file <- 'NSC/NSC_PA_Report_for_Fall_2020.csv'

df <- read.csv(file)

get_count <- function(df) {
  df <- df$College.Name %>% as.character() %>% as.data.frame()
  colnames(df) <- c('College')
  df <- df %>% count(College)
  colnames(df)[colnames(df) == 'n'] <- 'Total'
  return(df %>% arrange(desc(df$Total)))
}

graduated_or_not <- function(df, graduated) {
  ifelse(graduated, df <- df %>% filter(Graduated. == 'Y'), df <- df %>% filter(Graduated. == 'N'))
  return(get_count(df))
}

graduated_df <- function(df, graduated) {
  ifelse(graduated, df <- df %>% filter(Graduated. == 'Y'), df <- df %>% filter(Graduated. == 'N'))
  return(df)
}

df_graduated_colleges <- graduated_or_not(df,TRUE)

df_not_graduated_colleges <- graduated_or_not(df, FALSE)

df_total_colleges <- get_count(df)

df_graduated_stus <- graduated_df(df, TRUE)

df_did_not_graduated_stus <- graduated_df(df, FALSE)