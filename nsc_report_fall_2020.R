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

graduated_students_or_colleges <- function(df, graduated, wanted) {
  if (rapportools::is.boolean(graduated)) {
    ifelse(graduated, df <- df %>% filter(Graduated. == 'Y'), df <- df %>% filter(Graduated. == 'N'))
    ifelse(wanted == 'students', return(df), return(get_count(df)))
  }
  stop('Graduated must be boolean')
}

df_graduated_colleges <- graduated_students_or_colleges(df, TRUE, 'colleges')

df_not_graduated_colleges <- graduated_students_or_colleges(df, FALSE, 'colleges')

df_total_colleges <- get_count(df)

df_graduated_stus <- graduated_students_or_colleges(df, TRUE, 'students')

df_did_not_graduated_stus <- graduated_students_or_colleges(df, FALSE, 'students')