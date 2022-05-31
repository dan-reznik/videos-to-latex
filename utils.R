clean_video <- function(str) {
  str %>%
    str_remove_all('http[s?]://youtu\\.be/') %>%
    str_replace_all(fixed('_'),'\\_')
}

make_bibtex_entry <- function(df) {
  df_str <- df %>% mutate(str=glue("\\item \\textit{{{Title}}}, {Year}. youtu.be/\\href{{{Video1}}}{{\\nolinkurl{{{clean_video(Video1)}}}}}"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_bibtex <- function(df,key) {
  glue("\\section{{{key} ({nrow(df)})}\n\n{make_bibtex_entry(df)}\n\n")
}