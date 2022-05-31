clean_video <- function(str) {
  str %>%
    str_remove_all('http[s?]://youtu\\.be/') %>%
    str_replace_all(fixed('_'),'\\_')
}

clean_title <- function(str) {
  str %>%
    str_replace_all(fixed("&"),"\\&") %>%
    str_replace_all("X\\((\\d+?)\\)","$X_{\\1}$") %>%
    str_replace_all("X(\\d+)","$X_{\\1}$")
  # str_replace_all("N=(\\d+?)","$N=\\1$")
}

make_bibtex_entry <- function(df) {
  df_str <- df %>% mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. youtu.be/\\href{{{Video1}}}{{\\nolinkurl{{{clean_video(Video1)}}}}}"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_bibtex <- function(df,key) {
  glue("\\section{{{clean_title(key)} ({nrow(df)})}\n\n{make_bibtex_entry(df)}\n\n")
}

