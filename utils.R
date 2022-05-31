clean_video <- function(str) {
  str %>%
    # str_remove_all('http[s?]://youtu\\.be/') %>%
    str_remove_all('http[s?]://') %>%
    str_replace_all(fixed('_'),'\\_')
}

# rho -> X(ρ) -> $X_\rho$
# (i) -> $X_i$
# P1(t+pi/2) -> P1(t+\pi/2)

clean_title <- function(str) {
  str %>%
    str_replace_all(",([^ ])",", \\1") %>%
    str_replace_all(fixed("w/"),"with") %>%
    str_replace_all(fixed("X(ρ)"),"$X_\\rho$") %>%
    str_replace_all(fixed("ρ"),"$\\rho$") %>%
    str_replace_all(fixed("X(i)"),"$X_i$") %>%
    str_replace_all(fixed("pi/2"),"$\\pi/2$") %>%
    str_replace_all(fixed("&"),"\\&") %>%
    str_replace_all("X\\((\\d+?)\\)","$X_{\\1}$") %>%
    str_replace_all("X'\\((\\d+?)\\)","$X'_{\\1}$") %>%
    str_replace_all("X(\\d+)","$X_{\\1}$")
  # str_replace_all("N=(\\d+?)","$N=\\1$")
}

make_bibtex_entry <- function(df) {
  df_str <- df %>% mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. \\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_bibtex <- function(df,key) {
  glue("\\section{{{clean_title(key)} ({nrow(df)})}\n\n{make_bibtex_entry(df)}\n\n")
}

