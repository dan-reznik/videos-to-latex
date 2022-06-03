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
    str_replace_all(fixed("Δ"),"$\\Delta$") %>%
    str_replace_all(fixed("δ"),"$\\delta$") %>%
    str_replace_all(fixed("X(i)"),"$X_i$") %>%
    str_replace_all(fixed("pi/2"),"$\\pi/2$") %>%
    str_replace_all(fixed("&"),"\\&") %>%
    str_replace_all("X\\((\\d+?)\\)","$X_{\\1}$") %>%
    str_replace_all("X'\\((\\d+?)\\)","$X'_{\\1}$") %>%
    str_replace_all("X(\\d+)","$X_{\\1}$") %>%
    str_replace_all('"(.+?)"',"``\\1''")
  # str_replace_all("N=(\\d+?)","$N=\\1$")
}


make_enum_entry_long <- function(df) {
  df_str <- df %>%
    # mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. \\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
    mutate(str=str_glue("\\item \\textbf{{{clean_title(Title)}}}, {Year}. ",
                    "\\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}} ",
                    "{descr}\n"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_enum_entry <- function(df) {
  df_str <- df %>%
    # mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. \\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
    mutate(str=str_glue("\\item \\textbf{{{clean_title(Title)}}} ({fmt_dur_str(duration)}), {month(date)}/{year(date)}. ",
                    "\\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  str_glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_center_video <- function(fname,Video1) {
  str_glue("\n\n\\begin{{center}}",
           "\\includegraphics[width=.5\\textwidth]{{{fname}}} \\\\ \n",
           "\\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}",
           "\\end{{center}}\n",)
}

make_enum_entry_video <- function(df) {
  df_str <- df %>%
    # mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. \\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
    mutate(
      video_fname = str_glue("pics/{video_id}.jpg"),
      str=str_glue("\\item \\textbf{{{clean_title(Title)}}} ",
                   "({fmt_dur_str(duration)}, N={N}), {month(date)}/{year(date)}. ",
                   "{make_center_video(video_fname,Video1)}\n",
                   "% \\input{{descr/{sprintf('%03d',id)}_{video_id}}}"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  str_glue("\\begin{{enumerate}}[resume]\n{df_coll}\n\\end{{enumerate}}\n")
}

make_subsec_entry_video <- function(df) {
  df_str <- df %>%
    # mutate(str=glue("\\item \\textit{{{clean_title(Title)}}}, {Year}. \\href{{{Video1}}}{{\\url{{{clean_video(Video1)}}}}}"))
    mutate(
      video_fname = str_glue("pics/{video_id}.jpg"),
      str=str_glue("\\subsection{{{clean_title(Title)}}}\n",
                   "\\label{{vid:{video_id}}}\n",
                   "\\noindent N={N}, {fmt_dur_str(duration)} ({month(date)}/{year(date)}). ",
                   "{make_center_video(video_fname,Video1)}\n",
                   "% \\input{{descr/{sprintf('%03d',id)}_{video_id}}}\n\n"))
  df_coll <- df_str$str %>% str_c(collapse="\n")
  df_coll
}

make_enum <- function(df,key) {
  str_glue("\\section{{{clean_title(key)} ({nrow(df)})}\n\n{make_enum_entry(df)}\n\n")
}

make_enum_video <- function(df,key) {
  str_glue("\\section{{{clean_title(key)} ({nrow(df)})}\n\n{make_enum_entry_video(df)}\n\n")
}

make_subsec_video <- function(df,key) {
  str_glue("\\section{{{clean_title(key)} ({nrow(df)})}\n\n{make_subsec_entry_video(df)}\n\n")
}

