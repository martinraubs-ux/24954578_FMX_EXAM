render_tex_table <- function(df, caption_txt, label_txt, header_above = NULL) {
    tbl <- df %>%
        kable(
            format   = "latex",
            booktabs = TRUE,
            caption  = paste0(caption_txt, " \\label{", label_txt, "}"),
            align    = c("l", rep("r", ncol(df) - 1)),
            linesep  = ""
        )

    if (!is.null(header_above)) {
        tbl <- tbl %>% add_header_above(header_above)
    }

    tbl %>%
        kable_styling(
            latex_options = c("HOLD_position", "scale_down"),
            font_size     = 9
        )
}