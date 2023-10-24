pacman::p_load(ggstance)

lmer_coefs <- function(lmer_obj){
  confint(lmer_obj) %>% 
    as_tibble() %>% 
    bind_cols(
      tibble(para = c(".sig01",".sigma",names(fixef(lmer_obj))))
    ) %>% 
    bind_cols(tibble(estimate = c(NA, NA, fixef(lmer_obj)))) %>% 
    na.omit() %>% 
    filter(para != "(Intercept)") %>% 
    mutate(
      sig = `2.5 %` > 0 | `97.5 %` < 0
    ) %>% 
    ggplot(aes(x = estimate, y = para, colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
               linewidth = 1.5, alpha = 0.7) +
    geom_linerangeh(aes(xmin = `2.5 %`, xmax = `97.5 %`), size = 1.2) +
    geom_point(shape = 21, fill = "white", size = 3.5) +
    theme_bw() +
    drop_y_gridlines() +
    labs(x = "Estimate", y = NULL, colour = "Significant") +
    theme(legend.position = "top") +
    scale_colour_manual(values = c("grey60", "black")) 
}

