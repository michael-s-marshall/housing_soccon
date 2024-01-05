pacman::p_load(ggstance)

lmer_coefs <- function(lmer_obj, chosen_method, var_names){
  set.seed(123)
  confint(lmer_obj, method = chosen_method) %>% 
    as_tibble() %>% 
    bind_cols(
      tibble(term = c(".sig01",".sig02",".sigma",names(fixef(lmer_obj))))
    ) %>% 
    bind_cols(tibble(estimate = c(NA, NA, NA, fixef(lmer_obj)))) %>% 
    na.omit() %>% 
    filter(term != "(Intercept)") %>%
    left_join(var_names, by = "term") %>% 
    mutate(
      sig = `2.5 %` > 0 | `97.5 %` < 0,
      sig = ifelse(sig == T, "Significant","Non-significant"),
      sig = fct_rev(as.factor(sig))
    ) %>% 
    ggplot(aes(x = estimate, y = fct_rev(var_name), colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
               linewidth = 1.5, alpha = 0.7) +
    geom_linerangeh(aes(xmin = `2.5 %`, xmax = `97.5 %`), size = 1.2) +
    geom_point(shape = 21, fill = "white", size = 3.5) +
    theme_minimal() +
    drop_y_gridlines() +
    labs(x = "Estimate", y = NULL, colour = NULL) +
    facet_wrap(~grouping, ncol = 1, scales = "free_y") +
    theme(legend.position = "top",
          strip.text = element_text(face = "bold")) +
    scale_colour_manual(values = c("black","grey65")) 
}
