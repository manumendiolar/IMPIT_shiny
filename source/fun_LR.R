fun_LR <- function(yy, xx, main.title, y.lab, x.lab, yrs.lab, 
                   period, region, lag, var_resp, var_pred,
                   species){
  p <- NULL
  out <- NULL
  
  # linear model
  mod <- lm(yy ~ xx)
  CI90 <- confint(mod, "xx", level = 0.90)
  CI95 <- confint(mod, "xx", level = 0.95)
  cor.out95 <- cor.test(yy, xx, alternative = "two.sided", conf.level = 0.95, method = "pearson")
  cor.out90 <- cor.test(yy, xx, alternative = "two.sided", conf.level = 0.90, method = "pearson")
  
  # data frame with results
  out <- data.frame(species, 
                    var_resp, 
                    var_pred, 
                    region, 
                    period, 
                    lag,
                    slope = as.numeric(coef(mod)[2]), 
                    lb90 = CI90[ ,1], 
                    ub90 = CI90[ ,2], 
                    lb95 = CI95[ ,1], 
                    ub95 = CI95[ ,2],
                    R2 = as.numeric(summary(mod)$r.squared), 
                    pVal = as.numeric(anova(mod)$'Pr(>F)'[1]), 
                    corr = as.numeric(cor.out95$estimate), 
                    corr.lb90 = as.numeric(cor.out90$conf.int[1]),
                    corr.ub90 = as.numeric(cor.out90$conf.int[2]),
                    corr.lb95 = as.numeric(cor.out95$conf.int[1]),
                    corr.ub95 = as.numeric(cor.out95$conf.int[2]))
  
  # plot
  colreg <- ifelse( out$pVal > 0.10, "black", ifelse( out$pVal > 0.05, "magenta", "green"))
  colreg2 <- ifelse( out$pVal > 0.10, "black", ifelse( out$pVal > 0.05, "magenta", "green4"))
  subtitle <- ifelse(round(out$pVal,3) < 0.001,
                     paste0("Corr = ", round(out$corr,2),", Rsq = ",round(out$R2,2),", p value < 0.001"),
                     paste0("Corr = ", round(out$corr,2),", Rsq = ",round(out$R2,2),", p value = ",round(out$pVal,3))
                     )
  p <- ggplot(data = data.frame(yrs.lab, xx, yy), aes(x = xx, y = yy)) +
    geom_smooth(method ='lm', se = TRUE, col = colreg, alpha = 0.25) +
    geom_text(aes(label = yrs.lab), size = 3.5) +
    labs(y = y.lab, x = x.lab, title = main.title, subtitle = subtitle) +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(size = 9, face = 2),
          plot.subtitle = element_text(size = 9, face = 2, colour = colreg2))
  
  return(list(out = out, p = p))
  
}
