```{r appendix}
plot(lmer_reg_full, resid(., scaled=TRUE) ~ fitted(.) | away_team, abline = 0, xlab = "Fitted Values", ylab = "Scaled Residuals")
plot(lmer_reg_full, away_team ~ resid(., scaled=TRUE), xlab = "Scaled Residuals")

plot(lmer_reg_full2, resid(., scaled=TRUE) ~ fitted(.) | away_team, abline = 0, xlab = "Fitted Values", ylab = "Scaled Residuals", main = "fit.lmer2: \n Scaled Residuals vs. \n Fitted Values by Away Team")
plot(lmer_reg_full2, resid(., scaled=TRUE) ~ fitted(.) | Year, abline = 0, xlab = "Fitted Values", ylab = "Scaled Residuals", main = "fit.lmer2: \n Scaled Residuals vs. \n Fitted Values by Year")
plot(lmer_reg_full2, away_team ~ resid(., scaled=TRUE), xlab = "Scaled Residuals", main = "Away Team vs. \n Scaled Residual")
```