mcnemar <- function(preds1, preds2){
  if(length(preds1) != length(preds2)){
    stop("The number of predictions should be equal.")
  }
  
  data <- data.frame(model1 = preds1,
                     model2 = preds2)
  b <- data %>% filter(model1 == "pos", model2 == "neg") %>% nrow()
  c <- data %>% filter(model1 == "neg", model2 == "pos") %>% nrow()
  
  test_stat <- (b - c)^2/(b+c)
  p_val <- pchisq(test_stat, df = 1)
  
  return(list(Test_statistic = test_stat, p_value = p_val))
}

# For the example, make sure that you have constructed the objects 
# "lr_preds" and "ctree_preds" from the .Rmd

# Let's first have a look at the number of cases on which both models
# have the same prediction.
sum(lr_preds == ctree_preds)
# The LR and the tree predict 133 cases equally. This is quite large:
# there are only 154 cases in the test set. Probably the models will
# be considered equally by McNemar.

mcnemar(lr_preds, ctree_preds)
# As suggested, the p-value is quite large and there is not a 
# significant difference in performance of the two models.