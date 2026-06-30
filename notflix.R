## Notflix: Pro-active Churn Management

# Always start by loading the data
library(radiant)
notflix <- readr::read_rds("data/notflix.rds")

# Calculate weights to use in estimation
notflix <- mutate(notflix, cweight = ifelse(churn == "yes", 1L, 29L))

# Show descriptive statistics in the training data

result <- explore(
  notflix, 
  vars = c(
    "churn", "change_movies", "delay", "lowest10", "scratched", 
    "missing", "moved", "avg_rating", "senior", "referred"
  ), 
  fun = c("mean", "sd", "min", "max"), 
  data_filter = "training == 1", 
  nr = Inf
)
summary(result)
# dtab(result)
stats <- result$tab

# Show churn in the representative sample

result <- explore(
  notflix, 
  vars = "churn", 
  fun = "mean",
  data_filter = "representative == 1", 
  nr = Inf
)
summary(result)
# dtab(result)

# Estimating a Logistic Regression with weights (unstandardized) 

result <- logistic(
  notflix, 
  rvar = "churn", 
  evar = c(
    "change_movies", "delay", "lowest10", "scratched", "missing", 
    "moved", "avg_rating", "senior", "referred"
  ), 
  lev = "yes", 
  wts = "cweight", 
  data_filter = "training == 1"
)
summary(result)
plot(result, custom = FALSE)

# Estimating a Logistic Regression with weights (standardized) 

result_main <- logistic(
  notflix, 
  rvar = "churn", 
  evar = c(
    "change_movies", "delay", "lowest10", "scratched", "missing", 
    "moved", "avg_rating", "senior", "referred"
  ), 
  lev = "yes", 
  wts = "cweight", 
  check = "standardize", 
  data_filter = "training == 1"
)
summary(result_main)
plot(result_main, custom = FALSE)
pred <- predict(result_main, pred_data = notflix)
notflix <- store(notflix, pred, name = "p_base")

# To get the table with variable importance, you can click on the download icon on the top-right of the screen in _Model > Logistic Regression > Summary_. You can also use the `write.coeff` function to get the same information. 

## just print information
write.coeff(result, sort = TRUE)

## store information in a csv file
write.coeff(result, file = "~/Desktop/notflix-variable-importance.csv")

# Making some (temporary) changes to the data to shown the 'flipped' coefficient/OR plot that captures variable importance

notflix_flipped <- mutate(
  notflix, 
  change_movies = -change_movies, 
  avg_rating = -avg_rating,
  senior = ifelse(senior == "yes", "no", "yes")
)

result <- logistic(
  notflix_flipped, 
  rvar = "churn", 
  evar = c(
    "change_movies", "delay", "lowest10", "scratched", "missing", 
    "moved", "avg_rating", "senior", "referred"
  ), 
  lev = "yes", 
  wts = "cweight", 
  check = "standardize", 
  data_filter = "training == 1"
)
plot(result, custom = FALSE)

## Quantify the impact of actions/offers/incentives on the probability of churn
# We can start by calculating churn numbers for those customers that experienced shipping delays in the representative sample. This will require us to use the logistic regression model estimated above (assigned to `result_main` to generate predictions **as if** we eliminated all shipping delays by setting the values in the `delay` column to 0. 

pred <- predict(result_main, pred_data = notflix, pred_cmd = "delay = 0")
print(pred, n = 10)
notflix <- store(notflix, pred, name = "p_delay0")

# In the table below we can see that the actually churn rate amongst those that experienced delays was 5.58%. Our logistic regression model predicts that removing shipping delays for these customers would reduce churn to 2.82% on average. 

result <- explore(
  notflix, 
  vars = c("churn", "p_delay0"), 
  fun = c("n_obs", "mean"),
  data_filter = "representative == 1 & delay > 0", 
  nr = Inf
)
summary(result)
# dtab(result, dec = 4)

# We can also calculate churn numbers for customers in the `lowest10` in the representative sample.

result <- pivotr(
  notflix, 
  cvars = "lowest10", 
  nvar = "churn", 
  data_filter = "representative == 1", 
  nr = Inf
)
summary(result)
# dtab(result)

# What would be a reasonable churn number if we could convert people that are in the 4-at-a-time plan but are only watching, 3.1 DVDs per month on average? One option would be to assume that the new churn rate for these customers would be 3.33% (i.e., the average churn rate for 4-at-a-time customers). Of course, you could also provide an argument that 3.1%, i.e., the average churn amongst customers that are **not** in the lowest 10% of plan usage (see table above).

# We could, of course, also estimate the predicted probability of churn if we eliminate the effect of being in the lowest 10% of plan usage by setting `lowest10 = "no"` for everyone. Is likely to be as reasonable a prediction as we got for the `delay` variable above? Probably not because, while every customer has probably experienced multiple periods without any shipping delays, not customers in this data have actually received an offer to downgrade to a lower tier plan. At best, setting `lowest10 = "no"` during prediction will give us a **very** rough approximation of what might happen to churn. The best way to establish the effects of a "downgrade" promotion would be to run an experiment.

# That said, lets see what would happen if we run a prediction were we set `lowest10 = "no"` for all.

pred <- predict(result_main, pred_data = notflix, pred_cmd = "lowest10 = 'no'")
print(pred, n = 10)
notflix <- store(notflix, pred, name = "p_downgrade")

# Should we consider offering the plan-downgrade to all customers with usage in the lowest 10% for the 4-at-a-time plan? We could but that but, as a demo, lets consider what the results would look like if we decided to only offer the plan-downgrade to customers in the lowest 10% of usage for the plan that also have an above average overall predicted churn probability. This might eliminate seniors and those with high `avg_ratings` from consideration.

# To do this we need to calculate predicted churn for all customers (see `p_base` generated using the main logistic regression model above). We can then use a filter (i.e., `p_base > 0.033`) to ensure we are using the correct subset of data for our calculations. As you can see below, our predicted churn rate after downgrading the low-usage customers is 2.82%. 

# Note that in the CLV calculations shown in the class slides I used 3.33% as a more conservative estimates of the post-downgrade churn level. CLV calculation using either the 3.33% assumption or average churn predictions from the logistic regression model suggest that a tier down-grade could work. It therefor seems reasonable to suggest an experiment be setup to determine if the drop in customer churn for these customers is indeed sufficiently high to offset the loss in revenue after a plan downgrade (i.e., $23.99 to $17.99)

byvar = "representative"
result <- explore(
  notflix, 
  vars = c("churn", "p_downgrade"), 
  fun = c("n_obs", "mean"), 
  data_filter = "representative == 1 & p_base > 0.033 & lowest10 == 'yes'", 
  nr = Inf
)
summary(result)
# dtab(result, dec = 4)

