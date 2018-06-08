library(tidyverse)
set.seed(20180417)

fns <- c(sin,cos,function(x) cos(x)^2, function(x) sin(x)^2)

plt <- function(fn){
  x <- seq(1,10,0.1)
  plot(x,fn(x))
}

plt(fns[[4]])


test <- tibble(xx = runif(10),
               yy = c(4,rep(1:3,3)))
test
col_names <- c("yy", "zz")

# test_fn <- function(new_col){
#   print(!! new_col)
#   test %>% 
#     mutate(new_col = 1)
# }

# 
# test_fn(quo("y"))
xx <- 10
test_df_fn <- function(df){
  mutate(df, yy = .data$xx)
}

test_df_fn(test)




test3 <- function(blob){
  quo_blob <- enquo(blob)
  test %>% 
    group_by(!! quo_blob) %>% 
    summarise(sum_ = sum(xx))
}

test3(yy)

