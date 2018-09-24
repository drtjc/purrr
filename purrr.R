library(purrr)
library(magrittr)

# reduce
# reduce() combines from the left, reduce_right() combines from the right. reduce(list(x1, x2, x3), f)
# is equivalent to f(f(x1, x2), x3); reduce_right(list(x1, x2, x3), f) is equivalent to
# f(f(x3, x2), x1)

# reduce(.x, .f, ..., .init)
# reduce_right(.x, .f, ..., .init)
# reduce2(.x, .y, .f, ..., .init)
# reduce2_right(.x, .y, .f, ..., .init)

1:3 %>% reduce(`+`)
`+`(`+`(1,2), 3)
1:3 %>% reduce(`+`, .init = 4)


1:10 %>% reduce(`*`)
factorial(10)

x <- list(c(0, 1), c(2, 3), c(4, 5))
x
x %>% reduce(c)
x %>% reduce_right(c) 
# Equivalent to"
x %>% rev() %>% reduce(c)

paste2 <- function(x, y, sep = ".") paste(x, y, sep = sep)
paste2("A", "B")
letters[1:4] %>% reduce(paste2)

# For reduce2(), a 3-argument function. The function will be passed the accumulated
# value as the first argument, the next value of .x as the second argument,
# and the next value of .y as the third argument.
# For reduce2(), an additional argument that is passed to .f. If init is not set,
# .y should be 1 element shorter than .x.
letters[1:4] %>% reduce2(c("-", ".", "-"), paste2)


samples <- rerun(2, sample(10, 5))
samples

reduce(samples, union)
# same as
union(samples[[1]], samples[[2]]) 

reduce(samples, intersect)
# same as
intersect(samples[[1]], samples[[2]]) 


# but what if more samples
samples <- rerun(10, sample(10, 8))
samples
reduce(samples, union)
reduce(samples, intersect)



# functional programning
Funcall <- function(f, ...) f(...)
## Compute log(exp(acos(cos(0))))
Reduce(Funcall, list(log, exp, acos, cos), 5, right = TRUE)
## n-fold iterate of a function, functional style:
Iterate <- function(f, n = 1) function(x) Reduce(Funcall, rep.int(list(f), n), x, right = TRUE)
## Continued fraction approximation to the golden ratio:
Iterate(function(x) 1 + 1 / x, 30)(1)

reduce_right(list(log, exp, acos, cos), Funcall, .init = 5)


reduce_right(list(cos), Funcall, .init = 5)

Funcall(cos, 5)


Funcall(cos, 5)



Funcall(cos, 5)

cos(5)
Reduce(Funcall, list(cos), 5, right = TRUE)


Funcall(acos, Funcall(cos, 5))




# accumulate
# accumulate applies a function recursively over a list from the left, while accumulate_right applies
# the function from the right. Unlike reduce both functions keep the intermediate results.


# accumulate(.x, .f, ..., .init)
# accumulate_right(.x, .f, ..., .init)





tt <- rerun(2, sample(10, 5), sample(10, 3))
tt
