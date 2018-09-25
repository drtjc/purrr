library(purrr)
library(magrittr)
library(rlang)

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



# as_mapper
# FORMULA
f <- as_mapper(~print(.y))
f # gives function (..., .x = ..1, .y = ..2, . = ..1) print(.y)
f(3, 4, 5)

f <- as_mapper(~print(..3))
f(3, 4, 5)

f <- as_mapper(~print(list2(...)))
vars <- list(x = 1, y = 2)
name <- "z"
f(w = 0, !!!vars, !!name := 3)


# FUNCTION
# functions used as is
f <- as_mapper(mean)
f
f(3:5)

# CHARACTER VECTOR, NUMERIC VECTOR, LIST
# conveted to extractor function
f <- as_mapper(list(1, 2))
f # give pluck(x, list(1, 2), .default = NULL), which is same as x[[1]][[2]]
l <- list(a = list(u = 1, v = 2), b = list(x = 3, y = 4))
f(l) # gives 2

f <- as_mapper(1:2)
f
f(l) # gives 2

f <- as_mapper(c("a", "v"))
f(l) # gives 2





# pluck
obj1 <- list("a", list(1, elt = "foobar"))
obj2 <- list("b", list(2, elt = "foobaz"))
x <- list(obj1, obj2)
x

# And now an accessor for these complex data structures:
my_element <- function(x) x[[2]]$elt

# The accessor can then be passed to pluck:
pluck(x, 1, my_element)
pluck(x, 2, my_element)



# This technique is used for plucking into attributes with
# attr_getter(). It takes an attribute name and returns a function
# to access the attribute:
obj1 <- structure("obj", obj_attr = "foo")
obj2 <- structure("obj", obj_attr = "bar")
x <- list(obj1, obj2)

# pluck() is handy for extracting deeply into a data structure.
# Here we'll first extract by position, then by attribute:
pluck(x, 1, attr_getter("obj_attr"))  # From first object
pluck(x, 2, attr_getter("obj_attr"))  # From second object


# pluck() splices lists of arguments automatically. The following
# pluck is equivalent to the one above:
idx <- list(1, attr_getter("obj_attr"))
pluck(x, idx)



v <- list(1:2, 3:4, 5:6) %>% as_vector(integer(2))





# accumulate
# accumulate applies a function recursively over a list from the left, while accumulate_right applies
# the function from the right. Unlike reduce both functions keep the intermediate results.


# accumulate(.x, .f, ..., .init)
# accumulate_right(.x, .f, ..., .init)

1:3 %>% accumulate(`+`)
1:10 %>% accumulate(max, .init = 5)
c(5,1:10) %>% accumulate(max)


1:10 %>% accumulate(~ .x)
1:10 %>% accumulate(~ .y)



1:10 %>% reduce(~ .x)
1:10 %>% reduce(~ .y)


ff <- as_mapper(~ .x)
ff


gg <- as_mapper(~ .y)
gg
gg(1, 2)
gg(2, 3)

hh <- as_mapper(~ x ^2)
hh

tt <- function(...) {
  print(..1)
}
tt(3,4,5)




tt <- rerun(2, sample(10, 5), sample(10, 3))
tt


as_mapper(list(1, attr_getter("a")))

