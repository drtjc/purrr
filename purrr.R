library(purrr)
library(magrittr)
library(rlang)




# ACCUMULATE
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








# AS_MAPPER
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
# functions used mostly as is
f <- as_mapper(mean)
f # gives UseMethod("mean")
f(3:5)

# but
f <- as_mapper(`+`)
f # givesfunction (.x, .y) if (missing(.y)) .x else .x + .y

f <- as_mapper(`*`)
f


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




# ARRAY BRANCH
# array branch
x <- array(1:24, c(4, 3, 2))
x

# A full margin for such an array would be the vector 1:3. This is
# the default if you don't specify a margin
# Creating a branch along the full margin is equivalent to
# as.list(array) and produces a list of size length(x):
array_branch(x) %>% str()

# A branch along the first dimension yields a list of length 2
# with each element containing a 3x2 array:
array_branch(x, 1) %>% str()

# A branch along the first and third dimensions yields a list of
# length 4x2 whose elements contain a vector of length 3:
array_branch(x, c(1, 3)) %>% str()


# ARRAY TREE
# array tree
x <- array(1:24, c(4, 3, 2))
x
# Creating a tree from the full margin creates a list of lists of
# lists:
array_tree(x) %>% str()
# The ordering and the depth of the tree are controlled by the
# margin argument:
array_tree(x, c(3, 1)) %>% str()
array_tree(x, c(1, 3)) %>% str()




# AS_VECTOR
# as_vector
v <- list(1:2, 3:4, 5:6) %>% as_vector(integer(2))
v






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



## KEEP, DISCARD, COMPACT
# keep(.x, .p, ...)
rep(10, 10) %>%
  map(sample, 5) %>%
  keep(function(x) mean(x) > 6)

rep(10, 10) %>%
  map(sample, 5) %>%
  keep(~ mean(.) > 6)


# Using a string instead of a function will select all list elements
# where that subelement is TRUE
x <- rerun(5, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")
x %>% discard("a")


# compact is a handy wrapper that removes all elements that are NULL or list().
v <- c(1, NULL, list(), list(NULL), 2)
compact(v)



## COMPOSE
# compose(...)
# ... n functions to apply in order from right to left
not_null <- compose (`!`, is.null)
not_null
not_null(4)
not_null(NULL)


f <- compose(sin, cos)
f(0)
sin(cos(0))

l <- list(sin, cos)
f <- compose(l) # doesn't work


add1 <- function(x) x + 1
compose(add1, add1)(8)




## CROSS
# cross(.l, .filter = NULL)
data <- list(
  id = c("John", "Jane"),
  greeting = c("Hello.", "Bonjour."),
  sep = c("! ", "... ")
)

data %>%
  cross() %>%
  map(lift(paste))

data %>% cross()
data %>% cross_df()
data %>% cross_df() %>% as.list()
data %>% cross_df() %>% as.list() %>% transpose()
identical(data %>% cross(), data %>% cross_df() %>% as.list() %>% transpose()) #TRUE

args <- data %>% cross_df()

out <- vector("list", length = nrow(args))
for (i in seq_along(out))
  out[[i]] <- map(args, i) %>% invoke(paste, .)
out


str(cross2(1:3, 1:3))
filter <- function(x, y) x >= y
cross2(1:3, 1:3, .filter = filter) %>% str()


seq_len(3) %>%
  cross2(., ., .filter = `==`) %>%
  map(setNames, c("x", "y"))

seq_len(3) %>%
  list(x = ., y = .) %>%
  cross(.filter = `==`)


seq_len(3) %>%
  cross3(., ., .) 
  



## DETECT
# Find the value or position of the first match.
# detect(.x, .f, ..., .right = FALSE, .p)
# detect_index(.x, .f, ..., .right = FALSE, .p)
is_even <- function(x) x %% 2 == 0

3:10 %>% detect(is_even)
3:10 %>% detect_index(is_even)

3:10 %>% detect(is_even, .right = TRUE)
3:10 %>% detect_index(is_even, .right = TRUE)

x <- list(
  list(1, foo = FALSE),
  list(2, foo = TRUE),
  list(3, foo = TRUE)
)

detect(x, "foo")
detect_index(x, "foo")




## EVERY, SOME
## Do every or some elements of a list satisfy a predicate?
## every(.x, .p, ...)
## some(.x, .p, ...)
x <- list(0, 1, TRUE)
x %>% every(identity)
x %>% some(identity)

y <- list(0:10, 5.5)
y %>% every(is.numeric)
y %>% every(is.integer)

x <- list(
  list(1, foo = FALSE),
  list(2, foo = TRUE),
  list(3, foo = TRUE)
)
x %>% every("foo")
x %>% some("foo")


## FLATTEN
x <- rerun(2, sample(4))
x
str(x)
x %>% purrr::flatten()
x %>% rlang::flatten()


x %>% purrr::flatten_int()
x %>% rlang::flatten_int()

# You can use flatten in conjunction with map
x %>% map(1L) %>% flatten_int()
# But it's more efficient to use the typed map instead.
x %>% map_int(1L)



## HAS_ELEMENT
# Does a list contain an object?
## has_element(.x, .y)
x <- list(1:10, 5, 9.9)
x %>% has_element(1:10)
x %>% has_element(3)


## HEAD_WHILE
## TAIL_WHILE
# head_while(.x, .p, ...)
# tail_while(.x, .p, ...)
pos <- function(x) x >= 0
head_while(5:-5, pos)
tail_while(5:-5, negate(pos))

big <- function(x) x > 100
head_while(0:110, big)
tail_while(0:110, big)





## LIFT
## lift
# lift_xy() is a composition helper. It helps you compose functions by lifting their 
# domain from a kind of input to another kind. The domain can be changed from and to 
# a list (l), a vector (v) and dots (d). For example, lift_ld(fun) transforms a 
# function taking a list to a function taking dots.

# lift is alias for lift_dl

# lift_dl
# takes function that takes mulitple argument (maybe dots), and makes it accept a list
x <- list(x = c(1:100, NA, 1000), na.rm = TRUE, trim = 0.9)
x
mean(x)
lift_dl(mean)(x)
View(environment(lift_dl(mean)))

x <- list(x = c(1:100, NA, 1000), trim = 0.9)
lift_dl(mean)(x) # NA
lift_dl(mean)(x, na.rm = TRUE)
lift_dl(mean, na.rm = TRUE)(x)


l <- list(sin, cos)
lift_dl(compose)(l)(0)


# lift_dl() and lift_ld() are inverse of each other
# Here we transform sum() so that it takes a list
fun <- sum %>% lift_dl()
fun(list(3, NA, 4, na.rm = TRUE))

# Now we transform it back to a variadic function
fun2 <- fun %>% lift_ld()
fun2
fun2(3, NA, 4, na.rm = TRUE)



lifted_identical <- lift_dl(identical, .unnamed = TRUE)
mtcars[c(1, 1)] %>% lifted_identical()
mtcars[c(1, 2)] %>% lifted_identical()

lifted_identical <- lift_dl(identical, .unnamed = FALSE)
mtcars[c(1, 1)] %>% lifted_identical() # error, thinks there are args called mpg and mpg.q




## LIST ALONG, REP_ALONG
## list_along(x)
## rep_along(x, y)
x <- 1:5
rep_along(x, 1:2)
rep_along(x, 1)
list_along(x)
seq_along(1:5)



# LIST_MODIFY, LIST_MERGE, UPDATE_LIST
# list_modify() and list_merge() recursively combine two lists, matching elements either by name or position. 
# If an sub-element is present in both lists list_modify() takes the value from y, 
# and list_merge() concatenates the values together.
x <- list(x = 1:10, y = 4, z = list(a = 1, b = 2))
str(x)
str(list_modify(x, a = 1))
str(list_modify(x, z = 5))
str(list_modify(x, z = NULL))

str(list_merge(x, x = 11, z = list(a = 2:5, c = 3, b = 99)))

l <- list(new = 1, y = NULL, z = 5)
str(list_modify(x, !!! l))

# In update_list() you can also use quosures and formulas to
# compute new values. This function is likely to be deprecated in
# the future
update_list(x, z1 = ~z[[1]])
update_list(x, z = rlang::quo(x + y))


## MODIFY
# modify() is a short-cut for x[] <- map(x, .f); return(x)
# modify_if() only modifies the elements of x that satisfy a predicate and leaves the others unchanged.
# modify_at() only modifies elements given by names or positions. 
# modify_depth() only modifies elements at a given level of a nested data structure.



## NEGATE
x <- c(y = TRUE, z = FALSE)
negate("y")(x)
negate("z")(x)
negate(1)(x)

negate(is.null)(NULL)

negate(~ .x > 0)(4, 5)
negate(~ .x > 0)(-4, 5)





## PLUCK
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












tt <- rerun(2, sample(10, 5), sample(10, 3))
tt



f <- function(x) {
  y <- 20
  if (x > 5) {
    stop("!")
  } else {
    x
  }
}

if (interactive()) {
  map(1:6, f)
}


if (interactive()) {
  map(1:6, auto_browse(f))
}

auto_browse(f)


rep(10, 10) %>% map(sample, 5) %>% keep(function(x) mean(x) > 6)

x <- rerun(5, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")




