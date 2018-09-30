library(purrr)
library(magrittr)
library(rlang)
library(tibble)



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




# AS_VECTOR, SIMPLFY, SIMPLYFY_ALL
# as_vector
# as_vector() collapses a list of vectors into one vector
v <- list(1:2, 3:4, 5:6) %>% as_vector(integer(2))
v
# as_vector and simplfy call unlist()
# if list can't be simplified, as_vector returns an error, simplify return the original list
# simplify_all cal simplify on each element of list






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




## MAP
# The map functions transform their input by applying a function to each element and returning a vector the same length as the input.
# map(), map_if() and map_at() always return a list.
# The _if and _at variants take a predicate function .p that determines which elements of .x are transformed with .f.
# map_lgl(), map_int(), map_dbl() and map_chr() return vectors of the corresponding type (or die trying).
# map_dfr() and map_dfc() return data frames created by row-binding and column-binding respectively.
# walk() calls .f for its side-effect and returns the input .x.
1:10 %>%
  map(rnorm, n = 10) %>%
  map_dbl(mean)

# Or use an anonymous function
1:10 %>%
  map(function(x) rnorm(10, x))

1:10 %>%
  map(~ rnorm(10, .x))


# .default specifies value for elements that are missing or NULL
l1 <- list(list(a = 1L), list(a = NULL, b = 2L), list(b = 3L))
l1
l1 %>% map("a", .default = "???")
l1[[1]][["a"]]
l1[[2]][["a"]]
l1[[3]][["a"]]
l1 %>% map_int("b", .default = NA)


# Supply multiple values to index deeply into a list
l2 <- list(
  list(num = 1:3,     letters[1:3]),
  list(num = 101:103, letters[4:6]),
  list()
)
l2
l2 %>% map(c(2, 2))

# Use a list to build an extractor that mixes numeric indices and names,
# and .default to provide a default value if the element does not exist
l2 %>% map(list("num", 3))
l2 %>% map_int(list("num", 3), .default = NA)

is_even <- function(x) x %% 2 == 0
v <- 1:5
map_int(v, `+`, 1L)
map_at(v, c(1,4), `+`, 1L) %>% flatten_int()
map_if(v, is_even, `+`, 1L) %>% flatten_int()


mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map_dfr(~ as.data.frame(t(as.matrix(coef(.)))))



## LMAP. LMAP_IF, LMAP_AT
maybe_rep <- function(x) {
  n <- rpois(1, 2)
  out <- rep_len(x, n)
  if (length(out) > 0) {
    names(out) <- paste0(names(x), seq_len(n))
  }
  out
}
x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])
x %>% lmap(maybe_rep)


## MAP2, PMAP
x <- list(1, 10, 100)
y <- list(1, 2, 3)
z <- list(5, 50, 500)

pmap(list(x, y, z), ~lift_vd(mean)(...))



pluck(list(x, y, z), list(1,3))

pmap(list(x, y, z), sum)
l <- data.frame(x = 1:3, y = 4:6, z = 7:9)
lmap(l, `*`, 2)

# Matching arguments by position
pmap(list(x, y, z), function(a, b ,c) a / (b + c))

# Matching arguments by name
l <- list(a = x, b = y, c = z)
pmap(l, function(c, b, a) a / (b + c))

df <- data.frame(
  x = c("apple", "banana", "cherry"),
  pattern = c("p", "n", "h"),
  replacement = c("x", "f", "q"),
  stringsAsFactors = FALSE
)
pmap(df, gsub)
pmap_chr(df, gsub)


## Use `...` to absorb unused components of input list .l
df <- data.frame(
  x = 1:3 + 0.1,
  y = 3:1 - 0.1,
  z = letters[1:3]
)
plus <- function(x, y) x + y
## Not run: 
## this won't work
pmap(df, plus)

## End(Not run)
## but this will
plus2 <- function(x, y, ...) x + y
pmap_dbl(df, plus2)







## MODIFY
# modify() is a short-cut for x[] <- map(x, .f); return(x)
# modify_if() only modifies the elements of x that satisfy a predicate and leaves the others unchanged.
# modify_at() only modifies elements given by names or positions. 
# modify_depth() only modifies elements at a given level of a nested data structure.
l1 <- list(
  obj1 = list(
    prop1 = list(param1 = 1:2, param2 = 3:4),
    prop2 = list(param1 = 5:6, param2 = 7:8)
  ),
  obj2 = list(
    prop1 = list(param1 = 9:10, param2 = 11:12),
    prop2 = list(param1 = 12:14, param2 = 15:17)
  )
)
l1 %>% modify_depth(3, sum) %>% str()
l1 %>% purrr::modify(c("prop1", "param2")) %>% str()
l1 %>% modify_depth(2, "param2") %>% str()

map(l1, c("prop1", "param2")) %>% str()
map(l1, ~ map(., "param2")) %>% str()



## NEGATE
x <- c(y = TRUE, z = FALSE)
negate("y")(x)
negate("z")(x)
negate(1)(x)

negate(is.null)(NULL)

negate(~ .x > 0)(4, 5)
negate(~ .x > 0)(-4, 5)



## NULL DEFAULT
# This infix function makes it easy to replace NULLs with a default value.
1 %||% 2
NULL %||% 2



## PARTIAL
# Partial function application allows you to modify a function by pre-filling some of the arguments. 
# It is particularly useful in conjunction with functionals and other function operators.
# Partial is designed to replace the use of anonymous functions for
# filling in function arguments. Instead of:
compact1 <- function(x) discard(x, is.null)

# we can write:
compact2 <- partial(discard, .p = is.null)

# and the generated source code is very similar to what we made by hand
compact1
compact2


# Note that the evaluation occurs "lazily" so that arguments will be
# repeatedly evaluated
f <- partial(runif, n = rpois(1, 5))
f
f()
f()

# You can override this by saying .lazy = FALSE
f <- partial(runif, n = rpois(1, 5), .lazy = FALSE)
f
f()
f()


# This also means that partial works fine with functions that do
# non-standard evaluation
my_long_variable <- 1:10
plot2 <- partial(plot, my_long_variable)
plot2()
plot2(runif(10), type = "l")






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




## SAFELY, QUIETLY, POSSIBLY
# safely: wrapped function instead returns a list with components result and error. One value is always NULL.
# safely(.f, otherwise = NULL, quiet = TRUE)
safe_log <- safely(log)
safe_log(10)
safe_log("a")

str(list("a", 10, 100) %>%
  map(safe_log) %>%
  transpose())

safe_log <- safely(log, otherwise = NA_real_)
str(list("a", 10, 100) %>%
  map(safe_log) %>%
  transpose() %>%
  simplify_all())


# quietly: wrapped function instead returns a list with components result, output, messages and warnings.
f <- function(x) message(x)
quiet_f <- quietly(f)
quiet_f(10)
quiet_f("a")




# possibly: wrapped function uses a default value (otherwise) whenever an error occurs.
# possibly(.f, otherwise, quiet = TRUE)
str(list("a", 10, 100) %>%
  map_dbl(possibly(log, NA_real_)))

str(list("a", 10, 100) %>%
      map_dbl(possibly(log, NA_real_, FALSE)))



# PREPEND
# This is a companion to append() to help merging two lists or atomic vectors. 
# prepend() is a clearer semantic signal than c() that a vector is to be merged 
# at the beginning of another, especially in a pipe chain.
# prepend(x, values, before = 1)
x <- as.list(1:3)

x %>% append("a")
x %>% prepend("a")
x %>% prepend(list("a", "b"), before = 3)

append(1:5, 0:1, after = 3)


# RERUN
10 %>% rerun(rnorm(5))
10 %>% rerun(x = rnorm(5), y = rnorm(5))
10 %>% rerun(x = rnorm(5)) # second level list not dropped


## SPLICE
inputs <- list(arg1 = "a", arg2 = "b")

# splice() concatenates the elements of inputs with arg3
purrr::splice(inputs, arg3 = c("c1", "c2")) %>% str()
list(inputs, arg3 = c("c1", "c2")) %>% str()
c(inputs, arg3 = c("c1", "c2")) %>% str()


## TRANSPOSE
# Transpose turns a list-of-lists "inside-out"; it turns a pair of lists into a list of pairs, or a list of pairs into pair of lists.
# transpose(.l, .names = NULL)
x <- rerun(5, x = runif(1), y = runif(5))
x %>% str()
x %>% transpose() %>% str()

ll <- list(
  list(x = 1, y = "one"),
  list(z = "deux", x = 2)
)
ll %>% transpose()

# Provide explicit component names to prevent loss of those that don't
# appear in first component
nms <- ll %>% map(names) %>% reduce(union)
nms
ll %>% transpose(.names = nms)


## VEC_DEPTH
x <- list(
  list(),
  list(list()),
  list(list(list(1)))
)
vec_depth(x)
str(x)
x[[3]][[1]][[1]][[1]]

x %>% map_int(vec_depth)



## WHEN
# When a valid match/condition is found the action is executed and the result of the action is returned.
## when(., ...)
# .	the value to match against
# ...	formulas; each containing a condition as LHS and an action as RHS. named arguments will define additional values.
1:10 %>%
  when(
    sum(.) <=  50 ~ sum(.),
    sum(.) <= 100 ~ sum(.)/2,
    ~ 0
  )

1:10 %>%
  when(
    sum(.) <=   x ~ sum(.),
    sum(.) <= 2*x ~ sum(.)/2,
    ~ 0,
    x = 60
  )


1:10 %>%
  when(
    sum(.) <=  50 ~ sum(.),
    sum(.) <= 51 ~ sum(.)/2,
    ~ 0
  )


iris %>%
  subset(Sepal.Length > 10) %>%
  when(
    nrow(.) > 0 ~ .,
    ~ iris %>% head(10)
  )

iris %>%
  head %>%
  when(nrow(.) < 3 ~ .,
       ~ stop("Expected fewer than 3 rows."))



## ATTRIBUTE ACCESSOR
factor(1:3) %@% "levels"
mtcars %@% "class"
`%@%`








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




