# Assignment 2 by Marc (Fork of cachematrix) 
# https://github.com/rdpeng/ProgrammingAssignment2
## Creating a function creating a matrix and then a second function that
# inverses that function
# Create a FUN that creates a matrix

# side note - The operators <<- and ->> are normally only used in functions,
# and cause a search to made through parent environments for an existing
# definition of the variable being assigned. If such a variable is found
# (and its binding is not locked) then its value is redefined, otherwise 
# assignment takes place in the global environment. See 'The R Language 
# Definition' manual for further details
# and examples.

# I would like Khan Academy for help :)
# https://www.khanacademy.org/math/precalculus/precalc-matrices/
# inverting_matrices/v/matrices-to-solve-a-system-of-equations

# Start of function
makeCacheMatrix <- function(x = numeric()) {
    # set the variable m (matrix) to zero
    m <- NULL
    # start second function
    set <- function(y) {
        # use the 'super' variable' to assign first function to second function
        # y is free variable
        ## <<- makes the variable search through the parent variable (scoping)
        x <<- y
        # set the variable m (matrix) to zero
        m <<- NULL
    }
    get <- function() x
    # make the m (matrix) with the  
    setm <- function(solve)
        ## <<- makes the variable search through the parent variable m (scoping)
        m <<- solve
    # creates the function
    getm <- function() m
    # makes a list of the matrixes solving completing the FUN
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

# This FUN will return a inversen of x (a matrix)
# start the function
cacheSolve <- function(x = matrix(), ...) {
    # will get getmatrix for the FUN makeCacheMatrix
    m <- x$getm()
    # start a loop with if
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # took forever to get this right
    # THought it should have just recieved and not assigned to the function variable matrix
    matrix <- x$get()
    # puts the finally inversion of x (Now matrix)
    m <- solve(matrix, ...)
    # prints the output of the function
    m
}
