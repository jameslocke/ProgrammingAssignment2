## Put comments here that give an overall description of what your
## functions do

# Two functions included:
# 1. makecacheMatrix() - Create and return a list() of functions that are then callable
#                        using the dataelement passed (basematrix) and the function (set,get,setCache,getCache).
# 2. cacheSolve() - Solve for the inverse of a matrix. 
#                   Requires makecacheMatrix() function to be called first to create
#                   the list of functions called from here

## Write a short comment describing this function - see comments in function for complete description


makeCacheMatrix <- function(baseMatrix = matrix()) {
      #--------------------------------------------------------------------------------------
      #
      # James Locke
      # 10.24.2015
      # makecacheMatrix() - Create and return a list() of functions that are then callable
      #                       using the dataelement passed (basematrix) and the function (set,get,setCache,getCache).
      #                     Designed to work with numeric matrices.
      #     baseMatrix    - the base data to be used for the analysis.
      #     cacheMatrix   - the results of the analysis that are cached.
      #
      #
      # Sample Usage Examples (tested):
      #     > a <- matrix(c(3,1,-2,-2,0,3,0,1,0),3,3)
      #     > x <- makeCacheMatrix(a)
      #     > cacheSolve(x)
      #           [,1] [,2] [,3]
      #     [1,]  0.6    0  0.4
      #     [2,]  0.4    0  0.6
      #     [3,] -0.6    1 -0.4
      #     > cacheSolve(x)
      #     Getting cached data
      #           [,1] [,2] [,3]
      #     [1,]  0.6    0  0.4
      #     [2,]  0.4    0  0.6
      #     [3,] -0.6    1 -0.4
      #
      # Usage showing the use of the $set function 
      #     > x$set(a)
      #     > cacheSolve(x)
      #     New set of base data found
      #     [,1] [,2] [,3]
      #     [1,]  0.6    0  0.4
      #     [3,] -0.6    1 -0.4
      #--------------------------------------------------------------------------------------
      
      
      #------------------------------------------------------
      # Initialize common variables
      #------------------------------------------------------
      # create our cached matrix variable in the "makecacheMatrix" environment
      # this will be referenced from within children environment using <<-
      cacheMatrix <- NULL
      
      #------------------------------------------------------
      # set() - sets the data in the parent environment
      #------------------------------------------------------
      # Update these in the calling environement (parent) which is the function.
      #
      # Note: This $set function is used to reset the data using variable$set(new data matrix) in the global environment
      # Example: if your call was x <- makeCacheMatrix(myData), you could use x$set(myData) to reload the data
      #
      # other than re-running the makeCacheMatrix function, this is the only other way to set new data
      #
      set <- function(sMatrix) {
            baseMatrix <<- sMatrix        
            cacheMatrix <<- NULL
            
            # added this to track when matrix was used for caching (not seen as a requirement)
            # null or not-stamped indicated this is an new set of data with no timestamp
            # new data, clear our timestamp, although it should be NULL anyway
            attr(baseMatrix,"timestamp") <<- NULL
      }
      
      #------------------------------------------------------
      # get() - gets the value of the matrix to be be used 
      #         as a base for other functions
      #------------------------------------------------------
      get <- function() {
            baseMatrix
      }
      
      #------------------------------------------------------
      # setCache() - takes the matrix passed and stores it
      #              in the cached matrix variable
      #------------------------------------------------------
      setCache <- function(cMatrix) {
            cacheMatrix <<- cMatrix
            
            # only time we stamp our base data with a time stamp is when we used it for our cache
            # that way, if our base data changes, and we cache new results, we should know it
            # someone could get it 100 times and never cache it, which doesnt matter to us. 
            attr(baseMatrix,"timestamp") <<- Sys.time()
            
      }
      
      #------------------------------------------------------
      # getCache() - returns the cached matrix.
      #              this will return NULL if not cached yet
      #------------------------------------------------------
      getCache <- function() {
            
            # null or not-stamped indicated this is an new set of data with no timestamp
            # the actual value of the timestamp is just interesting information for this purpose
            timechk <- attr(baseMatrix,"timestamp")
            if (is.null(timechk)) {
                  # message("New set of base data found")
                  cacheMatrix <- NULL
                  return(NULL)                  
            } else {
                  ## Print the timestame of last cache if we want to. not going to for graded exercise
                  # cat("Timestamp of last cache ")
                  # print(timechk)
            }
            
            # if we get here, return our matrix, it could be NULL if no data was cached during previous results
            return(cacheMatrix)
            
      }
      
      
      #------------------------------------------------------
      # list - this is our return value which is a list of 
      #        function calls    
      #------------------------------------------------------
      return(list(set = set,
                  get = get,
                  setCache = setCache,
                  getCache = getCache)
      )
}



## Write a short comment describing this function - see comments in function for complete description

cacheSolve <- function(baseMatrix, ...) {
      #--------------------------------------------------------------------------------------
      #
      # James Locke
      # 10.24.2015
      # cacheSolve() - Solve for the inverse of a matrix. 
      #                 Requires makecacheMatrix() function to be called first to create
      #                 the list of functions called from here
      #                 Cache the results and re-use since solving for inverses is costly for large data sets
      #   baseMatrix - the matrix to be analyzed and solved for the inverse
      #     ...      - allows other parameters to be passed
      #
      #--------------------------------------------------------------------------------------
      
      
      #----------------------------------------------------
      # Check to see if we have a cached matrix already
      # If we do then let us know and return it
      #----------------------------------------------------
      inverseMatrix <- baseMatrix$getCache()
      if(!is.null(inverseMatrix)) {
            message("Getting cached data")
            return(inverseMatrix)
      }
      
      
      #-----------------------------------------------------------------
      # If there is not a cached version, we will need to solve for it
      # Check first to see if it is invertible using
      # the determinant function (not a requirement but a nice to have)
      #-----------------------------------------------------------------
      dataToInvert <- baseMatrix$get()
      if (det(dataToInvert)==0) {
            message("Data is not invertible, returning NA")
            return(NA)
      }
      
      #-----------------------------------------------------
      # Solve and call the function to store in the cache
      #-----------------------------------------------------
      inverseMatrix <- solve(dataToInvert)
      baseMatrix$setCache(inverseMatrix)
      
      #-----------------------------------------------------
      # Return our inverse matrix to the caller
      #-----------------------------------------------------
      return(inverseMatrix)
}
