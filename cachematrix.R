

#below are sample commands to run the function
#mt <- matrix(c(2,3,2,2),2,2)
#source("cachematrix.R")
#cacheM <- makeCacheMatrix(mt)
#ls(parent.env(cacheM$get.env()))
#parent.env(cacheM$get.env())$mx
#parent.env(cacheM$get.env())$invt.mx
#cacheSolve(cacheM)
#parent.env(cacheM$get.env())$invt.mx

#makeCacheMatrix: This function stores a matrix and its inverse. 
#The inverse matric is not computed within the function but has to calculated from
#outside and set.It is assumed that matrix is invertible one.
#ignore function get.env, it is for debugging purpose
makeCacheMatrix <- function(mx=matrix()){ 
	#inverse matrix is initialized to null
    invt.mx <- NULL 
    
    #get() returns the matrix used in initialization
    get <- function() mx 
	
	#get.invt.mx() returns the inverse matrix if it exists else NULL
    get.invt.mx <- function() invt.mx
    
    #set.invt.mx() sets the inverse matrix
    set.invt.mx <- function(invt.m=matrix()) invt.mx <<- invt.m
    
    #get.env() used for understading the environment, not of use
    get.env  <- function() environment()
    
    #returns list of functions
    list(get=get,get.invt.mx=get.invt.mx,set.invt.mx=set.invt.mx,get.env=get.env)    
}

#cacheSolve: This function takes makeCacheMatrix function as its parameter. It 
#computes inverse mtrix and sets it if not already set else it fetches it from cache
cacheSolve <- function(cacheM,...){
	
	#check inverse matrix exist or not
    if(is.null(cacheM$get.invt.mx())){
        print("Inverse not found, setting inverse")
        cacheM$set.invt.mx(solve(cacheM$get()))
    }else{
        print("Inverse fetched from memory")
    }
    cacheM$get.invt.mx()
}