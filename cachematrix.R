#makeCacheMatrix: This function stores a matrix and its inverse. 
#The inverse matric is not computed within the function but has to calculated from
#outside and set.It is assumed that matrix is invertible one.
#ignore function get.env, it is for debugging purpose

#cacheSolve: This function takes makeCacheMatrix function as its parameter. It 
#computes inverse mtrix and sets it if not already set else it fetches it from cache

#below are sample commands to run the function
#mt <- matrix(c(2,3,2,2),2,2)
#source("cachematrix.R")
#cacheM <- makeCacheMatrix(mt)
#ls(parent.env(cacheM$get.env()))
#parent.env(cacheM$get.env())$mx
#parent.env(cacheM$get.env())$invt.mx
#cacheSolve(cacheM)
#parent.env(cacheM$get.env())$invt.mx

makeCacheMatrix <- function(mx=matrix()){ 
    invt.mx <- NULL 
    
    get <- function() mx 

    get.invt.mx <- function() invt.mx
    
    set.invt.mx <- function(invt.m=matrix()) invt.mx <<- invt.m
      
    get.env  <- function() environment()
    
    list(get=get,get.invt.mx=get.invt.mx,set=set,get.env=get.env)    
}

cacheSolve <- function(cacheM,...){
    if(is.null(cacheM$get.invt.mx())){
        print("Inverse not found, setting inverse")
        cacheM$set(solve(cacheM$get()))
    }else{
        print("Inverse fetched from memory")
    }
    cacheM$get.invt.mx()
}