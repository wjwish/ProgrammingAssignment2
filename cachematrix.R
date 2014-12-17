## 以下函数对用于缓存逆矩阵。在重复计算逆矩阵时，如果矩阵内容未做更改，先缓存逆矩阵，
## 再次需要时则可从缓存查找，无需重复计算，这样可以节省系统计算时间。


## 这个函数是用于创建可缓存逆矩阵的特殊“矩阵”对象

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                      
        set <- function(y) {            #设置矩阵值
                x <<- y                 #在其他环境中给x赋值
                m <<- NULL              #在其他环境中，设置m初始值为NULL
        }
        get <- function() x             #获取矩阵值
        setsolve <- function(solve) m <<- solve         #设置逆矩阵
        getsolve <- function() m                        #获取逆矩阵
        list(set = set, get = get,      #返回列表
             setsolve = setsolve,
             getsolve = getsolve)
}



## 这个函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵
## 如果已经计算逆矩阵，则将检索缓存中的逆矩阵，否则计算逆矩阵

cacheSolve <- function(x, ...) {
        m <- x$getsolve()               #将获得的x的逆矩阵返回给m
        if(!is.null(m)) {               #判断缓存里m是否为空
                message("getting cached data")          #如果缓存里有值，先返回一段文字
                return(m)               #再返回缓存里的m值，即x的逆矩阵
        }
        data <- x$get()                 #如果缓存里没有值，把矩阵赋值给data
        m <- solve(data, ...)           #计算逆矩阵，并赋值给m
        x$setsolve(m)                   
        m                               #返回逆矩阵
}
