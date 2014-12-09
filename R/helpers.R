normalize <- function(vec)
{
    vec / sum(vec)
}

# a function for calculating the Jaccard index of two vectors
# it's assumed that the two vectors will be normalized to have a sum of 1
jaccard <- function(x1, x2)
{
    lesser.value <- function(x, y) {
        if (x <= y)
            x
        else
            y    
    }

    greater.value <- function(x, y) {
        if (x >= y)
            x
        else
            y
    }
        
    # calculate the amount of overlap (the intersection) between the two vectors
    eltwise.ints <- unlist(Map(lesser.value, x1, x2))
    intersection <- sum(eltwise.ints)

    # calculate the size of the union of the two vectors
    eltwise.unions <- unlist(Map(greater.value, x1, x2))
    union <- sum(eltwise.unions)

    intersection / union
}


