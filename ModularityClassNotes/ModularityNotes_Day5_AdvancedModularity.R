# Day 5: Advanced Modularity Notes
# Michelle Kelly

# Review: 6 lessons of modularity ----
#     can comment these into homework e.g. "I used lesson 2 here"

# 1. Do not type constants into your code, give them variable names.
# 2. Write extensible code.
#     Do your best to anticipate what you will need down the line
# 3. Rewriting/revising your code can be good and important, and should not depress you.
# 4. Define your modules and interfaces very clearly and precisely in advance.
#     think about task in advance, break into pieces, write pseudocode before you write the code,
#     decide the inputs and the outputs, write them into your code
# 5. Resist the temptation to just jump in and code for all but the simplest coding tasks.
# 6. Copied code is a sign that modularity could help.


# Review: advantages of modules ----
#     seperating code into modules means you can test them seperately for bugs
#     no copies of code




# 7. Write pseudocode ----

# Small scale modules: use of commented blocks of code

  # <Function name>: <short description of what function does>
  # <input variable>: <variable class>, <variable description>
  # Output: <`return` of function, including "side effects" like plots>
  function_name <- function(function_inputs){
    # write your pseudocode here before you start writing the code
    return(output)
  }


  
# 8. Be aware of the discrete purpose of each Rmd chunk ----

# Medium scale modules: use of Rmd chunks
#   Make sure to define "dependencies" of each chunk on the one before
  
<!--Chunk is for whatever purpose--> # this is the format for comments in Rmd
```{r echo = T}
# dependencies = none

# required packages
  library(packages)
  
# any other required prep
  # fill this in here
  
# new variables created: none, packages loaded
```

# Caching
#   if you've loaded the data in another chunk, you can use caching to communicate vairables across chunks
<!--Chunk for cleaning the data-->
```{r echo=F, cache=T, cache.extra=list(dat)}
#Dependencies: dat

  #fill in 

#New variables: dat has been modified
```
  
    


# 9. Link external scripts to your Rmd (if your chunk is super long) using source() ----

# High level modularity: Tree format
#       stem: the Rmd, which is your paper
#             branches: supplemental scripts that the stem calls upon


# 10. If you have a common set of functions that you use across projects - load them with source() ----

# This allows you to only have to update the one file, don't have to remember that you've changed x script
```{r}
source("MyCommonlyUsedFunctions_Set1.R")  
```
#   you can also build yourself a package



# In the homework: you'll have to flag at least 5 of these lessons of modularity