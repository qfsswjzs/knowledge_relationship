# Title     : test.R
# Author    : wujie
# Time      : 2021/4/1


    ## load package required
library(flare)
    ## generating data
    n = 50
    d = 50
    D = sugm.generator(n=n,d=d,graph="band",g=1)
    plot(D)
    ## sparse precision matrix estimation with method "clime"
    out1 = sugm(D$data, method = "clime")
    plot(out1)
    sugm.plot(out1$path[[4]])