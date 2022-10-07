##########################################
###     -*- Coding: utf-8 -*-          ###
### Analyste: Charles-Édouard Giguère  ###
###                              .~    ###
###  _\\\\\_                    ~.~    ###
### |  ~ ~  |                 .~~.     ###
### #--O-O--#          ==||  ~~.||     ###
### |   L   |        //  ||_____||     ###
### |  \_/  |        \\  ||     ||     ###
###  \_____/           ==\\_____//     ###
##########################################


require(reticulate, quietly = TRUE, warn.conflicts = FALSE)
setwd("C:/Users/gigc2/Desktop/stepmixr/Examples/")
test <- import(module = "test")
py_run_string("for i in range(0,10): print(i)")
test$addone(2)
