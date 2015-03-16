# r-statistics
Statistical tasks and homeworks in R (http://www.r-project.org/)

Репозиторий для задач и домашек на R

Краткий гайд по работе с репозиторием: http://course-bd-and-pa-bsuir.github.io/r-statistics/
Шпаргалки/конспекты: https://drive.google.com/folderview?id=0BwJrICCSFUGYNDBBc3kySXNLNkE&usp=drive_web

Предлагаемый Workflow:

1. Каждый заводит отдельную папку со своим именем в основной ветке (master)
2. Вся работа ведётся в основной ветке, можно свободно делать commit, push и pull в master, но изменения делать в своей папке.

Таким образом, мастер ветка будет иметь все результаты в удобном для просмотра и ревью виде.
Те, кто испытывают затруднения с Git, могут использовать SVN в той же манере (т.е. коммитить в trunk): https://help.github.com/articles/support-for-subversion-clients/

Текущий список пакетов для установки в R (спасибо @programmerby):

```
install.packages(c(
                "acepack", "car","colorspace","compareGroups","dichromat",
                "digest","ellipse","epitools","Formula","gdata",
                "ggplot2","gtable","gtools","haplo.stats","HardyWeinberg",
                "Hmisc","labeling","latticeExtra","lme4","manipulate",
                "mice","minqa","mnormt","moments","munsell","mvtnorm",
                "nloptr","pastecs","pbkrtest","plyr","proto","psych",
                "quantreg","randomForest","RColorBrewer","Rcpp","RcppEigen",
                "reshape2","scales","SNPassoc","SparseM","stringr","vcd","xtable",
                "lmtest", "nortest", "corrgram", "stats", "car", "gplots", 
                "scatterplot3d", "Amelia", "BaBoon", "cat", "deducorrect", "e1071", 
				"ForImp", "Hmisc", "imputation", "impute", "mi", "mice", "mix", 
				"norm", "robCompositions", "rrcovNA", "StatMatch", "VIM", 
				"yaImpute", "zoo", "outliers" ))
```