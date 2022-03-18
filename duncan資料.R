library("car")      
dim(Duncan) # rows and columns
DATA <- Duncan  # the whole data set
summary(Duncan) 
scatterplotMatrix(~ prestige + income + education, id=list(n=3),
                  smooth=list(span=0.7), data=Duncan)
names(DATA)[1] <- "類型"
names(DATA)[2] <- "收入"
names(DATA)[3] <- "教育程度"
names(DATA)[4] <- "聲望"
data.table::setDT(DATA)
rio::export(DATA,"occupational rankings.xlsx")
