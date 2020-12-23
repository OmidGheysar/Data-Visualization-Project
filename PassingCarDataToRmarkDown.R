
data(mtcars)
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Manual", "Automatic"))
df <- mtcars


ivs <- c("cyl", "disp")
dvs <- c("mpg", "qsec")
sth <- 100
rmarkdown::render("paramPass.Rmd", 
                  params = list(df = df, ivs = ivs, dvs = dvs, sth = sth))