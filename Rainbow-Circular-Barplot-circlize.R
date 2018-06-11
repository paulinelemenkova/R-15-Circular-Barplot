# Круговая диаграмма пакетом tidyverse // Circular Barplot 

# ЧАСТЬ-1. готовим датафрейм. 
	# шаг-1. вчитываем таблицу с данными. делаем из нее исходный датафрейм. чистим датафрейм от NA
MDepths <- read.csv("Morphology.csv", header=TRUE, sep = ",")
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(MDF)

library(tidyverse)
	# шаг-2 создаем короткий датафрейм всего из 3 значений (value - длина лепестка круга)
data<- data.frame(
	id = MDF$profile, # номера как факторное значение, т.е. просто перечисление, здесь 1:25
	individual = paste("Profile", seq(1,25), sep=""),
	value = MDF$sedim_thick) # value - длина лепестка круга


Category <- c(paste("Profile", seq(1,25), sep="")) 
Percent <- MDF$slope_angle
#color = add_transparency(rainbow(length(Percent)), 0.1)
color = rainbow(length(Percent))

Category = rev(Category) 
Percent = rev(Percent) 
color = rev(color)

library(circlize)

par(mar = c(1, 1, 1, 1))
circos.par("start.degree" = 90)
circos.initialize("a", xlim = c(0, 100)) # 'a` just means there is one sector
circos.trackPlotRegion(ylim = c(0.5, length(Percent)+0.5), track.height = 0.8, 
    bg.border = NA, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim") # in fact, it is c(0, 100)
    for(i in seq_along(Percent)) {
        circos.lines(xlim, c(i, i), col = "#CCCCCC")
        circos.rect(0, i - 0.45, Percent[i], i + 0.45, col = color[i], 
            border = "white")
        circos.text(xlim[2], i, paste0(Category[i], " - ", Percent[i], "%"), 
            adj = c(1, 0.5), cex = .65) 
    }
})
circos.clear()
text(0, 0, "Mariana Trench \nSlope Angles \nby Profiles 1:25", col = "#CCCCCC", cex = .75)