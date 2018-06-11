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

# ЧАСТЬ-2. создаем ярлычки-лейблы для каждого лепестка круга

	# шаг-3 из датафрейма берем информацию для ярлыков // Get the name and the y position of each label
label_data = data 
	# шаг-4 подсчитываем углы calculate the ANGLE of the labels 
	number_of_bar=nrow(label_data) 
	angle = 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
	# шаг-5. распределяем ярлычки справа-слева // calculate the alignment of labels: right or left/
# If I am on the left part of the plot, my labels have currently an angle < -90 
label_data$hjust<-ifelse(angle < -90, 1, 0)
	# шаг-6. переворачиваем ярлычки при переходе через 180 гр. // flip angle BY to make them readable 
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# ЧАСТЬ-3 теперь рисуем сам круг. 
	 
	# шаг-7. 
p <- ggplot(data, aes(x = as.factor(id), y = value, fill = id)) +
	geom_bar(stat = "identity", fill = alpha("blue", 0.3)) +
	ylim(-50,200) + # ylim задает 2 диаметра круга - внешний и внутренний. 
# здесь: ylim у меня 150, т.к. все значения value не превышают 200 (т.е. внешний круг) 
# значение -50 - диаметр внутреннего кружка. здесь у меня -50.
	theme_minimal() +  
	theme(     
		axis.text = element_blank(),     
		axis.title = element_blank(), 
		plot.title = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), family = "Kai", size = 10, face = "bold"),
		panel.grid = element_blank()) +    
	coord_polar(start = 0) +
	geom_text(data=label_data, aes(x = id, y = value+10, label = individual, hjust = label_data$hjust), color = "black", fontface = "bold",alpha=0.6, size=2.5, angle = label_data$angle, inherit.aes = FALSE ) +
	ggtitle("马里亚纳海沟。剖面1-25。\nMariana Trench, Profiles Nr.1-25. \nCircular Barplot: Sedimental Thickness by Profiles") 
p