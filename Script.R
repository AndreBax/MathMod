head(iris)
rm(list = ls())
mean(iris)
str(iris)

## ЗАДАНИЕ №1
##  Вычислить среднее для каждой колонки таблицы iris, 
##  за исключением колонки “Species” и соберите результат в список (list)
iris.list = list(colMeans(iris[,-5]))
iris.list

## ЗАДАНИЕ №2
## Вычислить среднее для каждой строки,
## исключив перед этим колонку “Species” и сохраните результат в векторе
iris = iris[,-5]
mRow = c(rowMeans(iris))
mRow

## ЗАДАНИЕ №3
## Создайте случайные 1000 нуклеотидов, сохранив их в вектор DNA, 
## посчитайте количество нуклеотидов A и T, их долю от общей длинны 
## ДНК и запишите результат в вектор ‘dna_at’
dna=c("T","G","C","A")
DNA = c(sample(dna, 1000, replace = T))
length(DNA)
summary(DNA)
DNA
AT = DNA[DNA %in% c("A","T")]
AT
ratio2=summary(factor(DNA))/length(DNA)
ratio2
dna_at = c(251, 262, 0.251, 0.262)

## ЗАДАНИЕ №4
## Создайте вектор, в котором записан произвольный набор 
## латинских букв длинной не менее 10000 символов и посчитайте 
## количество гласных
ABC = c( "a", "b", "c", "d","e", "f", "g", "h", "i", 
         "j", "k", "l", "m", "n", "o", "p", "q", "r", 
         "s", "t", "u", "v", "w", "x", "y", "z")
abc = c(sample(LETTERS, 10000, replace = T))
summary(abc)
aei = abc[abc %in% c("A","E","I","O","U")]
length(aei)
## ЗАДАНИЕ №5
## Отсортируйте все виды в таблице iris по средней длинне лепестков.
## Результат должен быть фактором с градациями в виде имен видов с 
## правильной последовательностью уровне
head(iris)
result = factor(iris[order(iris$Petal.Length),]$Species)
result
## ЗАДАНИЕ №6
## Напишите функцию для рассчета медианы вектора самостоятельно
med <- function(x) {
  z=sort(x)
  if((length(z)%%2)!=0){
    result = z[(length(x)/2)+1]
  }
  else
    result = (z[length(x)/2]+z[length(x)/2+1])/2
  return(result)
}
med(iris$Petal.Width)

## ЗАДАНИЕ №7
## Постройте график зависимости длины чашелистиков от длинны 
## лепестков для каждого вида из таблицы iris
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + 
  geom_jitter(alpha = 0.6) + facet_grid(. ~ Species)


## ЗАДАНИЕ №8
## По данным таблицы ‘diamonds’(пакет ggplot2 ) почитайте среднюю 
## стоимость цены карата для бриллиантов дороже 1000$ для каждой 
## категории яркости (clarity)
x=levels(factor(diamonds$clarity))
y=vector()
for (i in 1:length(x)) {
  y[i]=mean(diamonds$price[(diamonds$price>1000) & diamonds$clarity==x[i]])
}
y


###############################################
####Регрессионный и корелляционный анализы#####
###############################################

## №1
## Написать собственную функцию, которая бы для двух векторов одинакового 
## объема считала бы коэффициент корелляции 
## Спирмена r=1−6∑Ni=1d2n(n2−1)r=1−6∑i=1Nd2n(n2−1). Для получения рангов 
## ипользуйте функцию rank. Дополнительные балы для тех, кто сможет исользовать 
## функцую match вместо rank
my.spearman = function(x, y) {
  if(length(x)==length(y) && is.vector(x)==TRUE && is.vector(y)==TRUE){
    p=1
    n=length(x)
    rx=rank(x)
    ry=rank(y)
    for(i in 1:n)
    {
      p = p - ((( rx[i] - ry[i] )^2)*6)/(n*(n^2-1))
    }
  }
  else{
    if(is.vector(x)==TRUE && is.vector(y)==TRUE)
      print("Ошибка. Вектора должны быть одинаковой длины")
    else
      print("Ошибка.Входные данные должны быть векторами")
    p=-1
  }
  return(p)
}
my.spearman(iris$Sepal.Length,iris$Petal.Length)


## №2
## Используя данные по ссылке, постройте оптимальную линейную модель множественной
## регрессии для co2_flux используя только данные летних месяцев. В данных вместо 
## значения NA используется значения -9999, исправьте это действием подобным 
## data[data == -9999] = NA. Для выбора нужных суток используйте 
## переменную DOY - день года (1 января - DOY = 1)


#######################
###ПОДГОТОВКА ТАБЛИЦЫ##
#######################


## Считываем файл, сразу сделав все возможные преобразования
data = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
               skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
##Удаляем первую строку таблицы
data = data[-1,]
data
## Смотрим информацию по колонкам
glimpse(data)
## Убираем ненужную переменную roll
data = select(data, -(roll))
## Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения:
data = data %>% mutate_if(is.character, factor)
## Устраняем проблему со знаками в переменных
library(stringr)
names(data) =  str_replace_all(names(data), "[!]","_emph_")
names(data) = names(data) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(data)
## Оставляем только численные данные 
data_numeric = data[,sapply(data,is.numeric) ]

## Оставляем только летние месяцы
ar = arrange(data, DOY)
summer.data = filter(ar, DOY %in% ar$DOY[(ar$DOY>150) & (ar$DOY<240)])

## Создаём обучающую и тестовую выборки
row_numbers = 1:length(summer.data$date)
set.seed(655)
training = sample(row_numbers, floor(length(summer.data$date)*.7))
test = row_numbers[-training]

teaching_data_unq = summer.data[training,]
testing_data_unq = summer.data[test,]


#############################
####КОРРЕЛЯЦИОННЫЙ АНАЛИЗ####
#############################
cor_td = cor(data_numeric)
cor_td
## Избавляемся от всех строк, где есть хоть одно значение NA
cor_td = cor(drop_na(data_numeric))
##########

cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .2] %>% na.exclude
vars
##  Собираем все переменные из вектора с именнами переменных в одну формулу
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula

##Собственно линейная модель
my.model = lm(co2_flux ~ LE + rand_err_LE  + h2o_flux + rand_err_h2o_flux + 
                 un_LE + un_co2_flux + un_h2o_flux + h2o_var + w_div_co2_cov, 
               data = teaching_data_unq)
summary(my.model)

## Проверка
pred.model1 = predict(my.model, newdata = testing_data_unq)
summary(pred.model1)

library(ithir)
goofcat(observed = data$co2_flux[training], predicted = my.model)
#Проверка модели на независимой (контрольной) выборке
V.pred.my.model1 <- predict(my.model, newdata = data[-training, ])
goofcat(observed = data$co2_flux[-training], predicted = my.model)
