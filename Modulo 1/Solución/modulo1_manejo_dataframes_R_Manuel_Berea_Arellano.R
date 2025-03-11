# Databricks notebook source
# MAGIC %md
# MAGIC # Evalución 2025 - Módulo 1 - Manejo de dataframes
# MAGIC Manuel Berea Arellano
# MAGIC En el siguiente cuaderno estarán todos los ejercicios pedidos en R para Manejo de dataframes.

# COMMAND ----------

# MAGIC %md
# MAGIC Lo primero que haremos es cargar las librerías que vamos a utilizar.

# COMMAND ----------

R.version.string

# COMMAND ----------

library(SparkR)
# Configuración de dimensiones de los gráficos
options(repr.plot.width = 1200, repr.plot.height = 800)

# COMMAND ----------

# MAGIC %md
# MAGIC Ahora cargamos los datos

# COMMAND ----------

sparkR.session()
gymMembers <- as.data.frame(SparkR::sql("SELECT * FROM hive_metastore.default.gym_members_exercise_tracking2")) 
display(head(gymMembers))

# COMMAND ----------

# MAGIC %md
# MAGIC Se pide:
# MAGIC 1. Indicar la cantidad de miembros que realizan cada tipo de entrenamiento (Workout_Type), diferenciando por nivel de experiencia (Experience_Level)

# COMMAND ----------

print(table(gymMembers$Workout_Type, gymMembers$Experience_Level))

# COMMAND ----------

# MAGIC %md
# MAGIC 2. Calcular el máximo, mínimo, media y mediana de la variable Session_Duration para cada tipo de entrenamiento (Workout_Type)

# COMMAND ----------

df2 <- do.call(data.frame,
               aggregate(`Session_Duration (hours)` ~ Workout_Type, 
                         data = gymMembers,
                         FUN=function(x)c(Maximo = max(x), 
                                          Minimo = min(x),
                                          Media = mean(x),
                                          Mediana = median(x)))
)

colnames(df2) <- c("Workout Type","Maximo","Mininimo","Media","Mediana")

display(df2)
rm(df2)

# COMMAND ----------

# MAGIC %md
# MAGIC 3. Convertir la variable Gender a categórica y mostrar su distribución para los diferentes tipos de entrenamiento (Workout_Type)

# COMMAND ----------

gymMembers$Gender <- factor(gymMembers$Gender,levels = c("Female","Male"))

print(table(gymMembers$Workout_Type, gymMembers$Gender))

# COMMAND ----------

# MAGIC %md
# MAGIC 4. Identificar los miembros que queman más de 500 calorías en una sesión y cuya frecuencia cardíaca máxima (Max_BPM) supera los 180 latidos por minuto

# COMMAND ----------

display(gymMembers[(gymMembers$Calories_Burned > 500) & (gymMembers$Max_BPM >180),])

# COMMAND ----------

# MAGIC %md
# MAGIC 5. Encontrar los miembros con menos de 1 año de experiencia (Experience_Level = 1) que no realizan entrenamientos de fuerza y tienen un porcentaje de grasa corporal (Fat_Percentage) superior al 30%

# COMMAND ----------

display(gymMembers[gymMembers$Experience_Level==1 & gymMembers$Workout_Type!="Strength" & gymMembers$Fat_Percentage>30 ,])

# COMMAND ----------

# MAGIC %md
# MAGIC 6. Identificar los miembros que tienen una ingesta diaria de agua (Water_Intake) superior a 3 litros, realizan entrenamientos HIIT y tienen un IMC (Índice de Masa Corporal) superior a 25.
# MAGIC

# COMMAND ----------

#No existe la columna IMC, se calcula dividiendo el peso entre el cuadrado de la altura
gymMembers$IMC <- gymMembers$`Weight (kg)` / (gymMembers$`Height (m)` ^2)

display(gymMembers[gymMembers$Water_Intake >3 & gymMembers$Workout_Type =="HIIT" & gymMembers$IMC >25, ])

# COMMAND ----------

# MAGIC %md
# MAGIC 7. Crear una tabla de frecuencias cruzadas entre Workout_Frequency (categorías: <3 días/semana y ≥3 días/semana) y Experience_Level

# COMMAND ----------

gymMembers$Workout_Frequency_Factor <- factor(
  ifelse(gymMembers$`Workout_Frequency (days/week)` < 3,
         "Menos 3 dias",
         "Tres dias o mas"))

print(table(gymMembers$Experience_Level,gymMembers$Workout_Frequency_Factor))

# COMMAND ----------

# MAGIC %md
# MAGIC 8. Visualizar en un histograma la distribución de la variable Calories_Burned y dibujar dos líneas verticales para señalar la media (rojo) y la mediana (verde).

# COMMAND ----------

# Crear un histograma
hist(gymMembers$Calories_Burned, 
     main = "Distribución de Calorías quemadas", 
     xlab = "Calorías Quemadas", 
     ylab = "Frecuencia")

# Calcular la media y la mediana
media <- mean(gymMembers$Calories_Burned)
mediana <- median(gymMembers$Calories_Burned)

# Agregar líneas verticales
abline(v = media, col = "red",lwd = 1)  
abline(v = mediana, col = "green", lwd = 1)

# Agregar leyenda
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "green"), lwd = 1)


# COMMAND ----------

# MAGIC %md
# MAGIC 9. Generar un diagrama de cajas para comparar la duración de las sesiones (Session_Duration) según el tipo de entrenamiento (Workout_Type)

# COMMAND ----------

boxplot( `Session_Duration (hours)`~ as.factor(Workout_Type) , 
         data = gymMembers,
         xlab = "Tipo entrenamiento",
         ylab = "Duración de la sesion en horas",
         main = "Duracion de la sesion según tipo de entrenamiento",
         cex.axis = 1,   
         cex.lab = 1,    
         cex.main = 1.2)    