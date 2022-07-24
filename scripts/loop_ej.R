
## 10. Crea un loop que imprima una frase que diga cuántos carácteres tienen
# los siguientes nombres. nchar() puede ser de ayuda para obtener los carácteres

personas <- c("Julio", "Sofía", "Nicolás", "Juan", "Emiliano")

for (i in seq_along(personas)){
  
  print(paste("El nombre", personas[i], "tiene", nchar(personas[i])))
  
}

## 11. Copia todas las bases en formato .xlsx (excel, pero no .csv) a una
# carpeta dentro de "data" que se llame "archivos_excel" y crea un loop para 
# importarlas a R

# Lista con dataframes
filenames <- list.files("data/archivos_excel", full.names=TRUE)

# Creo la lista con los nombres que va a tener cada base (sacar .csv)
namesfiles <- substr(filenames, 21, nchar(filenames)-5) 

# Ahora hago un loop para leer cada base
for (i in seq_along(filenames)) {
  
  assign(namesfiles[i], 
         readxl::read_excel(filenames[i])
  )
  
}