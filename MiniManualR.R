## OPERADORES  ####################################
x <- 1 # operador de asignación "<-" es como el "=" en C++
x <- 1:20 # el ":" es para crear una secuencia, en este caso de 1 a 20


## DATA TYPES  ####################################
# 5 atomic classes: character,numeric,integer,complex,logical


## FUNCIONES  ####################################
print(x) # comando para imprimir

  #VECTORS (lista de objetos de la misma clase)
    x <- vector("numeric", length = 10) # comando para crear vectores
    x <- c(0.5, 0.6) #c se usa para crea vectores tambien, c de concatenar, en ese caso numerico
    x <- c(TRUE, FALSE) # vector logico
    x <- c(T, F) # vector logico de otra forma
    x <- c("a","b","c") # vector de characters
    x <- c(1+0i, 2+4i)#vector de complejos
    # si definimos un vector con distintos tipos de datos, se produce coercion automatica
      # si los datos son numericos y caracteres > todo pasa a caracteres o sea ej 1,7 pasa a ser "1,7"
      # si los datos son logicos y  numericos > todo pasa a numerico, TRUE se convierte en 1 y FALSE en 0
      # si los datos son logicos y caracteres > todo se pasa a caracteres, TRUE pasa a ser "TRUE"
      vec <- c(TRUE,"a", 2) # en este ejemplo queda "TRUE" "a" "2"
  
  #LISTS (lista de objetos de distintas clases)
    x <- list(1, "a", TRUE, 1+4i)
  
  #MATRIX
    x <- matrix(1:12, nrow = 4,ncol = 3)
    dim(x) # para obtener las dimensiones de la matriz, respuesta en formato #filas #columnas
    dim(x) <- c(3,4) # para cambiar las dimensiones de la matriz, en este caso pasa a ser de  3 filas, 4 columnas
    y <- 1:3
    z <- 10:12
    cbind(y,z) # crea una matriz juntando otros vectores, en este caso cada vector es una columna
    rbind(y,z) # crea una matriz juntando otros vectores, en este caso cada vector es una fila
    # convertir un vector en matriz
    x <- 1:10
    dim(x) <- c(2,5)
  
  #FACTORS vector numerico pero donde cada valor tiene un label. es bueno para categorizar, dado que no vemos 
    # 1 y 2 por ejemplo sino que vemos "hombre", "mujer".
    x <- factor(c("yes","yes","no","yes","no"))
    # y para ordenar los niveles a gusto, que el baseline sea el "yes" y el segundo nivel sea el "no":
    x <- factor(c("yes","yes","no","yes","no"), levels = c("yes","no"))
    x
    m
  
  #DATAFRAMES - es LA estructura que se usa para cargar tablas de datos en R
    x <- data.frame(foo = 1:4, bar = c(T,T,F,T))
    nrow(x) # obtiene numero de filas
    ncol(x) # obtiene numero de columnas
    row.names(x) # funcion especial de data.frames
    data.matrix(x) # se puede convertir a matriz con este comando
  
  #NOMBRES, R deja asignar nombres a items en vectores, o listas y a las filas y columnas de un matriz
    x <- 1:3
    names(x) <- c("a","b","c")
    x <- list(a=1,b=2,c=3)
    m <- matrix(1:4, nrow=2, ncol = 2)
    dimnames(m) <- list(c("a","b"),c("c","d"))
  
  #LEER DATA EN R
    read.table() #lee archivos en forma tabular (write.table es la opuesta)
      # argumentos
        # file - nombre de archivo
        # header - TRUE or FALSE si contiene o no encabezado
        # sep - caracter separador de campos
        # colClasses - vector de characters, tiene tantos elementos como campos tenga la tabla, describe el tipo de dato 
          # de cada columna (opcional pero si está ayuda en la velocidad de carga)
        # nrows - numero de filas de la tabla (opcional)
        # comment.char - si queremos especifica algun caracter que se vaya a usar para comentarios (ej #)
        # skip - numero de lineas para saltearse al inicio
        # stringAsFactors - se puede indicar TRUE y R va a observar las columnas char y tratar de identificar si
          # se pueden convertir en tipo factors (aquello de que clasificaba datos)
    #TIPS para tablas grandes
      # - calcular tamaño de archivo y ver si tengo RAM suficiente
      # - set comment.char = "" si no hay comentarios, así se evita buscar este caracter
      # - colClasses es muy importante, así R no se gasta averiguando los tipos de datos
      # - indicar en lo posible la cantidad de filas, ayuda a que R ya sepa de entrada cuanta memoria reservar, 
        # aun si nos pasmos un poco
    data <- read.table("auxiliar.csv")
    read.csv() # lee archivos en forma tabular, separados por coma (writeLines es la opuesta). identico a read.table 
        # pero asume que el caracter separador es la coma
    readLines() # lee lineas de un arhivo de texto
    source() # para cargar archivos con codigo R (dump es la opuesta)
    dget() # dput es la opuesta
      # dump o dput tiene de bueno que trabajan don metadatos de R, y sirve para exportar datos desde R ya con la metadata 
      # necesaria para una futura importacion y así no tener que especificar todos los parametors de read.table.
      # en detalle lo que hace es para cada objeto R genera código R.
      # source y dget, son las operaciones inversas, para importar datos con metadata
      # la diferencia entre dget/dput y dump/source es que los primeros trabajan con un objeto a la vez, en tanto que los
      # segundos pueden trabajar con multiples objetos
    load()
    unserialize()
  
  #CONNECTION : INTERFACES TO THE OUTSIDE WORLD
    file()
    gzfile()
    bzfile()
    url()
    
    con <- file("foo.txt", "r")
    data <- read.csv(con)
    close(con)
    
    # es lo mismo que
    
    data <- read.csv("foo.txt")
    
    # URL
    con <- url("https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/read.table")
    x <- readLines(con)

  #SUBSETTING
    #[]
    #[[]]
    #$
    
    x <- c("a","b","c","c","d","a")
    x[1]
    x[2:4]
    x[x > "a"]
    
    #subsetting lists
    x <- list(foo = 1:4, bar = 0.6, baz = "hello")
    x[1]
    x[[1]]
    x$bar # esta bueno porque puedo extraer elementos de una lista sabiendo el nombre nomas
    x[["bar"]] #idem anterior, este ademas tiene la ventaja que puedo usar una variable por ejemplo x[[varname]]
      # donde varname = "foo", con el operador $ no puedo.
    x[c(1,3)]
    
    #subsetting matrix
    x <- matrix(1:6, 2, 3)
    x[1,2] # es como x[fila,columna], devuelve un vector
    x[1,] # devuelve un vector
    x[,2]
    x[1,2, drop=FALSE] # si queremos que devuelva una matriz de 1x1
    x[1,, drop=FALSE]
    
    #subsetting partial matching
    # util para si hay listas con nombres complejos de objetos, poder usar pocas letras para buscarlos
    x <- list( aardvark = 1:5)
    x$a #si es solo 1, esto ya encuentra aardvark
    x[["a", exact = FALSE]]
    
    #subsetting removing Missing values
    x <- c(1,2,NA,4,NA,5)
    bad <- is.na(x) # se genera un vector logico que indica donde estan los NA
    x[!bad] # se genera un subset tal que no se muestren los TRUE de vector "bad" o sea los valores NA
  
    
    
  #SORTING
    sort(X$var1)
    sort(X$var1, decreasing = TRUE)
    sort(X$var2, na.last = TRUE)#ordenar dejando los NA al final
    X[order(X$var1),] # se puede ordenar la data frame pero se usa "order" en lugar de "sort"
    X[order(X$var1,X$var3),] # se puede ordenar la data frame por mas de un campo
    
  
  as() #explicit coercion, es para explicitamente cambiar la clase de objeto. un vector de enteros lo puedo 
  #tranformar en un vector de caracteres: 
  x <- 0:6
  x
  as.logical(x)
  as.character(x)
  as.complex(x)
  # no funciona la explicit coersion a numeric o a logical si el vector es de caracteres, da valores NA

attributes(y) #deja modificar los atributos para un objeto: nombre, clase, dimensiones, length, user-defined


## MISCELANESO ####################################
x # si se escribe solo el objeto se hace un print por defecto

x <- 1L # indica que vea a 1 como integer explicitamente
    # sino R ve por defecto a cualquier numero como double precission


1 / 0 # hay un objeto llamadao Inf, es infinito

# hay un objeto llamadao NaN, o numero indefinido
0 / 0

getwd() # get working directory
setwd("testdir") # setear el working directory
?c # muestr en el help del panel de al lado la ayuda, es ?[nombre de funcion]
?`:` # ayuda para operadores
dir() # para ver archivos en el directorio del workspace
list.files() #idem anterior
dir.create("testdir") #para crear directorios
file.create("mytest.R") #para crear archivos
file.exists("mytest.R") # para ver si un archivo existe
file.info("mytest.R") # informacion de archivo
file.rename("mytest.R","mytest2.R") # renombrar (FROM, TO)
file.copy("mytest2.R","mytest3.R") # hacer una copia de un archivo (FROM, TO)
file.path("folder1","folder2") # genera una secuencia de directorios teniendo en cuenta el simbolo separador
  # del sistema operativo que se esta usando


seq(0,10, by=0.5) # genera secuencias con mas poder que el `:`, por ejemplo en esta el paso es de 0.5 en lugar de 1
rep(0, times = 40) # funcion replicate, este genera 40 ceros
rep(c(0, 1, 2), times = 10) # este replica el vector 0,1,2 10 veces
rep(c(0, 1, 2), each = 10 ) # este toma el vector 0,1,2 y genera 10 ceros, 10 unos y 10 dos


which() # dado un vector y una condicion (ej vectorint > 3) devuelve las posiciones de los que son TRUE
any() # dado un vector y una condicion (ej vectorint > 3) devuelve TRUE si hay al menos un TRUE en el resultado
all() # dado un vector y una condicion (ej vectorint > 3) devuelve TRUE si todos son TURE en el resultado

# generar un vector logico para filtrar valores
data <- read.csv("hw1_data.csv")
logicalcomparator <- (!is.na(data[1])) & (data[4]>90)&(data[1]>31)

y <- data[2]
y <- y[logicalcomparator]
mean(y)

## ESTRUCTURAS DE CONTROL
  # IF-ELSE
  if(x > 3){
    y <- 10
  } else {
    y <- 0
  }

  #esto es valido tambien
  y <- if(x > 3){ 10 } else { 0 }

  # FOR
  for(i in 1:10){
    print(i)
  }

  # WHILE
  count <- 0
  while( count < 10){
    print(count)
    count <- count +1
  }

  # REPEAT, repite el bucle hasta que se encuentra el break
  count <- 0
  repeat{
    
    if( count > 10){
      break
    }
    count <- count+1
  }
  
  #NEXT sirve para saltarse una iteración dentro de un bucle
  
## LOOPs (lapply, sapply, apply, tapply, mapply, split)
  
  # LAPPLY - recibe una lista, y una funcion, y hace que cada elemento de la lista sea
    # procesado por la funcion
    # el input es una lista, o sera coerced a una lista. La salida es una lista
    # lapply({list},{function},...), el "..." es para pasar argumentos de la funcion {function}
  x <- list( a = 1:5, b = rnorm(10))
  lapply(x, mean)
    # aparece la anonymous function, si la funcion que quiero usar en lapply no existe la puedo crear en el momento
  lapply(x, function(elt) elt[,1])
    # esta funcion anonima, para el argumento de input devuelve la primer columna "[,1]". se usa el nombre
      # "elt" para nombrar el argumento de entrada y poder definir la funcion. en funcionamiento, los elementos
      # de x van a tomar el lugar de elt
  
  # SAPPLY es identico a lapply, pero intenta mostrar simplificada la salida. si por ejemplo la salida son 
    # todos elementos de largo 1, sapply lo muestra como un vector. Si son todos elementos con el mismo largo, 
    # sapply lo mustra como matriz, si no puede mejorarlo devuelve una lista como lapply
  sapply(x, mean) 
  
    # para alterar cada uno de las filas de un dataframe (en este caso calcula las distancias)
  sapply(1:nrow(df),function(i)spDistsN1(as.matrix(df[i,4:5]),as.matrix(df[i,12:11]),longlat=T))
  
  # APPLY es similar a lapply pero se puede acotar a que parte de la lista aplicar la funcion
  x <- matrix(rnorm(200),20,10) # matriz de 20 filas y 10 columnas
  apply(x,2,mean) # aplica la funcion "mean" a las columnas ( valor 2 en arg MARGIN) de "x"
  apply(x,1,mean) # aplica la funcion "mean" a las filas (valor 1 en arg MARGIN) de "x"
   
  # MAPPLY es similar a los anteriores, pero se usa para el caso de que la funcion requiera varias listas
    # de input en distintos argumentos: funcion(a,b), donde a es una lista y b es otra.
  mapply(rep, 1:4, 11:14) #usando mapply se pasa a la funcion rep 2 listas como argumentos: "1:4" y "11:14"

  # TAPPLY es para mezclar LAPPLY con agrupamiento. ademas de pasar x y la funcion, se pasa un vector
    # del mismo largo de X con la clasificación, y el resultado de TAPPLY es la aplicación de la funcion
    # a cada grupo de la clasificacion
  x <- c(rnorm(10),runif(10),rnorm(10,1))
  f <- gl(3,10)
  tapply(x,f,mean)
  
  # VAPPLY es identico a LAPPLY pero se puede especificar el tipo de valor que espero
    #y si la respuesta del VAPPLY no viene en ese formato, dar error. util para hacer funciones donde no vemos
    # los resultados en consola y lapply puede devolver en un formato que no nos sirve
  vapply(x,mean,numeric(1))
  
  # SPLIT es para clasificar un grupo de datos. SPLIT + LAPPLY = TAPPLY
  split(x,f) # separa el ejemplo de arriba en 3 listas
  sapply(split(x,f),mean)
    # lo bueno de split que permite hacer cosas mucho mas complejas
  s<-split(airquality,airquality$Month) #el resultado de split se guarda en s
  lapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")])) #function es una funcion anonima, no tiene nombre
    # se define y se usa en el momento, luego se destruye
  sapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))
  sapply(s, function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm = TRUE)) 
    #na.rm es para que eliminte los valores NA antes de hacer la cuenta
  
  # multilevel
  # se definen 2 factors y luego se combinan para dar una clasificacion en 2 niveles
  f1 <- gl(2,5)
  f2 <- gl(5,2)
  
  interaction(f1,f2) # genera una clasificacion de 2 niveles
  # [1] 1.1 1.1 1.2 1.2 1.3 2.3 2.4 2.4 2.5 2.5
  # Levels: 1.1 2.1 1.2 2.2 1.3 2.3 1.4 2.4 1.5 2.5
  
  ## FUNCIONES
  add2 <- function(x,y){
    x + y
  }
  # la funcion siempre muestra en pantalla la ultima sentencia
  add2(3,5) 
  
  # si quiero algun valor por defecto, lo defino al principio:
  add2 <- function(primer_arg,segundo_arg = 10){
    primer_arg + segundo_arg
  }
  add2(3,5) # esto da 8
  add2(3) # esto da 13 porque asume que y, si no está explicito toma el valor por defecto 10

  #sobre los argumentos,
    # pueden ser nombrados explicitamente add2(primer_arg=5,segundo_arg=3)
    # pueden ser partially matched, o sea poner las primeras letras add2(p=5,s=3)
    # pueden ser ingresados sin nombre add2(5,3)
    # se puede hacer una mezcla de las anteriores, y R lo que hace es:
      # poner la lista de argumentos sobre la mesa
      # los explicitos los matchea y los quita de la lista
      # los partial matching, idem anterior
      # ve cuales argumentos de la lista quedan por completar
      # va usando los que se ingresaron en orden.
      # los que queden sin completar se ignoran, si luego eran necearios fallará 
        #en el momento en que se vayan a utilizar
    # hay un argumento especial que es el "..." que se coloca cuando no se sabe exactamente cuantos esperar
      # si la funcion tiene mezcla de "..." y otros, a los otros hay que nombrarlos explicitamente
  formals(add2) # devuelve los argumentos de una función
  
#ENVIROMENTS
    # R no tiene todos los objetos en una sola bolsa, los tienen organizados en enviroments
    # Los enviroments están ordenados por jerarquia.
    # Las variables que creo en la consola se alojan en el enviroment GlobalEnv que es el de más baja jerarquia
    # Luego siguen todos los packages que se hayan cargado, siendo el ultimo cargado el siguiente en jerarquia
      # el cargado anterior a este es el tercero en jerarquía y así hasta el paquete que se cargó primero
    # Luego viene el Autoloads, sería el tercero en jerarquía más alta
    # Luego el "base", el segundo en jerarquía mas alta
    # y el de más alta jerarquía, el EmptyEnviroment.
    # Se puede ver este orden con la funcion:
    search()
    
    # las funciones crean su propio enviroment y el padre de la misma es el enviroment donde la creé, tipicamente el 
      #GlobalEnv
    ls(environment(add2))
    # si se crea una funcion INT dentro de otra furncion EXT, el parent environment de la funcion INT
      #  es la función EXT.
    
# DATE AND TIME
    # Fecha - numero de dias desde 01/01/1970
    # Tiempo - numero de segundos desde 01/01/1970
  x <- as.Date("2020-12-30")

    # Para tiempo se usa POSIXct (valor entero) o POSIXlt (lista de items, como el structtm de C++)
  x <- Sys.time()
  p <- as.POSIXlt(x)  
  unclass(p) # muestra los items de la lista (sec, min, hour,mday,mon, year,wday, isdst,zone,gmtoff)
  
  pct <- as.POSIXct(x)
  unclass(pct)
  
    # Para convertic cualquier cadena en formato date>
  strptime()
  
  datestring <- c("Diciembre 30, 2020")
  x <- strptime(datestring,"%B %d, %Y")
  ?strptime
  
  # se pueden hacer operaciones con fechas, pero tener en cuenta que las fechas estén con el mismo
    # tipo de datos: Date, POSIXlt o POSIXct
  # tener en cuenta para manejo de fechas el package lubridate
  
  format(x,"%d") # mostrar dia del mes (0-31)
  format(x,"%a") # mostrar dia de la semana abreviado (ej "dom.")
  format(x,"%A") # mostrar dia de la semana no abreviado (ej "domingo")
  format(x,"%m") # mostrar mes del año (01-12)
  format(x,"%b") # mostrar mes abreviado (ej "Ago.")
  format(x,"%B") # mostrar mes no abreviado (ej "Agosto")
  format(x,"%y") # mostrar año en 2 dígitos
  format(x,"%Y") # mostrar año en 4 dígitos
  
    
  #convertir cualquier texto a date, ej vector de caracteres (tener en cuenta que es un poco estricto, no reconoce "Ene", pero si "Ene.")
  x = "01Ene.1960"
  z = as.Date(x,"%d%b%Y")  
  z
  y = as.Date("14-08-2022","%d-%m-%Y")
  y - z  # diferencia en días, retorno en characters
  as.numeric(y-z) # diferencia en días, retorno en characters y conversión a numero
  
  weekdays(z) # para saber el día de la semana
  months(z) # para saber el mes
  julian(y) # para saber los días desde el 01-01-1970
  
  #Lubridate
  library(lubridate)
  # para trabajar con lubridate, es bueno configurar la variable Locale LC_TIME en formato US - UTF-8
  # es para que entienda por ejemplo Jan, sino tenemos que poner Ene y los datos normalmente no estan en español
    syslocaleactual <- Sys.getlocale("LC_TIME")
    "Spanish_Uruguay.1252"
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
  y <- ymd("20220413")  #ymd, convierte "cualquier cosa" en fecha, tambien estan "mdy" y "dmy"
  y <- ymd_hms("20220413 123124") #idem anterior, pero agrega hora, minuto y segundo
  
  Sys.timezone()  # se puede indicear también el Timezone
  y <- ymd_hms("20220413 123124", tz="America/Montevideo")
  # O
  y <- ymd_hms("20220413 123124", tz=Sys.timezone())
  wday(y) #igual a weekdays, pero devuelve un entero (1 = domingo, 2 = lunes, etc)
  today() #fecha de hoy YYYY-MM-DD
  now()   #fecha y hora de este momento "YYYY-MM-DD hh:mm:ss timezone"
  year() #devuelve el año
  month() #devuelve el mes
  day() #devuelve el día
  hour()  #devuelve la hora
  minute()  #devuelve el minuto
  second()  #devuelve el segundo
  
  #operaciones aritmeticas
  y + days(2) #suma 2 días a la fecha y
  #idem para sumar hours(), minutes(), etc..

    
########## TEXT - trabajos con textos

  #pasar a minusculas
  tolower(X)
  
  #separar texto usando algún separador (en este ejemplo, se usa el ., las \\ son por ser un metacharacter)
  X <- "Location.1"
  strsplit(X,"\\.")  
  
  library(stringr)
  #cantidad de caracteres
  nchar(X)
  # substring
  substr(X,1,3)
  # concatenar con espacios en el medio
  paste(X,"bien")
  # concatenar sin espacios en el medio
  paste0(X,"bien")
  # trim de espacios
  str_trim("agua   ")
  
  #buscar
  lista <- list("agua","aire","tierra","fuego")
  lista
  grep("agua", lista) # devuelve el id de donde esta la coincidencia
  grep("agua", lista, value=TRUE) # devuelve el contenido de las coincidencias
  grepl("agua", lista) #devuelve un vector del mismo largo con TRUEs y FALSEs
  
  #METACHARACTERS
  grep("^a", lista) # primer caracter es "a"
  grep("a$", lista) # ultimo caracter es "a"
  grep("[g][u]", lista) # palabras que contengan una g seguida de una u
  grep("[gu]", lista) # palabras que contengan las letras g y u, en cualquier orden
  grep("[a-g]", lista) # palabras que contengan las letras a,b,c,d,e,f o g en cualquier orden
  grep("a.u", lista) # palabras que contengan las letras "a", seguida de cualquier caracter, seguida de "u"
  grep("[g]|[u]", lista) # (OR) palabras que contengan una g OR una u 
  grep("^(t|a)", lista) # que empiece con t o con a
  grep("(a)?", lista ) # que opcionalmente tenga una "a"
  grep("a.", lista) # "." una ocurrencia de cualquier caracter
  grep("a*", lista) # "*" 0 o mas ocurrencias del caracter anterior, en este caso matchea cualquier palabra que tenga o no "a"
                    # apunte del *, es avaro, si no se indica nada, y si aparentemente faltan resultados, puede que el * este
                    # devolviendo el match más largo, puede se sano acompañar el .* con .*?
  grep("a+", lista) # "*" 1 o mas ocurrencias del caracter anterior, en este caso matchea cualquier palabra que tenga al menos una "a"
  grep("r{2}", lista) # curly son para definir cuantas ocurrencias, en este caso que tenga exactamente 2 "r"
  grep("r{2,5}", lista) # en este caso que tenga al menos 2, pero no mas de 5 "r"
  grep("r{2,}", lista) # en este caso que tenga al menos 2 "r"
  grep("(a)..\1", lista) # parentesis sirve para "guardar" patron y se lo vuelve a usar con \1 (primer par de parentesis) o \2, etc
  
  
 
  
  
# DEBUGGING
  # 3 niveles basicos, message(indica algo), warning(advierte algo, pero no detiene ejecucion)
    # error, avisa y detiene la ejecucion
  # un adicional es "condition" para definir otro tipo ademas de message,warning y error
  # preguntas utiles:
    # cual fue el input
    # cual era el output esperado
    # cual fue el output obtenido
    # como difiere lo obtenido de lo esperado
    # eran las espectativas correctas en un primer lugar
    # se puede reproducir el problema?
  #HERRAMIENTAS DE R
  # traceback: imprime el stack de funciones llamadas hasta llegar hasta la actual. 
    # SE EJECUTA JUSTO DESPUES DEL ERROR
  # debug: setea una funcion en modo debug, así que cuando entre a esa funcion se puede controlar la ejecucion
    # linea a linea
    # cambia el cursor a Browser2> y con la letra n vamos pasando linea a linea
    # se quita la condicion con undebug(funcion)
  # browser: es como debug, pero permite iniciar el modo debug en el medio de una funcion, no neceariamente al principio
  # trace: permite insertar codigo de debug en una funcion, sin alterar la funcion
  # recover:  al encontrar un error la funcion termina, con recover se puede pausar esto y mirar las distintas funciones
    #involucradas, y tratar de entender donde estuvo el error.
  # printing statements: lo que hago yo, imprimir en pantalla
  mean(a)
  traceback()  
  lm(a-x)
  traceback()
  debug(lm)  
  ?debug
  undebug(lm)    
  
  
  #FUNCION str para ver qu? tiene un objeto. se le puede hacer str a cualquier cosa y te muestra un resumen de lo 
   # c?mo es y qu? tiene adentro
  str(str)

# GENERAR NUMEROS ALEATORIOS
  x <-rnorm(100,2,4)
  
  dnorm(x) # densidad de probabilidad
  pnorm(x)  # funcion de distribuci?n
  qnorm()   # quantile
  
  #super util para generar los mismos numeros aleatorio, como para poder reproducir un error
  # siempre se recomienda setearlo
  set.seed(1)
  
  #otra forma es con sample
  sample(1:10,4) # elige de un vector del 1 al 10, 4 numeros al azar sin repetir
  sample(letters,4) # elige de las letras a-z, 4 sin repeticion
  sample(1:10) # trae todos los elementos con orden aleatorio  
  sample(1:10, replace = TRUE) # trae 10 elementos, pero se habilita la repeticion  

# PROFILING / o sea analizar el codigo para ver donde es lento
  # herramientas para eso
  
  # saca el tiempo (en segundos) de una funcion o una porcion de codigo, esta bueno cuando sospechamos donde esta el problema
  # se pueden poner entre {} varias lineas R y evalua el tiempo que lleva ejecutar todo.
  system.time(rnorm(100,2,4))
  
  # si no tenemos idea de donde esta el problema vamos por Rprof
  # rprof toma una foto del stack cada 0.02 segundos y luego con summaryRprof podes leer los resultados
  # de en que funcion paso mas tiempo
  
  Rprof()
  summaryRprof()
  by.total
  by.self #en principio el mas util, muestra los tiempos de cada funcion rankeados
  
  
  
  ##############
  # HERRAMIENTAS ANALISIS DE DATOS
  ################
  
# Descarga de archivos (se guarda una url en una variable y se la usa en download.file)
  fileurl <- "http://www.histdata.com/download-free-forex-historical-data/?/metatrader/1-minute-bar-quotes/eurusd/2022/6"
  download.file(fileurl, destfile = "histdata.html")
  
  # Trabajo con EXCEL
  install.packages("xlsx")
  library(xlsx)
  # sheetIndex =1 refiere a la primera hoja, header TRUE refiere a que tiene encabezado
  datos <- read.xlsx("Módems Ziv 2G.xlsx",sheetIndex = 1, header = TRUE)
  
  colIndex <- 2:3
  rowIndex <- 1:4
  # colIndex define que columnas importar y rowIndex define qué filas.  
  datos2 <- read.xlsx("Módems Ziv 2G.xlsx",sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex)

  #para escribir archivos excel
  write.xlsx(datos2,"salida.xlsx")
  
  
  #Leer archivos XML
  library(xml2)
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml" 
  #leer xml
  doc <- read_xml(fileurl) 
  #obtener de ese xml los items row
  recs <- xml_find_all(doc, "//row")
  #obtener de cada row el item zipcode
  zipcodes <- xml_find_all(recs, "//zipcode")
  #obtener de cada zipcode el valor
  zipcodesVal <- xml_text(zipcodes)
  
  #JSON para leer apis
  library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)


#comando para leer archivos a data.table
DT <- fread("UScommunities_preg5.csv")
# mejor que data.frame: data.table
library(data.table)
#creacion de una tabla de 3 col y 9 filas
DT = data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
# para ver a resumen del DT
tables()
# subsetting, el campo a la izquierda de la coma selecciona filas, el campo a la derecha es para aplicar formulas a la tabla
# para ver la fila 2
DT[2,]
# para ver las filas que tienen el valor "b" en la columna de nombre "y"
DT[DT$y=="b",]
# para ver las filas con valor menor a 0 en la columna x
DT[DT$x<0,]
# aplica las formulas sum de la columna x y sum de la columna z
DT[,list(sum(x),sum(z))]
# agrega una nueva columna w con valor z al cuadrado
DT[,w:=z^2]
# agrega columna m con valor, primero se asigna a tmp la suma de x+z y lego se hace el log en base 2 de tmp +5
DT[,r:={tmp <- x+z; log2(tmp+5)}]


#MYSQL
install.packages("RMySQL")
library(RMySQL)

# conexion a db
# el usuario en la db debe estar creado asi:
#  create user 'usuarior' IDENTIFIED WITH mysql_native_password BY 'usuariosr';
#  GRANT ALL PRIVILEGES ON fx.* TO 'usuarior';
#     mysql_native_password, sino da error de sha2

mydb = dbConnect(MySQL(), user='usuarior', password='usuariosr', dbname='fx', host='127.0.0.1')

# se guarda en resultset
rs = dbSendQuery(mydb, 'SELECT * FROM tradingresults30porciento')

# para limpiar resultset, si se necesita
dbClearResult(dbListResults(mydb)[[1]])

# se hace el fetch (con n = -1 se trae todos los registros)
data = fetch(rs, n=-1)

#otra forma de traer resultados
result <- dbGetQuery(mydb,"show databases;")

# todas las tablas de una base
allTables <- dbListTables(mydb)

#todos los campos de una tabla
dbListFields(mydb,"eurusd30")

#otra forma de leer datos, en este caso el contenido de una tabla completa
contenido <- dbReadTable(mydb,"eurusd1w")

#cerrar conexion
dbDisconnect(mydb)




#HDF5
#para instalarlo
install.packages("BiocManager")
BiocManager::install("rhdf5")

library(rhdf5)
getwd()
#crear archivo hdf5
created = h5createFile("example.5")

#ver informacion
browseVignettes("rhdf5")

#crear grupos dentro de archivo hdf5
created = h5createGroup("example.5","foo")
created = h5createGroup("example.5","baa")
created = h5createGroup("example.5","foo/foobaa")
h5ls("example.5")

#agrega contenido a grupos existentes o en grupos nuevos
A = matrix(1:10,nr=5,nc=2)
h5write(A,"example.5","foo/A")
h5ls("example.5")

#lee de los grupos información
readA = h5read("example.5","foo/A")
readA




#WEB SCRAPPING FORMA 1
con = url("https://www.ine.gub.uy/")
htmlCode = readLines(con)
htmlCode

library(XML)
# se usa el htmlcode obtenido antes, y se parsea con esta funcion, teniendo en cuenta
#que como es una variable que contiene html (y no un archivo o una pagina web que se fue a buscar)
# hay que oner asText = True, eso hace que se de cuenta que htmlCode es una variable que contiene datos 
#en formato html.
html <- htmlTreeParse(htmlCode, asText = TRUE,useInternalNodes =TRUE)

xpathSApply(html, "//title", xmlValue)

#WEB SCRAPPING FORMA 2
url <- "https://www.ine.gub.uy/"
library(httr)
html2 = GET(url)
content2 = content(html2,as="text")
content2
parsedHtml = htmlParse(content2,asText = TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

#con httr me puedo autenticar si es necesario
html2 = GET(url, authenticate("user","password"))




#API
# para esto se usa httr
myapp = oauth_app("twitter", key = "a", secret = "b")
sig = sign_oauth1.0(myapp,token = "t",token_secret = "ts")
# api.twitter da a entender que es el api
# 1.1 es la version de la API
# statuses y home_timeline, es lo que quiero leer, pueden ser otras cosas
# json, es el formato en el que vienen las cosas a traves de la API
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)

#se extrae el contenido de homeTL que es lo leido por API
json1 = content(homeTL)
# pero queda medio dificil de leer asi que cargando el paquete jsonlite se hace esto:
# json 1 se lo vuelve a convertir en JSON con toJSON
# y luego se lo desconvierte usando fromJSON
json2 = jsonlite::fromJSON(toJSON(json1))

# para apis tambien, tener en cuenta el sitio rapidapi



# COMO VISUALIZAR RAPIDAMENTE EL CONTENIDO DE UNA TABLA

# supongamos un dataframe x
head(x) # muestra las primeras 6 lineas de la tabla
tail(x) # muestra las ultimas 6 lineas de la tabla
summary(x) # muestra un resumen de cada columna
str(x) # tambien muestra un resumen de las columnas de una forma diferente a summary
quantile(x,na.rm=T) # te muestra el minimo, maximo y media

table(x$plcdst_no, useNA = "ifany") # con una variable, usa variables factor y las sumariza en una tabla
table(x$plcdst_no, x$cncl_dst) # con 2 variables hace comparacion tipo cubo, en filas una variable, en columnas la otra y cuenta.
table(x$plcdst_no %in% c(3,4,5)) # arma tabla con la cuenta de cuantos pldst_no son 3,4 o 5 y cuantos no
x[x$plcdst_no %in% c(3,4,5),] # tambien se puede traer toda la tabla con ese filtro
xtabs(Freq ~ Gender + Admit, data=x) # se pueden hacer cubos tambien con xtabs (Freq es para mostrar frecuencia, 
  # Gender es una columna de la tabla y Admit es otra columna de la tabla, data es el dataframe que tiene esas columnas)
ftable(xtab) # cuadno el cubo tiene mas de 2 variables se usa ftable para mostrarlo de mejor forma.

#chequeo de valore faltantes (na)
sum(is.na(x$blabla)) # suma todos los NA de la columna blbalba de la tabla x
any(is.na(x$blabla)) # da TRUE si alguno de los valores de blabla es NA
all(x$blabla >0) # da true si todos los valores de blabla son > 0
colSums(is.na(x)) # suma los na en cada columna y devuelve para cada columa la cantidad de NA
all(colSums(is.na(x))==0) # esto devuelve TRUE si todas las columnas tienen 0 NA

#syze of dataset
object.size(restData)
print(object.size(restData),units = "MB")

# AGREGAR COLUMNAS AL DATAFRAME CON DATOS QUE NECESITO PARA EL ANALISIS

#crear secuencias - para poder asignar un ID a cada fila por ejemplo
seq(along = x) 

# agregar COLUMNAS BINARIAS (T,F)

#ejemplo se agrega columna D con valores True cuando COLA tiene valores 3 y 2 (usando %in%)
colA <- c(1,2,3,4,5,6,7,8,9,10)
colB <- c("a","b","c","d","e","f","g","h","i","j")
colC <- c(T,T,F,T,T,F,F,F,T,F)
data2 <- data.frame(colA,colB,colC)

data2$colD = data2$colA %in% c(3,2)
data2

#ejemplo se agrega columna E con valores True colb = b (usando ifelse)
data2$colE = ifelse(data2$colB == c("b"),T,F)
data2

#crear una columna tipo categoria(factor), usando los datos de otra columna para armarla
data2$colAGroups =cut(data2$colA,breaks = quantile(data2$colA))
data2
table(data2$colA,data2$colAGroups)


#RESHAPING DATA, es cuando quermos transformar toda la tabla que nos viene, en otra cosa

# en este caso lo que se hace es generar a partir de una tabla, un subconjunto de valores.
# de los valores de mtcars, se extraen carname, gear, cyl, y para cada uno de esta triada se pone en una fila 
# el hp y en otra fila el mpg.
library(reshape2)
head(mtcars)
mtcars2 = mtcars
mtcars2$carname <- rownames(mtcars2)
carMelt <- melt(mtcars2,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
carMelt

# se puede usar la funcion dcast para sumarizar la tabla anterior
  #cuenta la frecuencia de las variables para los distintos tipos de cyl
cylData <- dcast(carMelt, cyl ~ variable)
cylData

  #hace el promedio de las variables para los distintos tipos de cyl
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData


# DPLYR package (buenisimo para trabajar dataframes, permite facilmente hacer cosas como con SQL)
library(dplyr)

#primero para pasar una dataframe a un data frame table, formato especificado por dplyr, se usa el comando tbl_df
chicago <- tbl_df(datafram1)

  #SELECT
#mostrar columnas city y dptp
head(select(chicago, c("city","dptp")))
#mostrar columnas desde city hasta dptp, o sea que se muestran todas las que estan en medio tambien
head(select(chicago, city:dptp))
#mostrar todas las columnas menos las que van desde city hasta dptp
head(select(chicago, -(city:dptp)))

  #FILTER
# filtra un data frame con condiciones
head(filter(chicago,pm25tmean2 > 30))
head(filter(chicago,pm25tmean2 > 30 & tmpd > 80))

  #ARRANGE
#ordena
# por fecha ASC por ejemplo
head(arrange(chicago,date))
# por fecha DESC por ejemplo
head(arrange(chicago,desc(date)))

  #RENAME
#cambia nombres de columnas
chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
head(chicago)

  #MUTATE
#sirve para agregar columnas con calculos, o para modificar contenido de las existentes
# ejemplo de agregar una columna x_centrada, que tiene el valor de x - media(x)
chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm=TRUE))
filter(chicago, !is.na(pm25))

  #GROUP BY
#agrupa para poder usar luego el sumarize
chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80), labels = c("cold","hot")))
hotcold <- group_by(chicago,tempcat)

summarize(hotcold)
summarize(hotcold,pm25)
summarize(hotcold, pm25 = mean(pm25, na.rm = TRUE))

# uso de pipeline %>% (encadenas operaciones, y observar que en el primer argumento de cada funcion no se indica el dataframe
  # esta implicito que nos referimos al mismo dataframe)
chicago %>% mutate(month = as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% summarize(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2, na.rm = TRUE), no2 = median(no2tmean2, na.rm = TRUE))

#Merge data
# con PLYR (tienen que tener el mismo nombre los campos en ambas tablas)
join(dataframe1,dataframe2)

#con la funcion merge
#by.x es el id en la tabla 1 que se corresponde con by.y o sea el id en la tabla 2.
# all = true es que trae todo
mergeData = merge(dataframe1,dataframe2,by.x="id_df1",by.y="id_df2",all=TRUE)

# usara gather para rearmar los datos y quedar con una tabla con la definicion de tidy data
# ejemplo, tengo una tabla con los siguiente campos: notas,male, female
# para cumplir con la definicion de tidy data, deberia tener en su lugar, notas, sex, count
gather(data,sex,count,-notas)
# hace que se transforme a la tabla tidy, -notas, es para que no tenga en cuenta ese campo, dado que ya está bien.
#It's important to understand what each argument to gather() means. The data argument, students, gives the name of the original
#dataset. The key and value arguments -- sex and count, respectively -- give the column names for our tidy dataset. The final
#argument, -grade, says that we want to gather all columns EXCEPT the grade column (since grade is already a proper column
#variable.)

#OTRA para separar el contenido de una columna en 2 variables, se puede usar:
separate(res,sex_class, c("sex","class"))
# se pasa el dataset, la columna que queresmos separar y luego las columnas destino
#Conveniently, separate() was able to figure out on its own how to separate the sex_class column. Unless you request otherwise
# with the 'sep' argument, it splits on non-alphanumeric values. In other words, it assumes that the values are separated by
# something other than a letter or number (in this case, an underscore.)

#OTRA spread, separa una columna en 2.
#por ejemplo si tengo 
## Source: local data frame [12 x 4]
## 
##        country year        key      value
## 1  Afghanistan 1999      cases        745
## 2  Afghanistan 1999 population   19987071
## 3  Afghanistan 2000      cases       2666
## 4  Afghanistan 2000 population   20595360

#usando 
spread(table2, key, value)
#llego a
##       country year  cases population
## 1 Afghanistan 1999    745   19987071
## 2 Afghanistan 2000   2666   20595360


######################################
######  GRAFICOS

#Principle 1: Show comparisons
#Principle 2: Show causality, mechanism, explanation
#Principle 3: Show multivariate data
#Principle 4: Integrate multiple modes of evidence
#Principle 5: Describe and document the evidence
#Principle 6: Content is king

# 3 grandes sistemas
# sistema Base, sistema Lattice, sistema ggplot


# PODEMOS USAR EL SISTEMA BASE PARA HACER GRAFICOS RAPIDOS, COMO PARA ENTENDER NOSOTROS LA INFORMACION
# EJEMPLOS:

# ejemplo de vela como de las cotizaciones
# grafica la columna especificada de la tibble, y se le puede poner color
boxplot(pollutionfiltered$pm25, col = "blue")

# ejemplo de histograma, para los mismos datos anteriores
hist(pollutionfiltered$pm25, col = "green")

#para agregar en la grafica anterior la concentracion de puntos como un minigrafico abajo
rug(pollutionfiltered$pm25)

# para hacer en el histograma barras mas estrechas, se usa el breaks
hist(pollutionfiltered$pm25, col = "green", breaks = 100)

# linea horizontal agregada al grafico, agrega linea horizontal en el valor 12
# y otra linea color magenta de espesor 4 en la mediana de valores
boxplot(pollutionfiltered$pm25, col = "blue")
abline(h = 12)
abline(h = median(pollutionfiltered$pm25), col = "magenta", lwd = 4)

# grafico de barras, muestra la frecuencia para cada valor distinto de pm25
barplot(table(pollutionfiltered$pm25), col = "wheat", main = "Frequency of each value")

#multiple boxplots
boxplot(pm25 ~ COUNTY_CODE, data = pollutionfiltered, col = "red")

#Multiple Histograms
  # par es para especificar caracteristicas del grafico que viene
    # mfrow, especifica cuantos graficos en fila y cuantos en columna: c(2,1), refiere a 2 filas y 1 columna
    # mar, refiere a los margenes
  # una vez ejecutado par, se ejecutan las instrucciones para graficar, en este caso 2 para completar las 2 Filas definidas antes
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollutionfiltered, COUNTY_CODE <= 60)$pm25, col = "green", breaks = 100)
hist(subset(pollutionfiltered, COUNTY_CODE > 60)$pm25, col = "green", breaks = 100)
# reset par effects
par(mfrow = c(1, 1))

#Scatterplot
with(pollutionfiltered, plot(COUNTY_CODE,pm25))
with(pollutionfiltered, plot(COUNTY_CODE,pm25,col = COUNTY_CODE) )


#Ejemplo de ir sumando informaci?n con el sistema base
library(datasets)
# se inicializa el graficado sin colocar los punto dentro del grafico (usando el type = "n"), 
# esto es porque queremos agregar los puntos luego con otros formatos
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n"))
# se agregarn ahora los puntos del mes de Mayo con color azul
with(subset(airquality, Month == 5), points(Wind,Ozone,col = "blue"))
# se agregarn ahora los puntos de los otros meses que no son mayo, con color rojo
with(subset(airquality, Month != 5), points(Wind,Ozone,col = "red"))
# se agrega la leyenda
  # pch es la forma del punto, 1 es la que se coloca por defecto
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))
# se puede calcular una linea de aproximacion lineal
model <- lm(Ozone ~ Wind, airquality)
# y se agrega esa linea al gr?fico
abline(model, lwd = 2, col = "purple")

# si queremos colocar varias graficas en la misma pagina
  # la funcion par, da los lineamientos de cantidad de graficos, su disposici?n y sus margenes
  # en este caso mfrow indica 1 fila 3 columnas
  # mar es margenes y maneja valores para los 4 lados comenzando por abajo y en sentido horaio
    # abajo 4, izquierda 4, arriba 2, derecha 1
  # oma es margen exterior (como para poner un titulo)
    # en este caso asigna el valor 2 al margen superior
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)
})

#extras
#agregar etiqueta a cada punto
text(x+0.05, y+0.05, labels=as.character(1:12))
## FIN SISTEMA BASE DE GRAFICO

##############################
### OTRO SISTEMA GRAFICO
###   LATTICE
library(lattice)
library(datasets)

# Formula principal de Lattice
# se grafica x e y, para cada variación de f * g
xyplot(y ~ x | f * g, data)
xyplot(Ozone ~ Wind, data = airquality)

# como dato al margen, lattice usa "grid" para trabajar, y genera como resultado un objeto "trellis"
# cuando llamamos a la funcion xyplot directamente, R se encarga de autoimprimir ese objeto trellis en pantalla
# pero también se lo puede guardar en una variable para usarlo de otra forma.

airquality <- transform(airquality, Month = factor(Month))
#En este caso son 5 paneles, generando 1 grafico de Ozono y Wind para cada mes
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

#Se pueden customizar los paneles, si quisiera agregarle una linea horizontal con la media a cada uno
# y otra linea con el modelo lineal, se puede hacer:
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1), panel = function(x, y, ...){
  panel.xyplot(x, y, ...) # Primero se llama a la funcion normal de graficado
  panel.abline(h = median(y, na.rm = TRUE), lty = 2) # linea horizontal con la mediana
  panel.lmline(x,y, col = 2) # linear regression line
})


##############################
### OTRO SISTEMA GRAFICO
###   GGPLOT
library(ggplot2)

#grafica con puntos las variables "hwy" en funcion de "displ", para el dataframe "mpg"
qplot(displ, hwy, data = mpg)
#misma grafica anterior, pero se colorean los puntos según el valor de "drv"
qplot(displ, hwy, data = mpg, color = drv)
#misma grafica anterior, pero el simbolo varía según el valor de "drv"
qplot(displ, hwy, data = mpg, shape = drv)
#misma grafica anterior, pero con linea promedio
qplot(displ, hwy, data = mpg, geom = c("point","smooth"))
#grafica anterior parecida, pero con linea promedio en otra forma
qplot(displ, hwy, data = mpg, color = drv) + geom_smooth(method = "lm")

#separar en 3 graficos, segun valor de "drv", el ~ separa, a la izquierda el valor de cuantas filas de graficos, 
# a la derecha, el valor de cuantas columnas de graficos, en este cso 3 columnas
qplot(displ, hwy, data = mpg, facets = . ~ drv)
#grafica anterior parecida, pero con linea promedio en otra forma
qplot(displ, hwy, data = mpg, facets = . ~ drv) + geom_smooth(method = "lm")
#histograma, coloreado según "drv", fill es porque llena el area de abajo con el color
qplot(hwy, data = mpg, fill = drv)
#histograma, coloreado según "drv", suavizado por geom: density, "color" pinta la linea del contorno
qplot(hwy, data = mpg, color = drv, geom = "density")
# 3 histogramas, según valor de "drv", el ~ separa, a la izquierda el valor de cuantas filas de graficos, 
# a la derecha, el valor de cuantas columnas de graficos, en este cso 3 filas
qplot(hwy, data = mpg, facets = drv ~ ., binwith = 2)

#ggplot
  #GRAFICADO SIMPLE
g <- ggplot(mpg, aes(displ, hwy))
summary(g)
#imprimir grafico anterior con puntos
g + geom_point()
  # AGREGADO DE LINEAS O PROMEDIOS 
#agregar una capa con la media
g + geom_point() + geom_smooth()
#agregar una capa con la aproximación lineal
g + geom_point() + geom_smooth(method = "lm")
  # MULTIPLES GRAFICOS EN MISMA PAGINA
#cambiar a la forma de 3 graficos, segun "drv" y en cada uno, una capa con la aproximación lineal
g + geom_point() + facet_grid(. ~ drv) + geom_smooth(method = "lm")
#cambiar a la forma de 12 graficos, segun "drv" y "cyl" y en cada uno, una capa con la aproximación lineal
g + geom_point() + facet_grid(cyl ~ drv) + geom_smooth(method = "lm")
#imprimir grafico puntos, pero dandole formato a estos puntos, otro color, tamaño = 4 en vez de 1, alpha (transparencia) = 0.5
  # CAMBIAR ESTETICA DEL GRAFICO
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)
#otra forma, para separar por colores segun "drv"
g + geom_point(aes(color = drv), size = 4, alpha = 1/2)
# modificar etiquetas
g + geom_point(aes(color = drv)) + labs(title = "Miles per Galon vs Engine Size") + labs(x = "Motor Size (ltrs)", y = "Miles per galon")
# modificar estetica de linea promedio (size = 4, linetype = linea punteada, se = intervalo de confianza desactivado)
g + geom_point(aes(color = drv), size = 2, alpha = 1/2) + geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
# cambiar el theme del grafico
g + geom_point(aes(color = drv)) + theme_bw(base_family = "Times")

# anexo como clasificar una variable numerica y convertirla en factor
#cutpoint <- 
  # se establecen puntos de corte, en este caso se toma el minimo, el maximo y se divide en 3 tramos (inicio, 33%, 66%, 100%)
cutpoints <- seq(min(mpg$cty),max(mpg$cty), length = 4)
  # se construye una nueva columna con el valor según la clasificacion
mpg$ctyfactor <- cut(as.numeric(mpg$cty), cutpoints)
levels(mpg$ctyfactor)

# Ejemplo de grafica compleja
g <- ggplot(mpg, aes(displ, hwy))
g + 
  geom_point(alpha = 1/3) + 
  facet_grid(ctyfactor ~ drv) + 
  geom_smooth(method = "lm", se = FALSE, col = "steelblue") + 
  theme_bw( base_family = "Avenir", base_size = 10) + # base_size es el tamaño de la fuente
  labs( x = "Motor size") + 
  labs( y = "Miles per Galon") + 
  labs( title = "Miles per Galon vs Motor size with varying CTY and DRIVE")


#### SOBRE COLORES EN GRAFICOS
colors() # ofrece 657 colores y sus nombres, que podemos usar

#pero también está en el package 
library(graphics)

pal <- colorRamp(c("red","blue")) # genera una funcion pal, que ingresando un parametro con valor entre 0 y 1, devuelve un color
  # en este caso entre rojo y azul, siendo 0 el rojo y 1 el azul

# la funcion rgb
rgb(1,1,1,0.5) # permite definir un color indicando la cantidad de Rojo, Verde, Azul y opacidad

# otro paquete
library(RColorBrewer) # tiene secuencias definidas de colores para usar en graficas tipo factor



##################################################################
#EJEMPLO DE UN ANALISIS

#1 cargar datos en dataframe
pm0 <- read.table("data/daily_88101_1999.csv", header=TRUE, sep = ",")
pm1 <- read.table("data/daily_88101_2012.csv", header=TRUE, sep = ",")

# mirar primeros valores
head(pm0)
head(pm1)
# mirar dimensiones del dataframe
dim(pm0)
dim(pm1)
# se analiza un campo clave
x0 <- pm0$X1st.Max.Value
x1 <- pm1$X1st.Max.Value
# que tipo de datos es
class(x0)
# str para revisar estos datos
str(x0)
str(x1)
# summary para revisar estos datos
summary(x0)
summary(x1)
# revisar NAs, los necesito? para calcular la proporcion de NAs se puede hacer
mean(is.na(x0)) #calcula la cantidad de verdaderos sobre el total

#una grafica para ir viendo algo
boxplot(x0,x1)
boxplot(log10(x0),log10(x1)) # util usar log si hay gran variacion en los numeros

#en este ejemplo aparecen numeros negativos para x1, vamos a aislarlos
  negative <- x1 < 0  # vector con negativos
  sum(negative, na.rm = TRUE) #cuantos son

  #a ver si esos negativos ocurren en ciertas fechas?
  dates <- pm1$Date.Local
  dates <- as.Date(dates,"%Y-%m-%d") # se convierten a formato Date
  str(dates)
  hist(dates, "month") # se hace un histograma, notese que al poner "month" ya lo clasifica por mes
  hist(dates[negative],"month") # ahora un histograma con los negativos, para ver en que fecha son mas frecuentes
  # o se ve gran relacion, sigamos con otra cosa
  
# vamos a comparar las medidas del 1999 y con las mismas del 2012 a ver que se ve
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code,Site.Num))) # se extrae de pm0, sin repetidos, los de State.Code 36 (New York) y para esos, solo devuelve las columnas County.Code y Site.Num
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code,Site.Num)))
  #se acomodan las 2 columnas de tal forma que quede una sola de esta forma  "col1.col2"
  site0 <- paste(site0[,1],site0[,2], sep = ".")
  site1 <- paste(site1[,1],site1[,2], sep = ".")
  
  #ahora cuantos sitios estaban en el 1999 y siguen estando en el 2012?
  both <- intersect(site0,site1)
  
  #ahora contemos cuantas observaciones tiene cada uno de estos monitores 
    # se agrega una columna con el formato "col1.col2" de antes
  pm0$County.Site <- with(pm0, paste(County.Code,Site.Num, sep = "."))
  pm1$County.Site <- with(pm1, paste(County.Code,Site.Num, sep = "."))
  
    #ahora se puede filtrar la tabla pm0 o pm1 usando "both" que aislamos antes
  cnt0 <- subset(pm0, State.Code == 36 & County.Site %in% both)
  cnt1 <- subset(pm1, State.Code == 36 & County.Site %in% both)
  
    #separemos ahora estos dataframes por monitor, y se cuenta para cada grupo las filas
  sapply(split(cnt0, cnt0$County.Site),nrow)
  sapply(split(cnt1, cnt1$County.Site),nrow)
  
  # se ve que el que tiene mas o menos la misma cantidad de observaciones es el "63.2008" profundizamos con este
  pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.Num == 2008)
  pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.Num == 2008)

  #ahora graficamos para ver en este monitor, como fue el comportamiento en 1999 y en 2012
  dates0 <- as.Date(pm0sub$Date.Local,"%Y-%m-%d")
  x0sub <-  pm0sub$X1st.Max.Value
  plot(dates0,x0sub)  
  
  dates1 <- as.Date(pm1sub$Date.Local,"%Y-%m-%d")
  x1sub <-  pm1sub$X1st.Max.Value
  plot(dates1,x1sub)  
 
  # pero separadas las graficas no es facil comparar, hagamos un panel
  par(mfrow = c(1,2), mar= c(4,4,2,1))
  plot(dates0,x0sub,pch=20)
  abline(h=median(x0sub,na.rm = TRUE))
  plot(dates1,x1sub,pch=20)
  abline(h=median(x1sub,na.rm = TRUE))
  
  #mucho mejor, pero las graficas quedaron con distinto rango en los ejes "y" y puede confundir.
    #la funcion range, pasa el rango total de todo lo que le ponemos dentro
  rng <- range(x0sub,x1sub)
  plot(dates0,x0sub,pch=20,ylim = rng)
  abline(h=median(x0sub,na.rm = TRUE))
  plot(dates1,x1sub,pch=20,ylim = rng)
  abline(h=median(x1sub,na.rm = TRUE))
  
  #esto fue para un monitor en particular. 
# Ahora vamos a ver que pasa para cada ESTADO
  # la idea es calcular para cda estado el promedio,
  # ideal el uso de tapply
  mn0 <- with(pm0, tapply(X1st.Max.Value,State.Code,mean, na.rm= TRUE))
  mn1 <- with(pm1, tapply(X1st.Max.Value,State.Code,mean, na.rm= TRUE))
  str(mn0)
  class(mn0)
  summary(mn1)
  #creamos dataframes con estos datos
  d0 <- data.frame(State = names(mn0), mean = mn0)
  d1 <- data.frame(State = names(mn1), mean = mn1)
  #y hacemos un merge
  mrg <- merge(d0,d1, by = "State")
  head(mrg)  
  
  #ahora graficamos
    #se resetea
  par(mfrow = c(1, 1))
    #se inicia el motor de graficos con los puntos del 1999
  with(mrg, plot(rep(1999,51), mrg[,2], xlim = c(1998,2013)))
    #se agrega al grafico existente los puntos del 2012
  with(mrg, points(rep(2012,51), mrg[,3]))
    #se conectan los puntos correspondientes
    #se usa la funcion segments, entre coordenadas 1 y coordenadas 2
  segments(rep(1999,51),mrg[,2],rep(2012,51),mrg[,3])
  
  # FIN EJEMPLO DE UN ANALISIS
  ##################################################################

  