import scala.io.StdIn.readLine
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.io.StdIn
import scala.io.Source
import java.io;


object Practica2 {

  def main(args: Array[String]) {
    println("------------------BIENVENIDO A 16384------------------")
    val vidas = 3
    val nivel = pedirNivel() //Pedimos nivel
    val modo = pedirModo() //Pedimos modo
    val tablero = generarNivel(nivel) //Generamos tablero inicial
    val puntaje = 0 //Inicializamos puntos
    val puntMax = cargarPuntMax() //Cargamos el score maximo
    println("·SCORE MAXIMO: " + puntMax + "\n")
    jugar(tablero, nivel, vidas, puntaje, modo, true, puntMax) //Bucle del juego
  }

  //Se guarda el score maximo en un txt externo
  def guardarPuntMax(score: Int) {
    val file = new File("./scoreMaximo.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("" + score)
    bw.close()
  }

  //Se carga el score maximo de un txt externo
  def cargarPuntMax(): Int = {
    if (scala.reflect.io.File("./scoreMaximo.txt").exists) {
      val rd = Source.fromFile("./scoreMaximo.txt").getLines.mkString
      val score = rd.toInt
      score
    } else {
      0
    }
  }

  //Se pide el nivel por pantalla
  def pedirNivel(): Int = {
    print("-Introduzca el nivel en el que desea jugar (1, 2, 3, 4): ")
    val input = readLine.toLowerCase.trim
    input match {
        case _ => try {
          val num = input.toInt
          if(num!=1 && num!=2 && num!=3 && num!=4) pedirNivel
          else num
        } catch {
          case _: NumberFormatException =>
            println("Incorrecto\n")
            pedirNivel()
        }
    }
  }

  //Se pide el modo por pantalla
  def pedirModo(): Int = {
    print("-Introduzca el modo en el que desea jugar (1-Manual, 2-Automatico): ")
    val input = readLine.toLowerCase.trim
    input match {
        case _ => try {
          val num = input.toInt
          if(num!=1 && num!=2) pedirModo
          else num
        } catch {
          case _: NumberFormatException =>
            println("Incorrecto\n")
            pedirModo()
        }
    }  
  }

  //Se genera el tablero inicial 
  def generarNivel(nivel: Int): List[Int] = {
    if (nivel == 1) generarTablero(16)
    else if (nivel == 2) generarTablero(81)
    else if (nivel == 3) generarTablero(196)
    else if (nivel == 4) generarTablero(289)
    else {
      println("VALOR INCORRECTO")
      generarNivel(pedirNivel())
    }
  }

  //Bucle del juego
  def jugar(tablero: List[Int], nivel: Int, vidas: Int, puntos: Int, modo: Int, primero: Boolean, puntMaximo: Int) {
    val inicial = rellenar(primero, nivel, tablero) //Se rellena el tablero inicial
    imprimirMatriz(inicial) //Se imprime por pantalla
    printMejorMovimiento(inicial, puntos) //Se imprime el mejor movimiento posible
    val mov = movManualAutomatico(modo, inicial) //Se pide o se genera el movimiento dependiendo del modo
    val juega = hacerMov(inicial, mov) //Se realiza el moviemiento
    val puntaje = calcPuntos(inicial, mov, puntos) //Se calcula los puntos del movimiento realizado
    if (finNiv(nivel, juega)) { //Si esta lleno el tablero
      println("\n\n**VIDA AGOTADA**\n\n\n")
      if (muerto(vidas - 1)) { //Si se han agotado las vidas
        println("\n\nGAME OVER")
        System.exit(0) 
      }
      else {
        println("Nuevo tablero:\n")
        jugar(generarNivel(nivel), nivel, vidas - 1, puntaje, modo, true, puntMaximo) //Se decrementa el numero de vidas y generamos un tablero nuevo
      }
    } else {
      if(puntaje > puntMaximo) { //Si se ha superado el score maximo
        guardarPuntMax(puntaje) //Guardamos los puntos
        println("·Número de vidas: " + vidas + "\n·Puntos: " + puntaje + "\n·Puntuacion Maxima: " + puntaje + "\n")
        jugar(juega, nivel, vidas, puntaje, modo, false, puntaje) //Convertimos los puntos a puntuación máxima
      }else{
        println("·Número de vidas: " + vidas + "\n·Puntos: " + puntaje + "\n·Puntuacion Maxima: " + puntMaximo + "\n")
        jugar(juega, nivel, vidas, puntaje, modo, false, puntMaximo)
      }
    }
  }

  //Retorna el movimiento a realiza
  def movManualAutomatico(modo: Int, lista: List[Int]): Int = {
    if (modo == 1) {//Si es modo manual
      val mov = pedirMov()
      mov
    } else movAutomatico(lista) //Si es modo automático
  }

  //Se pide el movimiento a realizar por teclado
  def pedirMov(): Int = {
    print("\n-Realice un movimiento (Arriba->8, Derecha->6, Abajo->2, Izquierda->4): ")
    val input = readLine.toLowerCase.trim
    input match {
        case _ => try {
          val num = input.toInt
          if(num!=2 && num!=4 && num!=8 && num!=6) pedirMov()
          else num
        } catch {
          case _: NumberFormatException =>
            println("Incorrecto\n")
            pedirMov()
        }
    }
  }

  //Genera un movimiento automático de forma aleatoria
  def movAutomatico(lista: List[Int]): Int = {
    val pos = mejorMovimiento(lista, 0)
    if(pos==2) generarAleatorioSemilla(List(6,4))
    else generarAleatorioSemilla(List(8,2))
  }

  //Rellena un tablero de semillas
  def rellenar(primero: Boolean, nivel: Int, tablero: List[Int]): List[Int] = {
    if (primero) rellenarInicial(nivel, tablero) //Si es el inicio de la jugada
    else rellenarTablero(nivel, tablero) //Si se rellena tras un movimiento
  }

  //Devuelve si el numero de semillas es 0
  def muerto(vidas: Int): Boolean = vidas match {
    case 0     => true
    case vidas => false
  }

  //Devuelve si en el tablero dado no se pueden insertar más semillas
  def finNiv(nivel: Int, tablero: List[Int]): Boolean = nivel match {
    case 1 => lleno(1, tablero)
    case 2 => lleno(3, tablero)
    case 3 => lleno(5, tablero)
    case 4 => lleno(6, tablero)
  }

  //Devuelve si está lleno el tablero (si no se pueden insertar más semillas)
  def lleno(numValores: Int, tablero: List[Int]): Boolean = {
    if (numValores > (numCeros(tablero, 0))) true
    else false
  }

  //Devuelve el numero de 0s que hay en una lista
  def numCeros(tablero: List[Int], cont: Int): Int = {
    if (tablero.head == 0) {
      if (tablero.length == 1) cont + 1
      else numCeros(tablero.tail, cont + 1)
    } else {
      if (tablero.length == 1) cont
      else numCeros(tablero.tail, cont)
    }
  }

  //Realiza el movimiento que se le pasa por parámetro, devolviendo la matriz movida
  def hacerMov(tablero: List[Int], mov: Int): List[Int] = mov match {
    case 8 => irArriba(tablero)
    case 4 => irIzquierda(tablero)
    case 2 => irAbajo(tablero)
    case 6 => irDerecha(tablero)
  }

  //Devuelve los puntos al pasarle un movimiento dado
  def calcPuntos(tablero: List[Int], mov: Int, puntos: Int): Int = mov match {
    case 8 => puntArriba(tablero, puntos)
    case 4 => puntIzquierda(tablero, puntos)
    case 2 => puntAbajo(tablero, puntos)
    case 6 => puntDerecha(tablero, puntos)
  }

  //Rellena el tablero con semillas posteriormente al realizar un movimiento
  def rellenarTablero(nivel: Int, tablero: List[Int]): List[Int] = nivel match {
    case 1 => rellenarTablero(tablero, 1, 1) //1 semillas, nivel 1
    case 2 => rellenarTablero(tablero, 3, 2) //3 semillas, nivel 2
    case 3 => rellenarTablero(tablero, 5, 3) //5 semillas, nivel 3
    case 4 => rellenarTablero(tablero, 6, 4) //6 semillas, nivel 4
  }

  //Rellena el tablero al principio de jugar
  def rellenarInicial(nivel: Int, tablero: List[Int]): List[Int] = nivel match {
    case 1 => rellenarTablero(tablero, 2, 1) //2 semillas, nivel 1
    case 2 => rellenarTablero(tablero, 4, 2) //4 semillas, nivel 2
    case 3 => rellenarTablero(tablero, 6, 3) //6 semillas, nivel 3
    case 4 => rellenarTablero(tablero, 6, 4) //6 semillas, nivel 4
  }

  //Devuelve el valor de una posicion de una lista dada
  def valorCas(matriz: List[Int], casilla: Int): Int = {
    if (casilla == 1) matriz.head
    else valorCas(matriz.tail, casilla - 1)
  }

  //Devuelve un numero aleatorio de una lista dada
  def generarAleatorioSemilla(lista: List[Int]): Int = {
    val random = util.Random
    val pos = random.nextInt(lista.length) + 1
    val numero = valorCas(lista, pos)
    return numero
  }

  //Devuelve un número aleatorio dado un rango (en este caso el tamaño del tablero)
  def generarAleatorioTablero(tam: Int): Int = {
    val random = util.Random
    val pos = random.nextInt(tam) + 1
    return pos
  }

  //Genera una lista de tamaño 'tam' rellena con ceros
  def generarTablero(tam: Int): List[Int] = {
    if (tam == 0) Nil
    else 0 :: generarTablero(tam - 1)
  }

  //Introduce un valor dado en una posicion dada en una lista
  def ponerTablero(casilla: Int, valor: Int, matriz: List[Int]): List[Int] = {
    if (matriz.isEmpty) Nil
    else if (casilla == 1) valor :: matriz.tail
    else matriz.head :: ponerTablero(casilla - 1, valor, matriz.tail)
  }

  //Devuelve la lista de los valores de las semillas de cada dificultad
  def listaAleatoriosDificultad(dificultad: Int): List[Int] = dificultad match {
    case 1 => List(2)
    case 2 => List(2, 4)
    case _ => List(2, 4, 8)
  }

  //Rellena el tablero de semillas dado su numero y dificultad
  def rellenarTablero(matriz: List[Int], numCasillas: Int, dificultad: Int): List[Int] = {
    if (numCasillas == 0) matriz
    else if (numCasillas > matriz.length) matriz
    else {
      val casilla = generarAleatorioTablero(matriz.length)
      val valor = generarAleatorioSemilla(listaAleatoriosDificultad(dificultad))
      val valor_cas = valorCas(matriz, casilla)
      //Si el valor de la casilla aleatoria es 0, se pone en el tablero y disminuye el contador de semillas
      //a introducir. En distinto caso, se llama recursivamente a la funcion sin disminuir el contador
      if (valor_cas == 0) {
        rellenarTablero(ponerTablero(casilla, valor, matriz), numCasillas - 1, dificultad)
      } else {
        rellenarTablero(matriz, numCasillas, dificultad)
      }
    }
  }

  //Cambia el valor del primer elemento coincidente de una lista dada.
  //Su valor lo transforma por su doble (simulación suma en el tablero)
  def sumarDerecha_coincidente(matriz: List[Int], valor_casilla: Int): List[Int] = {
    if (valor_casilla == matriz.head) {
      (valor_casilla * 2) :: matriz.tail
    } else {
      matriz.head :: sumarDerecha_coincidente(matriz.tail, valor_casilla)
    }
  }

  //Devuelve el numero de casillas con valor 'valorElemento' que se encuentran en la primera fila
  def paresIguales_D(numColumnas: Int, matriz: List[Int], cont: Int, valorElemento: Int): Int = {
    val valor_ev = matriz.head
    if (valorElemento == valor_ev) {
      if (limite(matriz, numColumnas)) cont + 1
      else paresIguales_D(numColumnas, matriz.tail, cont + 1, valorElemento)
    } else if (valor_ev != 0) { //Si es distinto de 0, se devuelve directamente contador
      cont
    } else {
      if (limite(matriz, numColumnas)) cont
      else paresIguales_D(numColumnas, matriz.tail, cont, valorElemento)
    }
  }

  //Devuelve el número de 0s que hay en la primera fila de una lista (matriz)
  def contCeros(numColumnas: Int, matriz: List[Int], cont: Int): Int = {
    val valor_ev = matriz.head
    if (valor_ev == 0) {
      if (limite(matriz, numColumnas)) cont + 1
      else contCeros(numColumnas, matriz.tail, cont + 1)
    } else {
      if (limite(matriz, numColumnas)) cont
      else contCeros(numColumnas, matriz.tail, cont)
    }
  }

  //Devuelve el tablero con las casilla sumadas correctamente
  def sumarDerecha(matrizOriginal: List[Int], matrizAuxiliar: List[Int], numColumnas: Int): List[Int] = {
    if (matrizOriginal.length == 0) Nil
    else if (limite(matrizOriginal, numColumnas)) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
    else {
      val valor_cas = matrizOriginal.head
      val valor_cas_s = (matrizOriginal.tail).head
      //Si el valor de la casilla a estudiar es 0, o su adyacente es de distinto valor, no se suma
      if (matrizOriginal.head == 0 || (valor_cas_s != 0 && valor_cas_s != valor_cas)) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else {
        //Se calcula el numero de casillas de igual valor en la misma fila (adyacentes)
        val num_coincidentes = paresIguales_D(numColumnas, matrizOriginal.tail, 1, matrizOriginal.head)
        //Si el numero de coincidentes es impar, se añade a la matriz auxiliar el valor sumado en la posicion correspondiente
        //y se añade un 0 en la posicion evaluada
        if (num_coincidentes % 2 != 0) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
        else {
          0 :: sumarDerecha(matrizOriginal.tail, sumarDerecha_coincidente(matrizAuxiliar.tail, valor_cas), numColumnas)
        }
      }
    }
  }

  //Devuelve si la cabeza de la lista se encuentra en el limite de la matriz
  def limite(matriz: List[Int], numColumnas: Int): Boolean = {
    if ((matriz.length - 1) % numColumnas == 0) true
    else false
  }

  //Mueve las casillas del tablero hacia la derecha
  def moverDerecha(matrizOriginal: List[Int], matrizAuxiliar: List[Int], numColumnas: Int): List[Int] = {
    //Si el tablero esta vacio
    if (matrizOriginal.length == 0) Nil
    //Si es el limite de una fila
    else if (limite(matrizOriginal, numColumnas)) {
      //Si es distinto de 0 el valor en la matriz original, se añade el elemento limite de la matriz original
      //En distinto caso, se añade el valor limite de la matriz auxiliar   
      if (matrizOriginal.head != 0) matrizOriginal.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
    } else {
      val valor_cas = matrizOriginal.head
      //Si el valor de la casilla es 0, no se hace nada
      if (valor_cas == 0) matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else {
        //Se calculan el numero de 0 de la fila
        val num_coincidentes = contCeros(numColumnas, matrizOriginal.tail, 0)        
        //Si no hay ningun 0, no se hace nada
        //En caso distinto, se añade a la matriz auxiliar el valor en la posicion referenciada por el conteo de 0s
        if (num_coincidentes == 0) {
          matrizOriginal.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
        } else {
          matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, ponerTablero(num_coincidentes, valor_cas, matrizAuxiliar.tail), numColumnas)
        }
      }
    }
  }

  //Invierte los elementos de una matriz
  def invertir(matriz: List[Int]): List[Int] = {
    if (matriz == Nil) matriz
    else invertir(matriz.tail) ::: List(matriz.head)
  }

  //Devuelve la matriz transpuesta de una matriz dada
  def transpuesta(lista: List[Int]): List[Int] = {
    transpuesta_aux1(lista, 1, raiz(lista.length))
  }

  //Cambia las columnas por las filas de una lista dada
  def transpuesta_aux1(lista: List[Int], posActual: Int, numColumnas: Int): List[Int] = {
    if (posActual > numColumnas) Nil
    else transpuesta_aux2(lista, numColumnas, posActual) ::: transpuesta_aux1(lista, posActual + 1, numColumnas)
  }

  //Añade los elementos de un numero de columna dado a la fila del mismo numero
  def transpuesta_aux2(lista: List[Int], numColumnas: Int, posActual: Int): List[Int] = {
    if (posActual > numColumnas * (numColumnas - 1)) valorCas(lista, posActual) :: Nil
    else {
      valorCas(lista, posActual) :: transpuesta_aux2(lista, numColumnas, posActual + numColumnas)
    }
  }

  //Se realiza el movimiento hacia la derecha
  def irDerecha(lista: List[Int]): List[Int] = {
    val lista_ceros = generarTablero(lista.length) //Se crea una lista auxiliar de ceros con el tamaño de la lista original
    val suma = sumarDerecha(lista, lista, raiz(lista.length)) //Se realiza la suma de las casillas
    val mover = moverDerecha(suma, lista_ceros, raiz(lista.length)) //Se realiza el movimiento hacia la derecha

    return mover
  }

  //Se realiza el movimiento hacia la izquierda
  def irIzquierda(lista: List[Int]): List[Int] = {
    val inversa = invertir(lista) //Se invierte la lista
    val mover = irDerecha(inversa) //Se realiza el movimiento hacia la derecha
    val inversa2 = invertir(mover) //Se invierte la matriz resultado

    return inversa2
  }

  //Se realiza el moviento del tablero hacia abajo
  def irAbajo(lista: List[Int]): List[Int] = {
    val trans = transpuesta(lista) //Se transpone la lista (matriz)
    val mover = irDerecha(trans) //Se realiza el movimiento hacia la derecha
    val trans2 = transpuesta(mover) //Se transpone la matriz resultado

    return trans2
  }

  //Se realiza el movimiento del tablero hacia arriba
  def irArriba(lista: List[Int]): List[Int] = {
    val trans = transpuesta(lista) //Se transpone la lista (matriz)
    val mover = irIzquierda(trans) //Se realiza el movimiento hacia la izquierda
    val trans2 = transpuesta(mover) //Se transpone la matriz resultado

    return trans2
  }

  //Devuelve el total de puntos que se realizaría moviendo el tablero hacia la derecha
  def puntosDerecha(matrizOriginal: List[Int], numColumnas: Int, puntos: Int): Int = {
    //Si el tablero está vacío
    if (matrizOriginal.length == 0) puntos 
    //Si es el limite de una fila
    else if (limite(matrizOriginal, numColumnas)) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
    else {
      val valor_cas = matrizOriginal.head
      val valor_cas_s = (matrizOriginal.tail).head
      //Si el valor actual de la casilla es 0 o el adyacente a el es de distinto valor
      if (valor_cas == 0 || (valor_cas_s != 0 && valor_cas_s != valor_cas)) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
      else {
        //Calculamos el numero de casilla con mismo valor de su misma fila,siendo adyacentes
        val num_coincidentes = paresIguales_D(numColumnas, matrizOriginal.tail, 1, matrizOriginal.head)
        //Si no es par, no se suma
        if (num_coincidentes % 2 != 0) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
        //Si es par, se añaden a los puntos el doble del valor de la casilla
        else (valor_cas * 2) + puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
      }
    }
  }

  //Devuelve la puntuacion moviendo el tablero hacia la derecha
  def puntDerecha(lista: List[Int], puntos: Int): Int = {
    val score = puntosDerecha(lista, raiz(lista.length), puntos)
    return score
  }

  //Devuelve la puntuacion moviendo el tablero hacia la izquierda
  def puntIzquierda(lista: List[Int], puntos: Int): Int = {
    val inversa = invertir(lista)
    val score = puntDerecha(inversa, puntos)
    return score
  }

  //Devuelve la puntuacion moviendo el tablero hacia abajo
  def puntAbajo(lista: List[Int], puntos: Int): Int = {
    val trans = transpuesta(lista)
    val score = puntDerecha(trans, puntos)
    return score
  }

  //Devuelve la puntuacion moviendo el tablero hacia arriba
  def puntArriba(lista: List[Int], puntos: Int): Int = {
    val trans = transpuesta(lista)
    val score = puntIzquierda(trans, puntos)
    return score
  }

  //Devuelve una lista con los puntos que se pueden realizar con cualquier movimiento
  def listaPuntajes(lista: List[Int], puntos: Int): List[Int] = {
    val scoreD = puntDerecha(lista, puntos)
    val scoreI = puntIzquierda(lista, puntos)
    val scoreA = puntArriba(lista, puntos)
    val scoreAb = puntAbajo(lista, puntos)

    return List(scoreD, scoreI, scoreA, scoreAb)
  }

  //Devuelve la posicion de una lista cuyo valor es el maximo
  def maxLista(lista: List[Int], punt_max: Int, pos: Int, pos_aux: Int): Int = {
    if (lista.isEmpty) pos
    else {
      val actual = lista.head
      if (actual >= punt_max) maxLista(lista.tail, actual, pos_aux, pos_aux + 1)
      else maxLista(lista.tail, punt_max, pos, pos_aux + 1)
    }
  }

  //Devuelve un numero identificativo del mejor movimiento
  def mejorMovimiento(lista: List[Int], totalPuntos: Int): Int = {
    val pos = maxLista(listaPuntajes(lista, totalPuntos), 0, 1, 1)
    return pos /*Derecha,Izquierda,Arriba,Abajo*/
  }

  //Muestra por pantalla el mejor movimiento
  def printMejorMovimiento(lista: List[Int], totalPuntos: Int) {
    val pos = mejorMovimiento(lista, totalPuntos)
    if (pos == 1) print("\n#Mejor movimiento Derecha-\n")
    else if (pos == 2) print("\n#Mejor movimiento Izquierda-Derecha\n")
    else if (pos == 3) print("\n#Mejor movimiento Arriba\n")
    else print("\n#Mejor movimiento Abajo-Arriba\n")
  }

  //Imprime el tablero con los separadores y marcadores
  def imprimirMatriz(tablero: List[Int]): Unit = {
    val cMax = cifras(valorMax(maxList(0, tablero), raiz(tablero.length)))

    imprimirLineaSeparadora(1, raiz(tablero.length), cMax)
    imprimirSeparador(1, raiz(tablero.length), cMax)
    imprimirMatrizAux(tablero, tablero.length, cMax, 1)
  }

  //Funcion auxiliar que imprime el tablero
  def imprimirMatrizAux(tablero: List[Int], size: Int, cMax: Int, counter: Int): Unit = {
    if (tablero.length < raiz(size)) Nil
    else {
      val spaces = cMax - cifras(counter)
      imprimirEspacios(spaces)
      print(counter + "-|")
      printList(toma(raiz(size), tablero), cMax)
      imprimirMatrizAux(deja(raiz(size), tablero), size, cMax, counter + 1)
    }
  }

  //Imprime el numero del marcador dado el tamaño de la lista
  def imprimirMarcador(num: Int) {
    if (num == 1) printf("-")
    else {
      printf("-")
      imprimirMarcador(num - 1)
    }
  }

  //Imprime el separador entre los marcodres de la lista y el tablero en si
  def imprimirSeparador(contador: Int, tam: Int, cMax: Int) {
    if (contador == tam) {
      imprimirMarcador(cMax)
      println(" ")
    } else {
      if (contador == 1) {
        imprimirEspacios(cMax + 2)
        imprimirMarcador(cMax)
        print(" ")
        imprimirSeparador(contador + 1, tam, cMax)
      } else {
        imprimirMarcador(cMax)
        print(" ")
        imprimirSeparador(contador + 1, tam, cMax)
      }
    }
  }

  //Imprime la linea separadora dado el tamaño de la lista
  def imprimirLineaSeparadora(contador: Int, tam: Int, cMax: Int) {
    val espacios = cMax - cifras(contador)
    if (contador == tam) {
      imprimirEspacios(espacios)
      println(contador + "|")
    } else {
      //Si es el primero
      if (contador == 1) {
        imprimirEspacios(cMax * 2 + 1)
        print(contador + "|")
        imprimirLineaSeparadora(contador + 1, tam, cMax)
      } else {
        imprimirEspacios(espacios)
        print(contador + "|")
        imprimirLineaSeparadora(contador + 1, tam, cMax)
      }
    }
  }

  //Imprime una lista dado el numero de espacios del maximo valor
  def printList(lista: List[Int], cMax: Int): Unit = {
    val espacios = cMax - cifras(lista.head)
    if (lista.length == 1) {
      if (lista.head != 0) {
        imprimirEspacios(espacios)
        println(lista.head + "|")
      } else {
        imprimirEspacios(espacios + 1)
        println("|")
      }
    } else {
      if (lista.head != 0) {
        imprimirEspacios(espacios)
        print(lista.head + "|")
      } else {
        imprimirEspacios(espacios + 1)
        print("|")
      }
      printList(lista.tail, cMax)
    }
  }
  
  //Devuelve el valor maximo de una lista
  def maxList(max: Int, list: List[Int]): Int = {
    if (list.length == 0) max
    else if (list.head >= max) {
      maxList(list.head, list.tail)
    } else {
      maxList(max, list.tail)
    }
  }

  //Devuelve el valor maximo de dos numero n y m
  def valorMax(n: Int, m: Int): Int =
    if (n > m) n
    else m

  //Devuelve el numero de cifras de un numero n
  def cifras(n: Int): Int = {
    if (n / 10 > 0) 1 + cifras(n / 10)
    else 1
  }

  //Imprime por pantalla n espacios
  def imprimirEspacios(n: Int): Unit = {
    if (n == 0) printf("")
    else {
      printf(" ")
      imprimirEspacios(n - 1)
    }
  }

  //Toma los n primeros elementos de una lista
  def toma(n: Int, l: List[Int]): List[Int] = {
    if (n == 0) Nil
    else l.head :: toma(n - 1, l.tail)
  }

  //Devuelve una lista de quitarle los primeros n elementos
  def deja(n: Int, l: List[Int]): List[Int] = {
    if (n == 0) l
    else deja(n - 1, l.tail)
  }

  //Devuelve la raiz de un numero dado
  def raiz(n: Int) =
    Math.sqrt(n).toInt
}