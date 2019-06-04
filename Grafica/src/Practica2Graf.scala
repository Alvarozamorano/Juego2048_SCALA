
import scala.io.StdIn.readLine
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.io.StdIn
import scala.io.Source
import java.io

object Practica2Graf {
  def main(args: Array[String]) {
    val interfaz = new PR2G //Instanciamos la clase refererida a la interfaz
    interfaz.startup(args)
    val vidas = 3
    val nivel = 1
    val tablero = generarNivel()
    val puntaje = 0
    val puntMax = cargarPuntMax()
    interfaz.cambiarPuntosMax(puntMax) //Cambiamos el número de puntuación máxima
    interfaz.cambiarVida(vidas) //Cambiamos las vidas
    jugar(tablero, vidas, puntaje, true, puntMax,interfaz)
  }

  def guardarPuntMax(score: Int) {
    val file = new File("./scoreMaximo.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("" + score)
    bw.close()
  }

  def cargarPuntMax(): Int = {
    if (scala.reflect.io.File("./scoreMaximo.txt").exists) {
      val rd = Source.fromFile("./scoreMaximo.txt").getLines.mkString
      val score = rd.toInt
      score
    } else {
      0
    }
  }


  def generarNivel(): List[Int] = {
    generarTablero(16)
  }

  def jugar(tablero: List[Int], vidas: Int, puntos: Int, primero: Boolean, puntMaximo: Int,interfaz:PR2G) {
    val inicial = rellenar(primero,tablero)
    interfaz.cambiarTablero(inicial) //Cambiamos los elementos del tablero
    val mov = movManualAutomatico(inicial)
    val juega = hacerMov(inicial, mov)
    val puntaje = calcPuntos(inicial, mov, puntos)
    if (finNiv(juega)) {
      interfaz.cambiarPuntos(puntos) //Cambiamos los puntos
      interfaz.cambiarPuntosMax(puntMaximo) //Cambiamos la puntuación máxima
      interfaz.cambiarVida(vidas-1) //Cambiamos las vidas
      if (muerto(vidas - 1)) System.exit(0)
      else jugar(generarNivel(), vidas - 1, puntaje, true, puntMaximo,interfaz)
    } else {
      if(puntaje > puntMaximo) {
        guardarPuntMax(puntaje) //Cambiamos los puntos
        interfaz.cambiarPuntos(puntaje) //Cambiamos los puntos
        interfaz.cambiarPuntosMax(puntaje) //Cambiamos la puntuación máxima
        jugar(juega, vidas, puntaje, false, puntaje,interfaz)
      }else{
        interfaz.cambiarPuntos(puntaje)
        jugar(juega, vidas, puntaje, false, puntMaximo,interfaz)
      }
    }
  }

  def movManualAutomatico(lista: List[Int]): Int = {
      val mov = pedirMov()
      mov
  }

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
  
  def rellenar(primero: Boolean, tablero: List[Int]): List[Int] = {
    if (primero) rellenarInicial(tablero)
    else rellenarTablero(tablero)
  }

  def muerto(vidas: Int): Boolean = vidas match {
    case 0     => true
    case vidas => false
  }

  def finNiv(tablero: List[Int]): Boolean ={
    lleno(1, tablero)
  }

  def lleno(numValores: Int, tablero: List[Int]): Boolean = {
    if (numValores > (numCeros(tablero, 0))) true
    else false
  }

  /*Cuenta numero de ceros que hay en la matriz*/
  def numCeros(tablero: List[Int], cont: Int): Int = {
    val valor = tablero.head
    if (valor == 0) {
      if (tablero.length == 1) cont + 1
      else numCeros(tablero.tail, cont + 1)
    } else {
      if (tablero.length == 1) cont
      else numCeros(tablero.tail, cont)
    }
  }

  def hacerMov(tablero: List[Int], mov: Int): List[Int] = mov match {
    case 8 => irArriba(tablero)
    case 4 => irIzquierda(tablero)
    case 2 => irAbajo(tablero)
    case 6 => irDerecha(tablero)
  }

  def calcPuntos(tablero: List[Int], mov: Int, puntos: Int): Int = mov match {
    case 8 => puntArriba(tablero, puntos)
    case 4 => puntIzquierda(tablero, puntos)
    case 2 => puntAbajo(tablero, puntos)
    case 6 => puntDerecha(tablero, puntos)
  }

  def rellenarTablero(tablero: List[Int]): List[Int] ={
    rellenarTablero(tablero, 1, 1)
  }

  def rellenarInicial(tablero: List[Int]): List[Int] ={
    rellenarTablero(tablero, 2, 1)
  }

  def valorCas(matriz: List[Int], casilla: Int): Int = {
    if (casilla == 1) matriz.head
    else valorCas(matriz.tail, casilla - 1)
  }

  def generarAleatorioSemilla(lista: List[Int]): Int = {
    val random = util.Random
    val pos = random.nextInt(lista.length) + 1
    val numero = valorCas(lista, pos)
    return numero
  }

  def generarAleatorioTablero(tam: Int): Int = {
    val random = util.Random
    val pos = random.nextInt(tam) + 1
    return pos
  }

  def generarTablero(tam: Int): List[Int] = {
    if (tam == 0) Nil
    else 0 :: generarTablero(tam - 1)
  }

  def ponerTablero(casilla: Int, valor: Int, matriz: List[Int]): List[Int] = {
    if (matriz.isEmpty) Nil
    else if (casilla == 1) valor :: matriz.tail
    else matriz.head :: ponerTablero(casilla - 1, valor, matriz.tail)
  }

  def listaAleatoriosDificultad(dificultad: Int): List[Int] = dificultad match {
    case 1 => List(2)
    case 2 => List(2, 4)
    case _ => List(2, 4, 8)
  }

  def rellenarTablero(matriz: List[Int], numCasillas: Int, dificultad: Int): List[Int] = {
    if (numCasillas == 0) matriz
    else if (numCasillas > matriz.length) matriz
    else {
      val casilla = generarAleatorioTablero(matriz.length)
      val valor = generarAleatorioSemilla(listaAleatoriosDificultad(dificultad))
      val valor_cas = valorCas(matriz, casilla)
      if (valor_cas == 0) {
        rellenarTablero(ponerTablero(casilla, valor, matriz), numCasillas - 1, dificultad)
      } else {
        rellenarTablero(matriz, numCasillas, dificultad)
      }
    }
  }

  def sumarDerecha_coincidente(matriz: List[Int], valor_casilla: Int): List[Int] = {
    if (valor_casilla == matriz.head) {
      (valor_casilla * 2) :: matriz.tail
    } else {
      matriz.head :: sumarDerecha_coincidente(matriz.tail, valor_casilla)
    }
  }

  def paresIguales_D(numColumnas: Int, matriz: List[Int], cont: Int, valorElemento: Int): Int = {
    val valor_ev = matriz.head
    if (valorElemento == valor_ev) {
      if (limite(matriz, numColumnas)) cont + 1
      else paresIguales_D(numColumnas, matriz.tail, cont + 1, valorElemento)
    } else if (valor_ev != 0) {
      cont
    } else {
      if (limite(matriz, numColumnas)) cont
      else paresIguales_D(numColumnas, matriz.tail, cont, valorElemento)
    }
  }

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

  def sumarDerecha(matrizOriginal: List[Int], matrizAuxiliar: List[Int], numColumnas: Int): List[Int] = {
    if (matrizOriginal.length == 0) Nil
    else if (limite(matrizOriginal, numColumnas)) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
    else {
      val valor_cas = matrizOriginal.head
      val valor_cas_s = (matrizOriginal.tail).head
      if (matrizOriginal.head == 0 || (valor_cas_s != 0 && valor_cas_s != valor_cas)) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else {
        val num_coincidentes = paresIguales_D(numColumnas, matrizOriginal.tail, 1, matrizOriginal.head)
        if (num_coincidentes % 2 != 0) matrizAuxiliar.head :: sumarDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
        else {
          0 :: sumarDerecha(matrizOriginal.tail, sumarDerecha_coincidente(matrizAuxiliar.tail, valor_cas), numColumnas)
        }
      }
    }
  }

  def limite(matriz: List[Int], numColumnas: Int): Boolean = {
    if ((matriz.length - 1) % numColumnas == 0) true
    else false
  }

  def moverDerecha(matrizOriginal: List[Int], matrizAuxiliar: List[Int], numColumnas: Int): List[Int] = {
    if (matrizOriginal.length == 0) Nil
    else if (limite(matrizOriginal, numColumnas)) {
      if (matrizOriginal.head != 0) matrizOriginal.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
    } else {
      val valor_cas = matrizOriginal.head
      if (valor_cas == 0) matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
      else {
        val num_coincidentes = contCeros(numColumnas, matrizOriginal.tail, 0)
        if (num_coincidentes == 0) {
          matrizOriginal.head :: moverDerecha(matrizOriginal.tail, matrizAuxiliar.tail, numColumnas)
        } else {
          matrizAuxiliar.head :: moverDerecha(matrizOriginal.tail, ponerTablero(num_coincidentes, valor_cas, matrizAuxiliar.tail), numColumnas)
        }
      }
    }
  }

  def invertir(matriz: List[Int]): List[Int] = {
    if (matriz == Nil) matriz
    else invertir(matriz.tail) ::: List(matriz.head)
  }

  def transpuesta(lista: List[Int]): List[Int] = {
    transpuesta_aux1(lista, 1, raiz(lista.length))
  }

  def transpuesta_aux1(lista: List[Int], posActual: Int, numColumnas: Int): List[Int] = {
    if (posActual > numColumnas) Nil
    else transpuesta_aux2(lista, numColumnas, posActual) ::: transpuesta_aux1(lista, posActual + 1, numColumnas)
  }

  def transpuesta_aux2(lista: List[Int], numColumnas: Int, posActual: Int): List[Int] = {
    if (posActual > numColumnas * (numColumnas - 1)) valorCas(lista, posActual) :: Nil
    else {
      valorCas(lista, posActual) :: transpuesta_aux2(lista, numColumnas, posActual + numColumnas)
    }
  }

  def irDerecha(lista: List[Int]): List[Int] = {
    val lista_ceros = generarTablero(lista.length)
    val suma = sumarDerecha(lista, lista, raiz(lista.length))
    val mover = moverDerecha(suma, lista_ceros, raiz(lista.length))

    return mover
  }

  def irIzquierda(lista: List[Int]): List[Int] = {
    val inversa = invertir(lista)
    val mover = irDerecha(inversa)
    val inversa2 = invertir(mover)

    return inversa2
  }

  def irAbajo(lista: List[Int]): List[Int] = {
    val trans = transpuesta(lista)
    val mover = irDerecha(trans)
    val trans2 = transpuesta(mover)

    return trans2
  }

  def irArriba(lista: List[Int]): List[Int] = {
    val trans = transpuesta(lista)
    val mover = irIzquierda(trans)
    val trans2 = transpuesta(mover)

    return trans2
  }

  def puntosDerecha(matrizOriginal: List[Int], numColumnas: Int, puntos: Int): Int = {
    if (matrizOriginal.length == 0) puntos
    else if (limite(matrizOriginal, numColumnas)) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
    else {
      val valor_cas = matrizOriginal.head
      val valor_cas_s = (matrizOriginal.tail).head
      if (valor_cas == 0 || (valor_cas_s != 0 && valor_cas_s != valor_cas)) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
      else {
        val num_coincidentes = paresIguales_D(numColumnas, matrizOriginal.tail, 1, matrizOriginal.head)
        if (num_coincidentes % 2 != 0) puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
        else (valor_cas * 2) + puntosDerecha(matrizOriginal.tail, numColumnas, puntos)
      }
    }
  }

  def puntDerecha(lista: List[Int], puntos: Int): Int = {
    val score = puntosDerecha(lista, raiz(lista.length), puntos)
    return score
  }

  def puntIzquierda(lista: List[Int], puntos: Int): Int = {
    val inversa = invertir(lista)
    val score = puntDerecha(inversa, puntos)
    return score
  }

  def puntAbajo(lista: List[Int], puntos: Int): Int = {
    val trans = transpuesta(lista)
    val score = puntDerecha(trans, puntos)
    return score
  }

  def puntArriba(lista: List[Int], puntos: Int): Int = {
    val trans = transpuesta(lista)
    val score = puntIzquierda(trans, puntos)
    return score
  }

  def raiz(n: Int) =
    Math.sqrt(n).toInt
}