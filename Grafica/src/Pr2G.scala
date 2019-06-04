import scala.swing._
import scala.swing.event._

class PR2G extends SimpleSwingApplication {

  //Labels
  val labelTablero1 = new Label("0")
  val labelTablero2 = new Label("0")
  val labelTablero3 = new Label("0")
  val labelTablero4 = new Label("0")
  val labelTablero5 = new Label("0")
  val labelTablero6 = new Label("0")
  val labelTablero7 = new Label("0")
  val labelTablero8 = new Label("0")
  val labelTablero9 = new Label("0")
  val labelTablero10 = new Label("0")
  val labelTablero11 = new Label("0")
  val labelTablero12 = new Label("0")
  val labelTablero13 = new Label("0")
  val labelTablero14 = new Label("0")
  val labelTablero15 = new Label("0")
  val labelTablero16 = new Label("0")
  val labelPonerPuntos = new Label("0")
  val labelPonerPuntosMax = new Label("0")
  val labelPonerVidas = new Label("0")
  
  def top = new MainFrame {
    title = "Interfaz Gráfica 16384"
    preferredSize = new Dimension(250,250)

    val colorPanel = new Color(0x58ACFA)
    val colorButton = new Color(0x58ACFA)
    val colorButton2 = new Color(0xFF3333)

    val labelVidas = new Label("Vidas >>   ")
    val labelPuntos = new Label("Puntos >>   ")
    val labelPuntosMax = new Label("Puntos Max >>   ")


    val buttonQ = new Button("Salir")
    buttonQ.background_=(colorButton2)

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += buttonQ
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelVidas
        contents += labelPonerVidas
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelPuntos
        contents += labelPonerPuntos
      }
      background = colorPanel
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelPuntosMax
        contents += labelPonerPuntosMax
      }

      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelTablero1
        contents += labelTablero2
        contents += labelTablero3
        contents += labelTablero4
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelTablero5
        contents += labelTablero6
        contents += labelTablero7
        contents += labelTablero8
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelTablero9
        contents += labelTablero10
        contents += labelTablero11
        contents += labelTablero12
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        background = colorPanel
        contents += labelTablero13
        contents += labelTablero14
        contents += labelTablero15
        contents += labelTablero16
      }

      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
     buttonQ.reactions += {
      case ButtonClicked(b) => System.exit(0)
    }

  }
  
  //Cambia los valores de los labels referentes al tablero
  def cambiarTablero(lista:List[Int]){
    labelTablero1.text_=(""+lista(0)+"-")
    labelTablero2.text_=(""+lista(1)+"-")
    labelTablero3.text_=(""+lista(2)+"-")
    labelTablero4.text_=(""+lista(3))
    labelTablero5.text_=(""+lista(4)+"-")
    labelTablero6.text_=(""+lista(5)+"-")
    labelTablero7.text_=(""+lista(6)+"-")
    labelTablero8.text_=(""+lista(7))
    labelTablero9.text_=(""+lista(8)+"-")
    labelTablero10.text_=(""+lista(9)+"-")
    labelTablero11.text_=(""+lista(10)+"-")
    labelTablero12.text_=(""+lista(11))
    labelTablero13.text_=(""+lista(12)+"-")
    labelTablero14.text_=(""+lista(13)+"-")
    labelTablero15.text_=(""+lista(14)+"-")
    labelTablero16.text_=(""+lista(15))
  }
  
  //Cambia el valor del label referente a los puntos
  def cambiarPuntos(puntos:Int){
    labelPonerPuntos.text_=(""+puntos)
  }
  
  //Cambia el valor del label referente a la puntuación máxima
  def cambiarPuntosMax(puntos:Int){
    labelPonerPuntosMax.text_=(""+puntos)
  }
  
  //Cambia el valor referente al número de vidas
  def cambiarVida(vida:Int){
    labelPonerVidas.text_=(""+vida)
  }


}