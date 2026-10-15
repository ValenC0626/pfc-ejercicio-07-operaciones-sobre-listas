package taller

// Un elemento de una lista anidada: un número o una sublista.
sealed trait Elemento
case class Valor(n: Int) extends Elemento
case class Sub(elementos: List[Elemento]) extends Elemento

// Una corrida es un valor y cuántas veces seguidas aparece.
case class Corrida(valor: Int, veces: Int)

class Ejercicio {

  // Punto 1. Recursión con if/else y head, tail e isEmpty.

  /** Cuántos elementos tiene la lista. */
  def longitud(l: List[Int]): Int = {
    if (l.isEmpty) 0
    else 1 + longitud(l.tail)
  }

  /** La lista con los elementos en orden inverso. */
  def invertir(l: List[Int]): List[Int] = {
    if (l.isEmpty) Nil
    else concatenar(invertir(l.tail), List(l.head))
  }

  /** Los elementos de l1 seguidos de los de l2. */
  def concatenar(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1.isEmpty) l2
    else l1.head :: concatenar(l1.tail, l2)
  }

  /** El último elemento. Falla con una lista vacía. */
  def ultimo(l: List[Int]): Int = {
    if (l.isEmpty)
      throw new NoSuchElementException("la lista está vacía")
    else if (l.tail.isEmpty)
      l.head
    else
      ultimo(l.tail)
  }

  /** Los primeros n elementos. */
  def tomar(l: List[Int], n: Int): List[Int] = {
    if (n <= 0 || l.isEmpty) Nil
    else l.head :: tomar(l.tail, n - 1)
  }

  /** La lista sin sus primeros n elementos. */
  def soltar(l: List[Int], n: Int): List[Int] = {
    if (n <= 0 || l.isEmpty) l
    else soltar(l.tail, n - 1)
  }

  /** El elemento en la posición n, contando desde cero. */
  def enPosicion(l: List[Int], n: Int): Int = {
    if (n < 0 || n >= longitud(l))
      throw new IndexOutOfBoundsException(n.toString)
    else if (n == 0)
      l.head
    else
      enPosicion(l.tail, n - 1)
  }

  // Punto 2. Recursión con match.

  /** La suma de los elementos; 0 para la lista vacía. */
  def sumar(l: List[Int]): Int = {
    l match {
      case Nil => 0
      case x :: xs => x + sumar(xs)
    }
  }

  /** Si e aparece en la lista. */
  def contiene(l: List[Int], e: Int): Boolean = {
    l match {
      case Nil => false
      case x :: xs => x == e || contiene(xs, e)
    }
  }

  /** La lista sin su último elemento. Falla con una lista vacía. */
  def inicio(l: List[Int]): List[Int] = {
    l match {
      case Nil =>
        throw new NoSuchElementException("la lista está vacía")
      case _ :: Nil => Nil
      case x :: xs => x :: inicio(xs)
    }
  }

  /** Si cada elemento es estrictamente menor que el siguiente. */
  def esCreciente(l: List[Int]): Boolean = {
    l match {
      case Nil => true
      case _ :: Nil => true
      case x :: y :: xs => x < y && esCreciente(y :: xs)
    }
  }

  // Punto 3. Ordenamiento por inserción.

  /** Pone x en su lugar dentro de una lista ordenada. */
  def insertar(x: Int, l: List[Int]): List[Int] = {
    l match {
      case Nil => List(x)
      case y :: ys =>
        if (x <= y) x :: l
        else y :: insertar(x, ys)
    }
  }

  /** La lista ordenada de menor a mayor. */
  def ordenar(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case x :: xs => insertar(x, ordenar(xs))
    }
  }

  // Punto 4. Aplanar una lista anidada.

  /** Los números de la lista en una sola lista y en el mismo orden. */
  def aplanar(l: List[Elemento]): List[Int] = {
    l match {
      case Nil => Nil
      case Valor(n) :: xs => n :: aplanar(xs)
      case Sub(elementos) :: xs =>
        aplanar(elementos) ::: aplanar(xs)
    }
  }

  // Punto 5. Codificación por corridas y su inversa.

  /** Las corridas de la lista. */
  def corridas(l: List[Int]): List[Corrida] = {
    l match {
      case Nil => Nil
      case x :: xs =>
        def contar(resto: List[Int], cantidad: Int): (Int, List[Int]) = {
          resto match {
            case y :: ys if y == x =>
              contar(ys, cantidad + 1)
            case _ =>
              (cantidad, resto)
          }
        }

        val (cantidad, resto) = contar(xs, 1)
        Corrida(x, cantidad) :: corridas(resto)
    }
  }

  /** La lista que describen las corridas. */
  def expandir(c: List[Corrida]): List[Int] = {
    def repetir(valor: Int, veces: Int): List[Int] = {
      if (veces <= 0) Nil
      else valor :: repetir(valor, veces - 1)
    }

    c match {
      case Nil => Nil
      case Corrida(valor, veces) :: xs =>
        repetir(valor, veces) ::: expandir(xs)
    }
  }
}