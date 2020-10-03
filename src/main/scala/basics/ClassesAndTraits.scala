package main.scala.basics

object ClassesAndTraits {

  sealed trait Shape[A] extends Bounded with Located with Movable[A] with WithSurface

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable[A] {
    def move(x: Double, y: Double): A
  }

  // Add method `area` to 2D shapes.
  sealed trait WithSurface {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def area: Double = 0
  }


  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape[Circle] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def move(x: Double, y: Double): Circle = Circle(centerX + x, centerY + y, radius)
    override def area: Double = math.Pi * math.pow(radius, 2)
  }


  def describe[A](x: Shape[A]): String = x match {
    case Point(x, y) => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
    case Rectangle(x1, x2, y1, y2) => s"Rectangle at (x1 = $x1, x2 = $x2, y1 = $y1, y2 = $y2)"
    case Triangle(vertices) => s"Triangle at ${vertices.toList.map(describe(_)).mkString(" ")}"
    case Square(centerX, centerY, w) => s"$w wide square in $centerX,$centerY"
  }

  def minimumBoundingRectangle[T <: Bounded](objects: Set[T]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

      // if needed, fix the code to be correct
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  object Bounded {
    def minimumBoundingRectangle[T <: Bounded](objects: Set[T]): Bounded = ClassesAndTraits.minimumBoundingRectangle(objects)
  }


  // Homework
  //
  // Add additional 2D shapes such as triangle and square.

  final case class Triangle(vertices: Set[Point]) extends Shape[Triangle] {
    override def move(x: Double, y: Double): Triangle = Triangle(vertices.map(_.move(x, y)))
    override def x: Double = vertices.head.x
    override def y: Double = vertices.head.y

    override def minX: Double = vertices.map(_.minX).min
    override def maxX: Double = vertices.map(_.maxX).max
    override def minY: Double = vertices.map(_.minY).min
    override def maxY: Double = vertices.map(_.minY).max
    override def area: Double = {
      def l(a: Point, b: Point): Double = math.sqrt(math.pow(a.x - b.x, 2) + math.pow(a.y - b.y, 2))

      val edges = vertices.subsets(2).map(_.toList).map({ case List(a, b) => l(a, b) }).toList
      val s = edges.sum / 2
      math.sqrt(s * (s - edges(0)) * (s - edges(1)) * (s - edges(2)))
    }
  }


  case class Rectangle(x1: Double, x2: Double, y1: Double, y2: Double) extends Shape[Rectangle] {
    override def x: Double = x1
    override def y: Double = y1

    override def minX: Double = math.min(x1, x2)
    override def maxX: Double = math.max(x1, x2)
    override def minY: Double = math.min(y1, y2)
    override def maxY: Double = math.max(y1, y2)
    override def move(x: Double, y: Double): Rectangle = Rectangle(x1 + x, x2 + x, y1 + y, y2 + y)
    override def area: Double = (maxX - minX) * (maxY - minY)
  }


  final case class Square(centerX: Double, centerY: Double, w: Double) extends Shape[Square] {
    private val rect = Rectangle(centerX - w / 2, centerX + w / 2, centerY - w / 2, centerY + w / 2)

    override def x: Double = rect.x
    override def y: Double = rect.y
    override def minX: Double = rect.minX
    override def maxX: Double = rect.maxX
    override def minY: Double = rect.minY
    override def maxY: Double = rect.maxY
    override def move(x: Double, y: Double): Square = Square(centerX + x, centerY + y, w)
    override def area: Double = rect.area
  }



  sealed case class Triplet(x: Double, y: Double, z: Double) extends Movable3D[Triplet] {
    override def move(vector: Triplet): Triplet = Triplet(x + vector.x, y + vector.y, z + vector.z)
  }

  sealed trait Origin {
    def origin: Triplet
  }

  sealed trait Volumed3D {
    def volume: Double
  }

  sealed trait Surfaced3D {
    def surface: Double
  }

  sealed trait Movable3D[A] {
    def move(vector: Triplet): A
  }

  sealed trait Shape3D[A] extends Origin with Volumed3D with Surfaced3D with Movable3D[A]

  sealed case class Point3D(origin: Triplet) extends Shape3D[Point3D] {
    override def surface: Double = 0
    override def volume: Double = 0
    override def move(vector: Triplet): Point3D = Point3D(origin.move(vector))
  }

  sealed case class Sphere3D(origin: Triplet, radius: Double) extends Shape3D[Sphere3D] {
    override def surface: Double = ???
    override def volume: Double = ???
    override def move(vector: Triplet): Sphere3D = Sphere3D(origin.move(vector), radius)
  }


  sealed case class Cuboid(origin: Triplet, sizeX: Double, sizeY: Double, sizeZ: Double) extends Shape3D[Cuboid] {
    override def surface: Double = (sizeX * sizeY * 2) * (sizeX * sizeZ * 2) * (sizeY * sizeZ * 2)
    override def volume: Double = sizeX * sizeY * sizeZ

    // failed to write proper polymorph for move to fit Cube as well
    override def move(vector: Triplet): Cuboid = Cuboid(origin.move(vector), sizeX, sizeY, sizeZ)
  }

  sealed case class Cube(origin: Triplet, side: Double) extends Shape3D[Cube] {
    private val cuboid = Cuboid(origin, side, side, side)
    override def surface: Double = cuboid.surface
    override def volume: Double = cuboid.volume
    override def move(vector: Triplet): Cube = Cube(origin.move(vector), side)
  }

  sealed case class Tethraeder(verticies: Set[Triplet]) extends Shape3D[Tethraeder] {
    override def origin: Triplet = verticies.head
    override def surface: Double = ???
    override def volume: Double = ???
    override def move(vector: Triplet): Tethraeder = Tethraeder(verticies.map(_.move(vector)))
  }


  def main(args: Array[String]): Unit = {
    describe(Point(10, 20))

    val t = Tethraeder(Set(
      Triplet(1, 2, 3),
      Triplet(2, 3, 4),
      Triplet(3, 4, 5),
    ))
  }
}
