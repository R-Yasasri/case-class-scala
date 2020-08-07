package caseclassassignment

case class Point(a: Int, b: Int) {
  def x: Int = a;

  def y: Int = b;

  def +(p: Point): Point = {
    Point(this.x + p.x, this.y + p.y);
  }

  def move(dx: Int, dy: Int): Point = {
    var newX: Int = this.x + dx;
    var newY: Int = this.y + dy;

    Point(newX, newY);
  }

  def distance(p: Point): Point = {
    var newX: Int = 0;
    var newY: Int = 0;
    this.x > p.x match {
      case true => newX = this.x - p.x;
      case false => newX = p.x - this.x;
    }

    this.y > p.y match {
      case true => newY = this.y - p.y;
      case false => newY = p.y - this.y;
    }

    Point(newX, newY);
  }

  def invert(): Point = {
    Point(this.y, this.x);
  }
}

object m extends App {

  val p = Point(4, 5);
  val p2 = Point(2, 2);
  val p3 = Point(3, 3);

  val p4 = p + p2 + p3;

  println(p4.toString);

  val p5=p4.move(1,2);

  println(p5.toString);

  val p6=p5.distance(p);

  println(p6.toString);

  val p7=p6.invert();

  println(p7.toString);
}
