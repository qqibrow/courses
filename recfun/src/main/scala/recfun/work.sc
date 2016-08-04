class Rational(x: Int, y: Int) {
  def number = x;
  def denom = y;

  def addRational(other: Rational) =
  new Rational(number * other.denom + other.number * denom,
    denom * other.denom)

  override def toString() = number + "*" + denom
}

val a = new Rational(1, 2)

abstract class TopLevel {
  def method1(x: Int): Int
  def method2(x: Int): Int = {
    5
  }
}

object MyObject extends TopLevel {
  override def method1(x: Int): Int = {
    2
  }
}

val tryTopLevel = new TopLevel() {
  override def method1(x: Int): Int = {
    3
  }
}

MyObject.method1(3)
tryTopLevel.method1(3)

def scanLeft[A](input: Array[A], a0: A, f:(A, A) => A, out: Array[A]): Unit = {
  out(0) = a0;
  var a = a0;
  var i = 0;
  while(i < input.length) {
    a = f(a, input(i))
    i  = i + 1
    out(i) = a
  }
}

val output =  Array(1, 2, 3, 4)
scanLeft(Array(1, 2, 3), 100, (accu:Int, s:Int) => accu + s,output)
output


abstract class Tree[A]
case class Leaf[A](a: A) extends  Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]


abstract class TreeRes[A] {val res: A}
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A], override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

val l = Node(Leaf(1), Leaf(3))
val r = Node(Leaf(8), Leaf(50))
val root = Node(l ,r)

def reduceRes[A](tree: Tree[A], f: (A, A) => A) : TreeRes[A] = tree match {
  case Leaf(a) => LeafRes(a)
  case Node(l, r) => {
    val (resl, resr) = (reduceRes(l, f), reduceRes(r, f))
    NodeRes(resl, f(resl.res, resr.res), resr)
  }
}

reduceRes(root, (s:Int, a:Int) => s + a)



