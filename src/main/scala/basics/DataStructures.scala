package main.scala.basics

object DataStructures {
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = map
    .groupBy(_._2)
    .map(el => (el._1, el._2.keys))
    .foldLeft(Map[Set[T], Int]())((acc, el) => acc + (el._2.toSet -> el._1))
    .toList
    .sortBy(_._2)
}
