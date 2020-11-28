package akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {

  private sealed trait Position

  private case object Left extends Position

  private case object Right extends Position

  def props(elem: Int): Props = Props(new BinaryTreeNode(elem))
}

final class BinaryTreeNode(val elem: Int) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()

  override def receive: Receive = {
    case Insert(requester, id, elem) => doInsert(Insert(requester, id, elem)) // it there a nice way how not to reconstruct that message?
    case Contains(requester, id, elem) => doContains(Contains(requester, id, elem))
    case Remove(requester, id, elem) => doRemove(Remove(requester, id, elem))
    case AppendRight(node) => doAppendRight(node)
    case ReplaceMe(elem, by) => doReplaceMe(elem, by)
  }

  private def doAppendRight(node: ActorRef): Unit = {
    subtrees.get(Right)
      .fold({
        subtrees += (Right -> node)
      })({
        right => right ! AppendRight(node)
      })
  }

  private def doReplaceMe(node: ActorRef, by: Option[ActorRef]): Unit = {
    val pos = subtrees.find(_._2 == node).map(_._1)
    pos.foreach(pos => subtrees = subtrees - pos)
    pos.zip(by).foreach(subtrees += _)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      m.requester ! OperationFinished(m.id)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos).fold({
        val ref = context.actorOf(BinaryTreeNode.props(m.elem))
        subtrees = subtrees + (pos -> ref)
        ref
      })(child => child) ! m
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) {
      m.requester ! ContainsResult(m.id, result = true)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos) match {
        case None => m.requester ! ContainsResult(m.id, result = false)
        case Some(child) => child ! m
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      //always replace self by Left node

      val myReplacement: Option[ActorRef] = (subtrees.get(Left), subtrees.get(Right)) match {
        case (Some(left), Some(right)) => {
          left ! AppendRight(right)
          Some(left)
        }
        case (Some(left), None) => Some(left)
        case (None, Some(right)) => Some(right)
        case _ => None
      }

      context.parent ! ReplaceMe(self, myReplacement)
      m.requester ! OperationFinished(m.id)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos) match {
        case None => m.requester ! OperationFinished(m.id)
        case Some(child) => child ! m
      }
    }
  }
}
