package akka

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {

  private sealed trait Position

  private case object Left extends Position

  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case Insert(requester, id, elem) => doInsert(Insert(requester, id, elem))
    case Contains(requester, id, elem) => doContains(Contains(requester, id, elem))
    case Remove(requester, id, elem) => doRemove(Remove(requester, id, elem))
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else {
      val pos = if (m.elem < elem) Left else Right
      subtrees.get(pos).fold({
        val ref = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
        subtrees = subtrees + (pos -> ref)
        ref
      })(child => child) ! m
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) {
      m.requester ! ContainsResult(m.id, result = !removed)
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
      removed = true
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
