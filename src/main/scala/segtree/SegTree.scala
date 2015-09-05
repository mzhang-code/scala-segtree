package segtree

/**
 * Created by mzhang on 9/5/15.
 */

/**
 * Abstract Type of Node and Leaf.
 */
abstract class TreeNode[T]

/**
 * Type of internal node.
 *
 * @param l           [l, r], the segment for which the node represents.
 * @param r           ...
 * @param value       aggregated value of this node.
 * @param leftChild   left child node.
 * @param rightChild  right child node.
 */
case class Node[T](l: Int, r: Int, value: T, leftChild: TreeNode[T], rightChild: TreeNode[T]) extends TreeNode[T]

/**
 * Type of leaf node, i.e. a point in the interval.
 *
 * @param idx   the index of the point.
 * @param value the value of the point.
 */
case class Leaf[T](idx: Int, value: T) extends TreeNode[T]

/**
 * Immutable operations on segment tree node.
 */
object SegTree {

  private def mid(l: Int, r: Int): Int = (l + r) / 2

  /**
   * Build a segment tree on interval [l, r].
   *
   * @param l       range of the whole interval, inclusively, [l, r]
   * @param r       ...
   * @param default initial value of all points.
   * @return        root of this empty segment tree.
   */
  def buildTree[T](l: Int, r: Int, default: T): TreeNode[T] = {
    if (l != r)
      Node(l, r, default, buildTree(l, mid(l, r), default), buildTree(mid(l, r) + 1, r, default))
    else
      Leaf(l, default)
  }

  /**
   * Insert new value to a specific point.
   *
   * @param rootNode  root of segment tree.
   * @param idx       point index to be insert.
   * @param value     value to be insert.
   * @param g         aggregation function.
   * @return          tuple of new root and its value.
   */
  def insertPoint[T](rootNode: TreeNode[T], idx: Int, value: T, g:(T, T) => T): (TreeNode[T], T) = rootNode match {
    case Node(l, r, prevValue, leftChild, rightChild) =>
      if (idx <= mid(l, r)) {
        val (updatedLeft, leftValue) = insertPoint(leftChild, idx, value, g)
        val aggrValue = g(prevValue, leftValue)
        (Node(l, r, aggrValue, updatedLeft, rightChild), aggrValue)
      }
      else {
        val (updatedRight, rightValue) = insertPoint(rightChild, idx, value, g)
        val aggrValue = g(prevValue, rightValue)
        (Node(l, r, aggrValue, leftChild, updatedRight), g(prevValue, aggrValue))
      }

    case Leaf(i, prevValue) => (Leaf(i, value), value)
  }

  /**
   * Query aggregated value of a specific interval.
   *
   * @param rootNode  root of segment tree.
   * @param i         query range [i, j], inclusively.
   * @param j         ...
   * @param g         aggregation function.
   * @return          aggregated value of interval [i, j].
   */
  def query[T](rootNode: TreeNode[T], i: Int, j: Int, g:(T, T) => T): T = rootNode match {
    case Node(l, r, value, leftChild, rightChild) =>
      if (l == i & j == r) value
      else if (j <= mid(l, r))
        query(leftChild, i, j, g)
      else if (i > mid(l, r))
        query(rightChild, i, j, g)
      else
        g(query(leftChild, i, mid(l, r), g), query(rightChild, mid(l, r) + 1, j, g))

    case Leaf(idx, value) => value
  }
}
