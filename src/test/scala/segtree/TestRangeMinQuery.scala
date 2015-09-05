package segtree

import org.junit.Test

/**
 * Created by mzhang on 9/5/15.
 */

class TestRangeMinQuery {

  /**
   * @see https://www.hackerrank.com/challenges/range-minimum-query
   */
  @Test
  def testRangeMinQuery(): Unit = {
    val M = 100001
    val points = List(10, 20, 30, 40, 11, 22, 33, 44, 15, 5)
    val rootNode = initTree(SegTree.buildTree[Int](0, points.length-1, M), points, gMin)

    val v0 = SegTree.query(rootNode, 0, 5, gMin)
    val v1 = SegTree.query(rootNode, 1, 2, gMin)
    val v2 = SegTree.query(rootNode, 8, 9, gMin)
    val v3 = SegTree.query(rootNode, 0, 9, gMin)
    val v4 = SegTree.query(rootNode, 4, 6, gMin)

    assert(10 == v0)
    assert(20 == v1)
    assert(5 == v2)
    assert(5 == v3)
    assert(11 == v4)
  }

  /**
   * Set aggregation function as `Math.min`.
   */
  def gMin(a:Int, b:Int): Int = Math.min(a, b)

  def initTree[T](rootNode: TreeNode[T], points: List[T], f:(T, T) => T): TreeNode[T] = {
    var root = rootNode
    for ((p, idx) <- points.zipWithIndex) {
      root = SegTree.insertPoint(root, idx, p, f)._1
    }
    root
  }
}
