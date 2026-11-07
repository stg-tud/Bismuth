package text_rdt.fugue

import text_rdt.fugue.avl.AVLTreeNode
import text_rdt.fugue.avl2.{AVL2Tree, AVL2TreeNode}
import text_rdt.{*}

object DescendantCacheHelper {

  def empty[N](node: AVLTreeNode[N]): AVL2TreeNode[AVLTreeNode[N]] = {
    val sidemostDescendantCacheRoot = AVL2Tree[AVLTreeNode[N]](null, node)
    val sidemostDescendantCacheNode =
      AVL2TreeNode(0, sidemostDescendantCacheRoot)
    sidemostDescendantCacheRoot.insert(
      null,
      sidemostDescendantCacheNode,
      Side.Right
    )
    sidemostDescendantCacheNode
  }

  def append[N](
      parentCache: AVL2TreeNode[AVLTreeNode[N]],
      isAtEnd: Boolean,
      node: AVLTreeNode[N],
      add: Int
  ): AVL2TreeNode[AVLTreeNode[N]] = {
    if isAtEnd then {
      if parentCache.isLastNode then {
        val sidemostDescendantCacheNode =
          AVL2TreeNode(
            parentCache.value + add,
            parentCache
          )
        parentCache
          .tree()
          .insert(
            parentCache,
            sidemostDescendantCacheNode,
            Side.Right
          )
        parentCache
          .tree()
          .descendant = node
        sidemostDescendantCacheNode
      } else {
        val treeForSmaller =
          AVL2Tree[AVLTreeNode[N]](null, node)
        val _ = parentCache
          .tree()
          .split(
            parentCache.value,
            treeForSmaller
          )
        val sidemostDescendantCacheNode =
          AVL2TreeNode(
            parentCache.value + add,
            null.asInstanceOf[AVL2TreeNode[AVLTreeNode[N]]]
          )
        parentCache
          .tree()
          .insert(
            parentCache,
            sidemostDescendantCacheNode,
            Side.Right
          )
        sidemostDescendantCacheNode
      }
    } else {
      val sidemostDescendantCacheRoot =
        AVL2Tree[AVLTreeNode[N]](null, node)
      val sidemostDescendantCacheNode =
        AVL2TreeNode(0, sidemostDescendantCacheRoot)
      sidemostDescendantCacheRoot.insert(
        null,
        sidemostDescendantCacheNode,
        Side.Right
      )
      sidemostDescendantCacheNode
    }
  }
}
