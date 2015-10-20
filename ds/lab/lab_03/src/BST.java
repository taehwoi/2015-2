import java.lang.Comparable;
import java.util.*;

/** Binary Search Tree implementation for Dictionary ADT */
public class BST<Key extends Comparable<? super Key>, E>
  implements Dictionary<Key, E> {
  private BSTNode<Key,E> root; // Root of the BST
  int nodecount;             // Number of nodes in the BST

  /** Constructor */
  public BST() { //init
    root = new BSTNode<>();
    nodecount = 0;
  }

  /** Insert a record into the tree.
    @param k Key value of the record.
    @param e The record to insert. */
  public void insert(Key k, E e) {
    BSTNode<Key,E> tmp;
    BSTNode<Key,E> data = new BSTNode<>(k,e);
    tmp = root;
    if (nodecount==0)
    {
      root.setKey(k);
      root.setElement(e);
      nodecount++;
      return;
    }
    nodecount++;
    tmp = insert_help(k,e,tmp);//find leaf
    if (k.compareTo(tmp.key()) < 0)
    {
      tmp.setLeft(data);
    }
    else if (k.compareTo(tmp.key()) > 0)
    {
      tmp.setRight(data);
    }
  }

  private BSTNode<Key,E> insert_help(Key k, E e, BSTNode<Key,E> tmp)
  {
    if (k.compareTo(tmp.key()) < 0)
    {
      if (tmp.left() == null) 
        return tmp;
      else
        tmp = insert_help(k,e,tmp.left());
    }
    if (k.compareTo(tmp.key()) > 0)
    {
      if (tmp.right() == null) 
        return tmp;
      else
        tmp =insert_help(k,e,tmp.right());
    }
    return tmp;
  }

  /** @return The number of records in the dictionary. */
  public int size() { 
    return nodecount;
  }

  public void preorder() { 
    preorderHelper(root); 
  }

  private void preorderHelper(BSTNode<Key, E> rt){
    if (rt==null) return; 
    System.out.print(rt.element() + " ");
    if (rt.left() != null) 
      preorderHelper(rt.left());
    if (rt.right() != null) 
      preorderHelper(rt.right());
  }

  public void inorder() { 
    inorderHelper(root); 
  }

  private void inorderHelper(BSTNode<Key, E> rt){
    if (rt==null) return;
    if (rt.left() != null)
      inorderHelper(rt.left());
    System.out.print(rt.element() + " ");
    if (rt.right() != null) 
      inorderHelper(rt.right());
  }

  public void postorder() { 
    postorderHelper(root); 
  }

  private void postorderHelper(BSTNode<Key, E> rt){
    if (rt==null) return;
    if (rt.left() != null)
      postorderHelper(rt.left());
    if (rt.right() != null) 
      postorderHelper(rt.right());
    System.out.print(rt.element() + " ");

    // fill your code

  }

}
