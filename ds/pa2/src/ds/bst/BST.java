package ds.bst;

import java.lang.Comparable;
import ds.list.SingleLinkedList;

/** Binary Search Tree implementation for Dictionary ADT */
public class BST<Key extends Comparable<? super Key>, E>
implements Dictionary<Key, E> {
	private BSTNode<Key,E> root; // Root of the BST
	int nodecount;             // Number of nodes in the BST

	/** Constructor */
	public BST() { 
    this.clear();
	}

	/** Reinitialize tree */
	public void clear() { 
    root = null;
    nodecount = 0;
	}

	/** Insert a record into the tree.
      @param k Key value of the record.
      @param e The record to insert. */
	public void insert(Key k, E e) {
    root = inserthelp(root,k,e);
    nodecount++;
	}

	/** Remove a record from the tree.
      @param k Key value of record to remove.
      @return The record removed, null if there is none. */
	public E remove(Key k) {
    E temp = findhelp(root,k);
    if (temp != null) {
      root = removehelp(root,k);
      nodecount--;
    } 
    return temp;
	}

	/** Remove and return the root node from the dictionary.
      @return The record removed, null if tree is empty. */
	public E removeAny() {
    //do nothing.
		return null;
	}

	/** @return Record with key value k, null if none exist.
      @param k The key value to find. */
	public E find(Key k) { 
    return findhelp(root,k);
	}

	/** @return The number of records in the dictionary. */
	public int size() { 
		return nodecount; 
	}

  public BSTNode<Key,E> min()
  {
    return getmin(root);
  }

  public BSTNode<Key,E> max()
  {
    return getmax(root);
  }
	public void inorder() { 
		inorderHelper(root); 
	}
	public int rangeSearch(Key from, Key to) { 
		return rangeSearchHelper(root, from, to, 0); 
	}

	private E findhelp(BSTNode<Key,E> rt, Key k) {
    if (rt==null) {
      return null;
    }
    if (rt.key().compareTo(k)==0)
      return rt.element();
    else if (rt.key().compareTo(k) > 0)
      return findhelp(rt.left(),k);
    else
      return findhelp(rt.right(),k);
	}
	private BSTNode<Key,E> finder(BSTNode<Key,E> rt, Key k) {
    if (rt==null) {
      return null;
    }
    if (rt.key()==k)
      return rt;
    else if (rt.key().compareTo(k) > 0)
      return finder(rt.left(),k);
    else
      return finder(rt.right(),k);
	}

	
	private void inorderHelper(BSTNode<Key, E> rt){
    if (rt == null)
      return;
    inorderHelper(rt.left());
    System.out.println("ORDER: " + rt.key()); //TODO: to linkedlist
    inorderHelper(rt.right());
	}
	
	
	private int rangeSearchHelper(BSTNode<Key, E> rt, Key from, Key to, int cnt){
    //hack: do a inorder search, and count
    if (rt == null)
      return cnt;
    cnt = rangeSearchHelper(rt.left(),from,to,cnt);
    if (rt.key().compareTo(from) >= 0 && rt.key().compareTo(to) <= 0) 
      cnt++;
    cnt = rangeSearchHelper(rt.right(),from,to,cnt);
		return cnt;
	}
	
	
	/** @return The current subtree, modified to contain
   the new item */
	private BSTNode<Key,E> inserthelp(BSTNode<Key,E> rt, Key k, E e) {
    if (rt==null)
      return new BSTNode<>(k,e);
    else if (rt.key().compareTo(k) > 0)
      rt.setLeft(inserthelp(rt.left(), k, e));
    else
      rt.setRight(inserthelp(rt.right(), k, e));
    return rt;
	}
	
	/** Remove a node with key value k
    @return The tree with the node removed */
	private BSTNode<Key,E> removehelp(BSTNode<Key,E> rt,Key k) {
    if (rt == null)
      return null;
    if (rt.key().compareTo(k) > 0) 
      rt.setLeft(removehelp(rt.left(), k));
    else if (rt.key().compareTo(k) < 0) 
      rt.setRight(removehelp(rt.right(), k));
    else  {
      if (rt.left() == null)
        return rt.right();
      else if (rt.right() == null)
        return rt.left();
      else {
        BSTNode<Key,E> tmp = getmin(rt.right());
        rt.setElement(tmp.element());
        rt.setKey(tmp.key());
        rt.setRight(deletemin(rt.right()));
      }
    }
    return rt;
  }

  private BSTNode<Key,E> getmin(BSTNode<Key,E> rt) {
    if (rt==null)
      return null;
    if (rt.left() == null)
      return rt;
    else
      return getmin(rt.left());
  }

  private BSTNode<Key,E> getmax(BSTNode<Key,E> rt) {
    if (rt==null) 
      return null;
    if (rt.right() == null)
      return rt;
    else
      return getmax(rt.right());
  }

  private BSTNode<Key,E> deletemin(BSTNode<Key,E> rt) {
    if (rt.left()==null)
      return rt.right();
    else {
      rt.setLeft(deletemin(rt.left()));
      return rt;
    }
  }
}
