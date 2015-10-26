//package ds.bst;

import java.lang.Comparable;
//import ds.list.SingleLinkedList;

/** Binary Search Tree implementation for Dictionary ADT */
public class BST<Key extends Comparable<? super Key>, E>
implements Dictionary<Key, E> {
	private BSTNode<Key,E> root; // Root of the BST
	int nodecount;             // Number of nodes in the BST

	/** Constructor */
	public BST() { 
		
		// fill your code

	}

	/** Reinitialize tree */
	public void clear() { 
		
		// fill your code

	}

	/** Insert a record into the tree.
      @param k Key value of the record.
      @param e The record to insert. */
	public void insert(Key k, E e) {
		
		// fill your code

	}

	/** Remove a record from the tree.
      @param k Key value of record to remove.
      @return The record removed, null if there is none. */
	public E remove(Key k) {
		
		// fill your code

		return null;
	}

	/** Remove and return the root node from the dictionary.
      @return The record removed, null if tree is empty. */
	public E removeAny() {
		
		// fill your code

		return null;
	}

	/** @return Record with key value k, null if none exist.
      @param k The key value to find. */
	public E find(Key k) { 
		// fill your code
		return null;
	}

	/** @return The number of records in the dictionary. */
	public int size() { 

		// fill your code

		return 0; 
	}
	private E findhelp(BSTNode<Key,E> rt, Key k) {

		// fill your code
		
		return null;
	}

	public void inorder(SingleLinkedList<Key> list) { 
		inorderHelper(root, list); 
	}
	
	private void inorderHelper(BSTNode<Key, E> rt, SingleLinkedList<Key> list){
		
		// fill your code
		
	}
	
	public int rangeSearch(Key from, Key to) { 
		return rangeSearchHelper(root, from, to); 
	}
	
	private int rangeSearchHelper(BSTNode<Key, E> rt, Key from, Key to){
		
		// fill your code
		
		return 0;
	}
	
	
	/** @return The current subtree, modified to contain
   the new item */
	private BSTNode<Key,E> inserthelp(BSTNode<Key,E> rt, Key k, E e) {
		
		// fill your code
		
		return null;
	}
	
	/** Remove a node with key value k
    @return The tree with the node removed */
	private BSTNode<Key,E> removehelp(BSTNode<Key,E> rt,Key k) {
		
		// fill your code
		
		return null;
	}
	
	private BSTNode<Key,E> getmin(BSTNode<Key,E> rt) {
		
		// fill your code
		
		return null;
	}
	
	private BSTNode<Key,E> deletemin(BSTNode<Key,E> rt) {
		
		// fill your code
		
		return null;
	}
	
	

}
