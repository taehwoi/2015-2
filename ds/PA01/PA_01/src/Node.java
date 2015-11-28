/** Source code example for "A Practical Introduction to Data
    Structures and Algorithm Analysis, 3rd Edition (Java)" 
    by Clifford A. Shaffer
    Copyright 2008-2011 by Clifford A. Shaffer
 */

/** Singly linked list node */
class Node<E> {
	private E element;        // Value for this node
	private Node<E> next;     // Pointer to next node in list

	// Constructors
  Node()
  { element = null; next = null;} //empty Node
  Node(E it)
  { element = it; next = null;} //Node with only item
	Node(E it, Node<E> nextval)
	{ element = it;  next = nextval; }
	Node(Node<E> nextval) { next = nextval; }

	Node<E> getNext() { return next; }  // Return next field
	Node<E> setNext(Node<E> nextval) // Set next field
	{ return next = nextval; }     // Return element field
	E getElement() { return element; }  
	E setElement(E it) { return element = it; }// Set element field
}
