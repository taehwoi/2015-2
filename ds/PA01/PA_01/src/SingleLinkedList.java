/** Source code example for "A Practical Introduction to Data
    Structures and Algorithm Analysis, 3rd Edition (Java)" 
    by Clifford A. Shaffer
    Copyright 2008-2011 by Clifford A. Shaffer
 */

/** Linked list implementation */
class SingleLinkedList<E> implements List<E> {
	private Node<E> dummyH;         // Pointer to list dummyH
	private Node<E> dummyTail;         // Pointer to last element
	protected Node<E> curr;       // Access to current element
	int cnt;		      // Size of list
  int currpos; //current position.

	/** Constructors */
	SingleLinkedList(int size) { this(); }   // Constructor -- Ignore size
	SingleLinkedList() {
    this.clear();
	}
	
	@Override
	public void clear() {
    dummyH = new Node<E>();
    cnt = 0;
    curr = dummyH;
    curr.setNext(dummyTail);
	}

	@Override
	public void insert(E item) {
    Node<E> n = new Node<E>(item,curr.getNext());
    curr.setNext(n);
    cnt++;
	}

	@Override
	public void append(E item) {
    Node<E> tmp;
    tmp = curr;
    if (cnt == 0) 
      insert(item);
    else {
      curr = dummyTail;
      curr = getPrev();//move current to end
      insert(item);//insert() at tail
    }

    curr = tmp; //restore curr position
	}
	@Override
	public E remove() {
    E r = getValue();
    if (curr.getNext().getNext() == dummyTail) {
      currpos--;
    }
    curr.setNext(curr.getNext().getNext());
    cnt--;
		return r;
	}
	@Override
	public void moveToStart() {
    curr = dummyH;
    currpos = 0;
	}
	@Override
	public void moveToEnd() {
    moveToPos(cnt-1);
    currpos = cnt;
	}
	@Override
	public void prev() {
    curr = this.getPrev();
	}
	@Override
	public void next() {
    curr = curr.getNext();
    currpos++;
	}
	@Override
	public int length() {
		return cnt;
	}
	@Override
	public int currPos() {
		return currpos;
	}
  @Override
	public void moveToPos(int pos) {
    curr = dummyH;
    if (pos <0 || pos > cnt) {
      pos = 0;
    }
    else 
    {
      for (int i = 0; i < pos; i++) {
        next();
      }
      currpos = pos;
    }

  }
  @Override
  public E getValue() {
    //curr actually points to one prev node before current.
    return curr.getNext().getElement();
  }

  public String toString()
  {
    String str ="";
    Node<E> it = dummyH;

    while (it.getNext()!=null) 
    {
      it = it.getNext();
      str += it.getElement();
      str +=" ";
    }
    return str;
  }

  private Node<E> getPrev()
  {//returns the node previous to current.
    Node<E> it = dummyH;
    if (it != curr) 
      while (it.getNext() != curr)
        it = it.getNext();
    return it;
  }
}
