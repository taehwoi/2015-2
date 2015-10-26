package ds.bst;

public class BookSearch {
	private BST<String, String> bst;
  private static final String ERRORMSG_find
    = "BookSearch can not find the book.";
  private static final String ERRORMSG_order
    = "BookSearch does not have any book.";
	
	public BookSearch(){
    bst = new BST<>();
	}
	
	public void add(String name, String location){
    bst.insert(name,location);
	}
	
	public String remove(String name){
		return bst.remove(name);
	}
	
	public String get(String name){
    return bst.find(name);
	}
	
	public int size(){
		return bst.size();
	}
	
	public void order(){
    bst.inorder();
    return;
	}
	
	public String first(){
    if (bst.min()==null) 
      return null;
    else
      return bst.min().key();
	}
	
	public String last(){
    if (bst.max()==null) 
      return null;
    else
      return bst.max().key();
	}
	
	public int range(String from, String to){
    return bst.rangeSearch(from,to);
	}
	
}
