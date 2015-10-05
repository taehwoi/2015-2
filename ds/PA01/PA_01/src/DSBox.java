/**
 * Implement this class using SingleLinkedList
 * @author DMLab
 *
 */
public class DSBox {
	private SingleLinkedList<String> list;
  private final String ERROR = "BOX is empty";
	
	DSBox(){
    list = new SingleLinkedList<>();
	}
	
	/**
	 * 
	 * @param document
	 */
	public void submit(String document){
    list.moveToStart();
    list.insert(document);
	}
	
	/**
	 * 
	 * @return
	 */
	public String get_top(){
    String doc;
    list.moveToStart();
    doc = peek();
    list.remove();
		return doc;
	}
	
	/**
	 * 
	 * @return
	 */
	public String get_bottom(){
    String doc;
    list.moveToEnd();
    doc = peek();
    doc = list.remove();
		return doc;
	}
	
	/**
	 * 
	 * @return
	 */
	public String view_top(){
    list.moveToStart();

		return peek();
	}
	
	/**
	 * 
	 * @return
	 */
	public String view_bottom(){
    list.moveToEnd();
		return peek();
	}
	
	/**
	 * 
	 * @return
	 */
	public int size(){
		return list.length();
	}

  private String peek()
  {
    if(size()>0)
      return list.getValue();
    else
      return ERROR;
  }
}
