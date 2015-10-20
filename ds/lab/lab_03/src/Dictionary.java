


/** The Dictionary abstract class. */
public interface Dictionary<Key, E> {

	/** Insert a record
      @param k The key for the record being inserted.
      @param e The record being inserted. */
	public void insert(Key k, E e);

	/** @return The number of records in the dictionary. */
	public int size();
};