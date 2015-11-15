package ds.powergame;

public class ParPtrTree {
	private Integer[] array; // Node array

	public ParPtrTree(int size) {
		array = new Integer[size]; // Create node array
		for (int i = 0; i < size; i++)
			array[i] = null;
	}

	/** Determine if nodes are in different trees */
	public boolean differ(int a, int b) {
		// fill your code
		return true;
	}

	/** Merge two subtrees */
	public void UNION(int a, int b) {
		// fill your code
	}

	public Integer FIND(Integer curr) {
		// fill your code
		// must use path compression
		return 0;
	}
}
