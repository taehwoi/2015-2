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
    Integer root1 = find(array[a]);
    Integer root2 = find(array[b]);
		// fill your code
		return root1 != root2;
	}

	/** Merge two subtrees */
	public void union(int a, int b) {
    Integer root1 = find(a);
    Integer root2 = find(b);
    if (root1 != root2)
      array[root2]=root1; //FIXME
		// fill your code
	}

	public Integer find(Integer curr) {
    if (array[curr] == null) return curr;
    array[curr] = find(array[curr]);
    return array[curr];
	}
}
