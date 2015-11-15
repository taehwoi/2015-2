package ds.powergame;

public class PowerGame {
	private ParPtrTree PPT;
	private Team[] TeamList;
	private static final int MAXPLAYER = 100000;
	
	public PowerGame(){
		PPT = new ParPtrTree(MAXPLAYER);
		TeamList = new Team[MAXPLAYER];
	}
	
	public boolean Merge(int player1, int player2){
		// fill your code
		return true;
	}
	
	public void Login(int player, int power){
		// fill your code
	}
	
	public void PrintLeader(int player){
		// fill your code
	}
	
	public void PrintPower(int player){
		// fill your code
	}
}
