package ds.powergame;

public class PowerGame {
	private ParPtrTree PPT;
	private Team[] TeamList;
	private static final int MAXPLAYER = 100001;
  //one more because player number can be upto 100000
  //when indices start from 0
	
	public PowerGame(){
		PPT = new ParPtrTree(MAXPLAYER);
		TeamList = new Team[MAXPLAYER];
	}
	
	public boolean Merge(int player1, int player2)
  {
    Integer ldr1 = PPT.find(player1);//find leader
    Integer ldr2 = PPT.find(player2);
    Integer team1Power=TeamList[ldr1].GetTeamPower();//find team
    Integer team2Power=TeamList[ldr2].GetTeamPower();

    if (team1Power > team2Power) {
      PPT.union(ldr1,ldr2); //merge team
      TeamList[ldr1].setTeamPower(team1Power+team2Power);//add power
    }
    else if(team1Power < team2Power) {
      PPT.union(ldr2,ldr1);
      TeamList[ldr2].setTeamPower(team1Power+team2Power);//add power
    }
    else //does this handle cases for same team?
      return false;

		return true;
	}
	
	public void Login(int player, int power)
  {//generate team
    Team t = new Team(player,power);
    TeamList[t.GetLeader()] = t;
	}
	
	public Integer PrintLeader(int player){
    Integer leader = PPT.find(player);//find leader
    return leader;
	}
	
	public Integer PrintPower(int player){
    Integer leader = PPT.find(player);//find leader
    return TeamList[leader].GetTeamPower();
	}
}
