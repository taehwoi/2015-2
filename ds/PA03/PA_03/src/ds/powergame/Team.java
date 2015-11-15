package ds.powergame;

public class Team {
	private Integer Leader;
	private Integer TeamPower;
	
	Team(){
		Leader = 0;
		TeamPower = 0;
	}
	
	Team(Integer l, Integer tp){
		Leader = l;
		TeamPower = tp;
	}
	
	public void setLeader(Integer l){
		Leader = l;
	}
	
	public void setTeamPower(Integer tp){
		TeamPower = tp;
	}
	
	public void addTeamPower(Integer tp){
		TeamPower += tp;
	}
	
	public Integer GetLeader(){
		return Leader;
	}
	
	public Integer GetTeamPower(){
		return TeamPower;
	}
}
