package ds.test;
import java.io.StringReader;
import java.util.Scanner;

import ds.powergame.PowerGame;

public class Main {
	private static final int MERGE = 0;
	private static final int LOGIN = 1;
	private static final int PRINTLEADER = 2;
	private static final int PRINTPOWER = 3;

	public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		PowerGame Game = new PowerGame();

		while (scanner.hasNext()) {
			String line = scanner.nextLine();
			Scanner i_scanner = new Scanner(new StringReader(line));
			String cmd = i_scanner.next();
			int Player1 = 0;
			int Player2 = 0;
			int Power = 0;

			switch (getCommandNum(cmd)) {
			case MERGE:
				Player1 = i_scanner.nextInt();
				Player2 = i_scanner.nextInt();
				// fill your code

				break;
			case LOGIN:
				Player1 = i_scanner.nextInt();
				Power = i_scanner.nextInt();
				// fill your code

				break;
			case PRINTLEADER:
				Player1 = i_scanner.nextInt();
				// fill your code				
				
				break;
			case PRINTPOWER:
				Player1 = i_scanner.nextInt();
				// fill your code

				break;
			}

			i_scanner.close();
		}

		scanner.close();
	}

	private static int getCommandNum(String cmd) {
		// System.out.println(cmd);
		if (cmd.equals("merge"))
			return MERGE;
		else if (cmd.equals("login"))
			return LOGIN;
		else if (cmd.equals("printleader"))
			return PRINTLEADER;
		else
			return PRINTPOWER;
	}
}
