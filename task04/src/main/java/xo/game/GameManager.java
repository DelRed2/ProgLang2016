package xo.game;

import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GameManager extends Thread
{
	private static final int GAME_FIELD_SIZE = 20;

	private final int[][] gameField = new int[GAME_FIELD_SIZE][GAME_FIELD_SIZE];
	
	private volatile List<PlayerConnection> players = new ArrayList<>();

	public GameManager()
	{
		// Empty
	}
	
	@Override
	public void run()
	{
		waitFor2Players();
		startNewGame();
		
		PlayerConnection curPlayer = players.get(0);
		while (true)
		{
			doTurn(curPlayer);
			if (checkWin())
			{
				startNewGame();
				curPlayer = players.get(0);
			}
			else
			{
				synchronized (players)
				{
					if (curPlayer.id == players.size() - 1)
						curPlayer = players.get(0);
					else
						curPlayer = players.get(curPlayer.id + 1);
				}
			}
		}
	}

	public void createNewPlayer(Socket clientSocket)
	{
		PlayerConnection newPlayer = null;
		synchronized (players)
		{
			newPlayer = new PlayerConnection(players.size(), clientSocket);
			players.add(newPlayer);
		}
		
		if (newPlayer.id > 1)
		{
			for (int col = 0; col < GAME_FIELD_SIZE; ++col)
				for (int row = 0; row < GAME_FIELD_SIZE; ++row)
					if (gameField[col][row] != -1)
						newPlayer.sendTurn(gameField[col][row], new Cell(col, row));
		}
	}

	private void startNewGame()
	{
		for (int i = 0; i < GAME_FIELD_SIZE; i++)
		{
			Arrays.fill(gameField[i], -1);
		}
	}
	
	private int getWinPlayerId()
	{
		int win = -1;
		if ((win = checkWinCase(0, 1)) != -1)
			return win;

		if ((win = checkWinCase(1, 0)) != -1)
			return win;
		
		if ((win = checkWinCase(-1, 1)) != -1)
			return win;
		
		return checkWinCase(1, 1);
	}
	
	private int checkWinCase(int colDelta, int rowDelta)
	{
		int colMargin = colDelta != 0 ? 4 : 0;
		int rowMargin = rowDelta != 0 ? 4 : 0;
		
		int colStart = colDelta >= 0 ? 0 : 4;
		int rowStart = rowDelta >= 0 ? 0 : 4;
		
		for (int col = colStart; col < GAME_FIELD_SIZE - colMargin; ++col)
		{
			for (int row = rowStart; row < GAME_FIELD_SIZE - rowMargin; ++row)
			{
				int s1 = gameField[col][row];
				int s2 = gameField[col + colDelta][row + rowDelta];
				int s3 = gameField[col + colDelta * 2][row + rowDelta * 2];
				int s4 = gameField[col + colDelta * 3][row + rowDelta * 3];
				int s5 = gameField[col + colDelta * 4][row + rowDelta * 4];
				
				if (s1 != -1 && s1 == s2 && s2 == s3 && s3== s4 && s4 == s5)
					return s1;
			}
		}
		
		return -1;
	}
	
	private boolean checkWin()
	{
		int win = getWinPlayerId();
		if (win != -1)
		{
			synchronized (players)
			{
				for (PlayerConnection player : players)
				{
					player.sendWinnerId(win);
				}
			}
			return true;
		}
		return false;
	}

	private void doTurn(PlayerConnection curPlayer)
	{
		synchronized (players)
		{
			for (PlayerConnection player : players)
				if (player != curPlayer)
					player.sendNextTurn(curPlayer.id);
		}
		
		Cell p = curPlayer.getTurn();
		gameField[p.col][p.row] = curPlayer.id;
		
		synchronized (players)
		{
			for (PlayerConnection player : players)
				player.sendTurn(curPlayer.id, p);
		}
	}
	
	private void waitFor2Players()
	{
		while (true)
		{
			try
			{
				Thread.sleep(500);
				synchronized (players)
				{
					if (players.size() >= 2)
						break;
				}
			}
			catch (Exception e)
			{
				throw new RuntimeException(e);
			}
		}
	}
}
