package xo.game.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Base64;

import xo.game.GameManager;

class GameServer
{
	private static final int PORT = 2016;
	private static final String SEC_WS_ASSEPT_KEY = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
	private static final String WELCOME_HEADER_PART =
			"HTTP/1.1 101 Switching Protocols\nUpgrade: websocket\nConnection: Upgrade\nSec-WebSocket-Accept: ";
	
	
	public static void main(String args[]) throws IOException
	{
		new StaticServer().start();
		GameManager game = new GameManager();
		game.start();
		
		try (ServerSocket serverSocket = new ServerSocket(PORT))
		{
			while (true)
			{
				try
				{
					Socket clientSocket = serverSocket.accept();
					welcomePlayer(clientSocket);
					game.createNewPlayer(clientSocket);
				}
				catch (IOException e)
				{
					throw new RuntimeException(e);
				}
			}
		}
	}
	
	private static void welcomePlayer(Socket clientSocket) throws IOException
	{
		BufferedReader reader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

		String key = "";
		while (true)
		{
			String line = reader.readLine();
			String[] keyValue = line.split(":");
			if (keyValue[0].equals("Sec-WebSocket-Key"))
			{
				key = keyValue[1].substring(1);
				break;
			}
		}
		
		try
		{
			String acceptKey = key + SEC_WS_ASSEPT_KEY;
			byte[] hash = MessageDigest.getInstance("SHA1").digest(acceptKey.getBytes(StandardCharsets.UTF_8));
			String accept = Base64.getEncoder().encodeToString(hash);
			String response = WELCOME_HEADER_PART + accept + "\n\n";
			
			OutputStream os = clientSocket.getOutputStream();
			os.write(response.getBytes(StandardCharsets.UTF_8));
			os.flush();
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}
	
}
