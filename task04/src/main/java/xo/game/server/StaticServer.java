package xo.game.server;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

class StaticServer extends Thread
{
	private static final int PORT = 80;
	private static final byte[] GET_GAME_FIELD_RESPONSE;
	
	static 
	{
		String path = StaticServer.class.getResource("gameField.html").getFile();
		
		String gameFieldHtml = null;
		try (Scanner scaner = new Scanner(new File(path)))
		{
			gameFieldHtml = scaner.useDelimiter("\\A").next();
		}
		catch (FileNotFoundException e)
		{
			throw new RuntimeException(e);
		}
		
		String header = "HTTP/1.1 200 OK\nContent-Type: text/html\nContent-Length: "
				+ gameFieldHtml.length() + "\nConnection: close\n\n";
		
		String response = header + gameFieldHtml;
		
		GET_GAME_FIELD_RESPONSE = response.getBytes(StandardCharsets.UTF_8);
	}

	@Override
	public  void run()
	{
		try (ServerSocket serverSocket = new ServerSocket(PORT))
		{
			while (true)
			{
				try (Socket clientSocket = serverSocket.accept())
				{
					returnGameFieldHtml(clientSocket);
				}
			}
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	private static void returnGameFieldHtml(Socket clientSocket) throws IOException
	{
		try (OutputStream os = clientSocket.getOutputStream())
		{
			os.write(GET_GAME_FIELD_RESPONSE);
			os.flush();
		}
	}

}