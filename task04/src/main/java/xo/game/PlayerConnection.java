package xo.game;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.ByteBuffer;


class PlayerConnection
{
	public final int id;
	
	private final Socket clientSocket;

	public PlayerConnection(int id, Socket clientSocket)
	{
		this.id = id;
		this.clientSocket = clientSocket;
	}

	public void sendTurn(int id, Cell p)
	{
		sendMessage(id, p.col, p.row);
	}
	
	public void sendNextTurn(int id)
	{
		sendMessage(-1, id);
	}

	public Cell getTurn()
	{
		sendMessage(-1);
		
		try
		{
			int length = 0;
			InputStream is = clientSocket.getInputStream();
			while ((length = is.available()) == 0)
			{
				Thread.sleep(70);
			}

			byte[] message = new byte[length];
			is.read(message, 0, length);

			String text = decodeMessage(message);
			String[] parts = text.split(",");
			int col = Integer.parseInt(parts[0]);
			int row = Integer.parseInt(parts[1]);
			return new Cell(col, row);
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}

	public void sendWinnerId(int id)
	{
		sendMessage(id);
	}
	
	private String decodeMessage(byte[] b)
	{
		int len = b.length;
		int rMaskIndex = 2;

		byte data = b[1];
		byte op = (byte) 127;
		byte rLength = (byte) (data & op);

		if (rLength == (byte) 126)
			rMaskIndex = 4;
		if (rLength == (byte) 127)
			rMaskIndex = 10;

		byte[] masks = new byte[4];

		for (int i = rMaskIndex, j = 0; i < (rMaskIndex + 4); ++i, ++j)
		{
			masks[j] = b[i];
		}

		int rDataStart = rMaskIndex + 4;

		int messLen = len - rDataStart;
		byte[] message = new byte[messLen];

		for (int i = rDataStart, j = 0; i < len; ++i, ++j)
		{
			message[j] = (byte) (b[i] ^ masks[j % 4]);
		}

		return new String(message);
	}
	
	private void sendMessage(int... params)
	{
		ByteBuffer buffer = ByteBuffer.allocate(2 + params.length * 4)
				.put((byte) 0x82) // FIN-bit
				.put((byte) (params.length * 4));
		
		for (int i = 0; i < params.length; ++i)
			buffer.putInt(params[i]);
				
		try
		{
			OutputStream os = clientSocket.getOutputStream();
			os.write(buffer.array());
			os.flush();
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
	
	@Override
	protected void finalize() throws Throwable
	{
		clientSocket.close();
	}
}
