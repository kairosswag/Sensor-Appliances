package dryr.broadcast.test;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;

public class BroadcastTest {

	private static final String RESPONSE = "DISCOVER_DRYR_BASESTATION_RESPONSE";
	private static final String REQUEST = "DISCOVER_DRYR_BASESTATION_REQUEST";

	public static void main(String[] args) throws SocketException {
		DatagramSocket c = new DatagramSocket();
		c.setBroadcast(true);
		byte[] sendData = REQUEST.getBytes();
		try {
			DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,
					InetAddress.getByName("255.255.255.255"), 8888);
			c.send(sendPacket);

			byte[] recvBuffer = new byte[15000];
			DatagramPacket recvPacket = new DatagramPacket(recvBuffer, recvBuffer.length);

			// You can set
			c.setSoTimeout(0);
			// with a value > 0 to cause a timeout interruption if it takes too
			// long since receive will block.
			c.receive(recvPacket);

			System.out.println("The IP-Address to connect to the tomcat server: \n>>>>>>>>>" + recvPacket.getAddress());
			System.out.println("This should match to RESPONSE: " + new String(recvPacket.getData()));
			
			//This code is rubbish
			if ((new String(recvPacket.getData())).contains(RESPONSE)
					|| RESPONSE.contains(new String(recvPacket.getData()))) {
				System.out.println("It matches");
			} else {
				System.out.println("It does not match");
			} // - til here

		} catch (Exception e) {
			System.out.println(e);
		} finally {
			if (c != null)
				c.close();
		}

	}

}
