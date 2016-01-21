package dryr.basestation.connections;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;

public final class ConnectionThreadControl {
	private volatile boolean stopThread = false;
	private static ConnectionThreadControl instance = null;
	private Thread listenerThread;

	private ConnectionThreadControl() {
		listenerThread = new Thread(new ConnectionListenerBackgroundTask());
	}

	public synchronized static ConnectionThreadControl getInstance() {
		if (instance == null)
			instance = new ConnectionThreadControl();

		return instance;
	}

	public void activateThread() {
		if (stopThread == true) {
			stopThread = false;
		}
		if (!listenerThread.isAlive()) {
			listenerThread.start();
		}
	}

	public void stopThread() {
		if (listenerThread.isAlive()) {
			stopThread = true;
			try {
				listenerThread.join(2000);
			} catch (InterruptedException e) {
				//whatev
			} finally {
				listenerThread.interrupt();
			}
		}
	}

	/**
	 * Checks for broadcasts on port 8888 and returns a package as an answer.
	 * @author kairosswag
	 *
	 */
	private class ConnectionListenerBackgroundTask implements Runnable {
		private final int timeout = 1500;
		private final String RESPONSE = "DISCOVER_DRYR_BASESTATION_RESPONSE";

		@Override
		public void run() {
			while (!stopThread) {

				DatagramSocket socket = null;
				try {
					socket = new DatagramSocket(8888, InetAddress.getByName("0.0.0.0"));
					socket.setBroadcast(true);
					socket.setSoTimeout(timeout);
					System.out.println("[Info] " + this.getClass() + ": Starting Connection Listener Thread.");
					while (!stopThread) {
						byte[] recvBuf = new byte[15000];
						DatagramPacket packet = new DatagramPacket(recvBuf, recvBuf.length);
						try {
							socket.receive(packet);

							byte[] sendData = RESPONSE.getBytes();
							DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,
									packet.getAddress(), packet.getPort());
							socket.send(sendPacket);

						} catch (SocketTimeoutException ex) {
							// only to check the variable again
						}
					}
				} catch (SocketException socketExc) {
					//System.err.println(socketExc);
				} catch (UnknownHostException e) {
					//e.printStackTrace();
				} catch (IOException e) {
					//e.printStackTrace();
				} finally {
					if (socket != null)
						socket.close();
				}
			}
			System.out.println("[Info] " + this.getClass() + ": Connection Listener Thread stopped." + ((stopThread) ? "" : "..retry now"));
		}
	}

}
