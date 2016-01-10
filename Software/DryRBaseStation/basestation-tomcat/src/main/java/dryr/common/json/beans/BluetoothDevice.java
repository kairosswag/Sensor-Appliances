package dryr.common.json.beans;

/**
 * Resembles a bluetooth device.
 * 
 * 
 * This follows the JavaBean conventions.
 * 
 * @author kairo
 *
 */
public class BluetoothDevice {
	private long mac;
	private int status;
	
	public BluetoothDevice() { 
		
	}

	public long getMac() {
		return mac;
	}

	public void setMac(long mac) {
		this.mac = mac;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

}
