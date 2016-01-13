package dryr.common.json.beans;

/**
 * Resembles a Bluetooth device.
 * 
 * 
 * This follows the JavaBean conventions.
 * 
 * @author kairo
 *
 */
public class BluetoothDevice {
	private String mac;
	private int status;
	
	public BluetoothDevice() { 
		
	}

	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

}
