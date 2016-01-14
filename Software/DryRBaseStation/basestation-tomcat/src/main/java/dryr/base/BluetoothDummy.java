package dryr.base;

public class BluetoothDummy implements bluetooth {

	@Override
	public void ConnectDevice(String a) {
		System.out.println("BluetoothDummy class has been called. If this appears in the log, something is on test mode.");
	}

	@Override
	public void DisconnectDevice(String a) {
		System.out.println("BluetoothDummy class has been called. If this appears in the log, something is on test mode.");

	}

}
