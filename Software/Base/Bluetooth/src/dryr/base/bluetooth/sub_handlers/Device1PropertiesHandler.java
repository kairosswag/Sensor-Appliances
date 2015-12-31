package dryr.base.bluetooth.sub_handlers;

import dryr.base.bluetooth.chained_handler.SubHandler;
import dryr.org.freedesktop.DBus.Properties.PropertiesChanged;

public class Device1PropertiesHandler implements SubHandler<PropertiesChanged> {

	public Device1PropertiesHandler() {
	}

	@Override
	public boolean handle(PropertiesChanged in) {
		if (!in._interface.equals("org.bluez.Device1"))
			return false;
		
		System.out.println("Device1");
		
		return true;
	}

}
