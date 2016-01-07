package dryr.base.bluetooth;

import dryr.base.bluetooth.chained_handler.ChainedHandler;

import org.freedesktop.dbus.DBusSigHandler;
import dryr.org.freedesktop.DBus.Properties;
import dryr.base.bluetooth.sub_handlers.*;

public class PropertiesHandler extends ChainedHandler<Properties.PropertiesChanged> implements DBusSigHandler<Properties.PropertiesChanged> {

	public PropertiesHandler() {
		super.subHandlers.add(new Adapter1PropertiesHandler());
		super.subHandlers.add(new Device1PropertiesHandler());
		super.subHandlers.add(new GC1Handler());
	}

	public void handle(Properties.PropertiesChanged propertiesChanged) {
		subHandleFirst(propertiesChanged);
	}
	
}
