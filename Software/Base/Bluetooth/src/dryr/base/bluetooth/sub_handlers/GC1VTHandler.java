package dryr.base.bluetooth.sub_handlers;

import dryr.base.bluetooth.chained_handler.SubHandler;
import dryr.org.freedesktop.DBus.Properties.PropertiesChanged;

public class GC1VTHandler implements SubHandler<PropertiesChanged> {

	public GC1VTHandler() {
	}

	@Override
	public boolean handle(PropertiesChanged in) {
		return false;
	}

}
