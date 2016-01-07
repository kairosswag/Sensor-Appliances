package dryr.base.bluetooth.sub_handlers;

import dryr.base.bluetooth.chained_handler.SubHandler;
import dryr.org.freedesktop.DBus.Properties.PropertiesChanged;

public class GC1VHHandler implements SubHandler<PropertiesChanged> {

	public GC1VHHandler() {
	}

	@Override
	public boolean handle(PropertiesChanged in) {
		return false;
	}

}
