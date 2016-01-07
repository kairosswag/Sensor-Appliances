package dryr.base.bluetooth.sub_handlers;

import dryr.base.bluetooth.chained_handler.ChainedHandler;
import dryr.base.bluetooth.chained_handler.SubHandler;
import dryr.org.freedesktop.DBus.Properties.PropertiesChanged;

public class GC1VHandler extends ChainedHandler<PropertiesChanged>
		implements SubHandler<PropertiesChanged> {

	public GC1VHandler() {
		super.subHandlers.add(new GC1VHHandler());
		super.subHandlers.add(new GC1VTHandler());
	}

	@Override
	public boolean handle(PropertiesChanged in) {
		if (!in.changed_properties.containsKey("Value"))
			return false;
		
		subHandleAll(in);
		
		return true;
	}

}
