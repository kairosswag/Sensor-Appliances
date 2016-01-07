/**
 * 
 */
package dryr.base.bluetooth.sub_handlers;

import dryr.base.bluetooth.chained_handler.ChainedHandler;
import dryr.base.bluetooth.chained_handler.SubHandler;
import dryr.org.freedesktop.DBus.Properties.PropertiesChanged;

/**
 * @author addis
 *
 */
public class GC1Handler extends ChainedHandler<PropertiesChanged>
		implements SubHandler<PropertiesChanged> {

	/**
	 * 
	 */
	public GC1Handler() {
		super.subHandlers.add(new GC1VHandler());
	}

	/* (non-Javadoc)
	 * @see dryr.base.bluetooth.chained_handler.SubHandler#handle(java.lang.Object)
	 */
	@Override
	public boolean handle(PropertiesChanged in) {
		if (!in._interface.equals("org.bluez.GattCharacteristic1"))
			return false;
		
		subHandleAll(in);
		
		return true;
	}

}
