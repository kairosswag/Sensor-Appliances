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
public class Adapter1PropertiesHandler extends ChainedHandler<PropertiesChanged>
		implements SubHandler<PropertiesChanged> {

	/**
	 * 
	 */
	public Adapter1PropertiesHandler() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see dryr.base.bluetooth.chained_handler.SubHandler#handle(java.lang.Object)
	 */
	@Override
	public boolean handle(PropertiesChanged in) {
		if (!in._interface.equals("org.bluez.Adapter1"))
			return false;
		
		System.out.println("Adapter1");
		
		return true;
	}

}
