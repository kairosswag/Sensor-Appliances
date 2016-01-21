package dryr.basestation.connections;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

public class ConnectionListener implements ServletContextListener {

	@Override
	public void contextDestroyed(ServletContextEvent arg0) {
		ConnectionThreadControl.getInstance().stopThread();

	}

	@Override
	public void contextInitialized(ServletContextEvent arg0) {
		ConnectionThreadControl.getInstance().activateThread();
	}

}
