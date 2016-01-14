package dryr.basestation.servlets;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.base.BluetoothDummy;
import dryr.basestation.database.BluetoothDeviceDB;
import dryr.basestation.util.ServletUtil;
import dryr.common.json.beans.*;

/**
 * Servlet implementation class AvailableDevicesHandler
 */
public class DevicesHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public DevicesHandler() {
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		if (request.getPathInfo().equals("/connect")) {
			String deviceMac = request.getParameter("device");
			BluetoothDevice device = (new BluetoothDeviceDB()).getBluetoothDevice(deviceMac);
			if (device.getStatus() == 1) {
				response.getWriter().append("status: " + deviceMac + " connected");
				return;
			}
			(new BluetoothDummy()).ConnectDevice(deviceMac);
			long killtime = System.currentTimeMillis() + 10000;
			while (!checkAndSleep(deviceMac)) {
				if (killtime < System.currentTimeMillis()) {
					response.getWriter().append("status: could not connect device " + deviceMac);
					return;
				}
			}
			response.getWriter().append("status: connected");
			return;
			
		} else if (request.getPathInfo().equals("/disconnect")) {
			String deviceMac = request.getParameter("device");
			(new BluetoothDummy()).DisconnectDevice(deviceMac);
			response.getWriter().append("status: " + deviceMac + " connected");
		} else {
			String res = request.getParameter("status");
			int status = Integer.getInteger(res, -1);
			List<BluetoothDevice> resultList = (new BluetoothDeviceDB()).getBluetoothDeviceList(status);
			// response.setContentType("application/json");
			response.getWriter().append(ServletUtil.jsonize(resultList));
			
		}
	}
	
	private boolean checkAndSleep(String deviceMac) {
		BluetoothDevice device = (new BluetoothDeviceDB()).getBluetoothDevice(deviceMac);
		if (device != null && device.getStatus() != 1) {
			try {
				Thread.sleep(150);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
			}
			return false;
		}
		return true;
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

}
