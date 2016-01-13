package dryr.basestation.servlets;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.BluetoothDeviceDB;
import dryr.basestation.database.DatabaseHelper;
import dryr.basestation.util.ServletUtil;
import dryr.common.json.beans.BluetoothDevice;

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
		List<BluetoothDevice> resultList = (new BluetoothDeviceDB()).getBluetoothDeviceList();
		// response.setContentType("application/json");
		response.getWriter().append(ServletUtil.jsonize(resultList));
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
