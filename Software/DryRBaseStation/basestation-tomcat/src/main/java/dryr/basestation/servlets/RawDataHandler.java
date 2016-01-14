package dryr.basestation.servlets;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.DataPointDB;
import dryr.basestation.database.DatabaseHelper;
import dryr.basestation.util.ServletUtil;
import dryr.common.json.beans.HumiditySensorDataPoint;

/**
 * Servlet implementation class RawDataHandler
 */
public class RawDataHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public RawDataHandler() {
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		DatabaseHelper helper = new DatabaseHelper();
		if (request.getPathInfo().equals("/single")) {
			// Object item = helper.getData(1).get(0);
			// response.getWriter().append(ServletUtil.jsonize(item));
			response.getWriter().append("Single Data Point");
		} else if (request.getPathInfo().equals("/multiple")) {
			int amount = Integer.getInteger(request.getParameter("amount"), -1);
			List<HumiditySensorDataPoint> sensor = (new DataPointDB()).getData(amount);
			//response.getWriter()
			//		.append(ServletUtil.jsonize(helper.getData(Integer.parseInt(request.getParameter("amount")))));
			response.getWriter().append("Multiple Data Points");
		} else {
			response.getWriter().append("Requested page: data").append(request.getPathInfo()).append(
					". \nAccess /data/single for the most recent data point or /data/multiple for multiple data points.");
		}
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
