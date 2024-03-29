package dryr.basestation.servlets;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.DataPointDB;
import dryr.basestation.database.DatabaseHelper;
import dryr.basestation.util.OtherUtil;
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
		String pinfo = request.getPathInfo();
		if (pinfo != null && pinfo.equals("/single")) {
			// Object item = helper.getData(1).get(0);
			// response.getWriter().append(ServletUtil.jsonize(item));
			response.getWriter().append("Single Data Point");
		} else if (pinfo != null && pinfo.equals("/multiple")) {
			if (request.getParameterMap().containsKey("device")) {
				String mac = request.getParameter("device");
				List<HumiditySensorDataPoint> res;
				if (request.getParameterMap().containsKey("minDate")) {
					String minDate = request.getParameter("minDate");
					res = (new DataPointDB()).getDataNewerThan(mac, minDate);
				} else {
					res = (new DataPointDB()).getData(mac);
				}
				if (res != null) {
					response.getWriter().append(ServletUtil.jsonize(res));
					return;
				}
				response.getWriter().append("Multiple Data Points");
			} else {
				response.setStatus(response.SC_NOT_ACCEPTABLE);
			}
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
