package dryr.basestation.servlets;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.DataPointDB;
import dryr.basestation.util.ServletUtil;
import dryr.common.json.beans.Dry;
import dryr.common.json.beans.HumidityTreshold;

public class DryHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public DryHandler() {
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		HumidityTreshold treshold = new HumidityTreshold();
		treshold.setUserBias(0);
		
		String pinfo = request.getPathInfo();
		if (pinfo != null && pinfo.equals("/info")) {
			response.getWriter().append(ServletUtil.jsonize(treshold));
		} else {
			response.getWriter().append(ServletUtil.jsonize((new DataPointDB()).getDry(treshold.getTreshold(), 60)));
		}
	}
}
