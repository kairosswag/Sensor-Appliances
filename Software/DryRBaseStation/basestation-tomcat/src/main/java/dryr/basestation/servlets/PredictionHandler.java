package dryr.basestation.servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.util.ServletUtil;
import dryr.forecast.Forecast;

/**
 * Servlet implementation class PredictionHandler
 */
public class PredictionHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#HttpServlet()
	 */
	public PredictionHandler() {
		super();
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String deviceMac = request.getParameter("device");
		if (deviceMac != null) {
			try {
				Forecast forecast = new Forecast(deviceMac);
				response.getWriter().append(ServletUtil.jsonize(forecast));
			} catch (Exception e) {
				response.getWriter().append(ServletUtil.jsonize("Could not generate forecast."));
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			response.getWriter().append("Could not find required Parameter: device");
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
