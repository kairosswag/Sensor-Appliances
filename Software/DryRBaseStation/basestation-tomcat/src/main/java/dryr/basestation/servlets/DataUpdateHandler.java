package dryr.basestation.servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.DataPointDB;
import dryr.basestation.type.MinMax;

//Performs cleanup tasks on the database
public class DataUpdateHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;

    /**
     * @see HttpServlet#HttpServlet()
     */
	public DataUpdateHandler() {
		super();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		DataPointDB dataPointDB = new DataPointDB();
		if (request.getParameterMap().containsKey("device")) {
			String mac = request.getParameter("device");
			MinMax minMax = dataPointDB.getMinMax(mac, 60);
			if (minMax != null && minMax.getMax() - minMax.getMin() > 10.0) {
				dataPointDB.deleteData(mac);
			}
		}
		dataPointDB.deleteOldData(5);
	}
}
