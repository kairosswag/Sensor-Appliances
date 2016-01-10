package dryr.basestation.servlets;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import dryr.basestation.database.DatabaseHelper;

/**
 * Servlet implementation class BluetoothUpdateHandler
 */
public class BluetoothUpdateHandler extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public BluetoothUpdateHandler() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		if (request.getPathInfo().equals("/alert")) {
			//push to phone
		} else if (request.getPathInfo().equals("/register")) {
			DatabaseHelper helper = new DatabaseHelper();
			try {
				Long id = Long.parseLong((String) request.getAttribute("id"));
				helper.addPhone(id);
			} catch (NumberFormatException ex) {
			}
		}
		response.getWriter().append("Served at: ").append(request.getContextPath());
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
