package dryr.basestation.util;

public class OtherUtil {
	private OtherUtil() {
	}

	public static Integer parseInt(String string, Integer default_value) {
		try {
			return Integer.parseInt(string);
		} catch(NumberFormatException nfe) {
			return default_value;
		}
	}
}
