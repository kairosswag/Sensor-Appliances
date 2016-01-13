package dryr.android.utils;

/**
 * Utility containing methods to format strings
 */
public class FormatUtil {

    public static String longToMacString(long mac) {
        String raw = Long.toHexString(mac);
        String formatted = "";
        for (int i = 0; i < raw.length(); i+= 2) {
            if (i < raw.length()) {
                formatted.concat(raw.substring(i - 2, i));
            } else {
                formatted.concat(raw.substring(i - 2));
            }
            if (i < raw.length()) {
                formatted += ":";
            }
        }

        return formatted;
    }
}
