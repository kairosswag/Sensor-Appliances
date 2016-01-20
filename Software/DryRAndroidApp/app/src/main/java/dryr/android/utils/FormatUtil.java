package dryr.android.utils;

import android.content.Context;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import dryr.android.R;
import dryr.common.json.beans.HumiditySensorDataPoint;

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

    public static Date getDateFromHDT(HumiditySensorDataPoint dataPoint, Context context) {
        SimpleDateFormat sdf = new SimpleDateFormat(context.getString(R.string.date_time_format_server));
        Date date = null;
        try {
            date = sdf.parse(dataPoint.getDate());
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return date;
    }
}
