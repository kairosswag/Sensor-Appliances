package dryr.android.utils;

import android.content.Context;
import android.text.format.DateFormat;
import android.text.format.DateUtils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import dryr.android.R;
import dryr.common.json.beans.HumiditySensorDataPoint;
import dryr.common.json.beans.Prediction;

/**
 * Utility containing methods to format strings
 */
public class FormatUtil {

    public static int DAY_IN_SECONDS = 60 * 60 * 24;

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

    public static String formatPredictionTime(Prediction prediction, Context context) {
        Calendar now = Calendar.getInstance();
        Calendar pred = Calendar.getInstance();
        pred.setTimeInMillis(prediction.getEstimate());

        int flags = DateUtils.FORMAT_SHOW_TIME;

        if (now.get(Calendar.YEAR) != pred.get(Calendar.YEAR) || now.get(Calendar.DAY_OF_YEAR) != pred.get(Calendar.DAY_OF_YEAR)) {
            flags = flags | DateUtils.FORMAT_SHOW_DATE | DateUtils.FORMAT_NUMERIC_DATE;
        }

        // TODO: variance

        return DateUtils.formatDateTime(context, prediction.getEstimate(), flags);
    }
}
