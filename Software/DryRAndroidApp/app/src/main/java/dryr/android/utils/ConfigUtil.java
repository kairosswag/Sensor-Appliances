package dryr.android.utils;

import android.content.Context;
import android.util.TypedValue;

import dryr.android.R;

public class ConfigUtil {
    public static final float getDryThreshold(Context context) {
        TypedValue outValue = new TypedValue();
        context.getResources().getValue(R.dimen.sensor_humidity_dry_threshold, outValue, true);
        return outValue.getFloat();
    }

    public static final float getJumpThreshold(Context context) {
        TypedValue outValue = new TypedValue();
        context.getResources().getValue(R.dimen.sensor_humidity_jump_threshold, outValue, true);
        return outValue.getFloat();
    }

    /**
     * Converts the bssi to a number between 0 and 1
     * (bssi range defined in configuration.xml)
     * @return reception value between 0 and 1
     */
    public static double convertRssi(int rssi, Context context) {
        int bssiMin = context.getResources().getInteger(R.integer.sensor_rssi_min);
        int bssiMax = context.getResources().getInteger(R.integer.sensor_rssi_max);

        double result = (double) (rssi - bssiMin) / (bssiMax - bssiMin);

        // prevent unexpected values to affect the ui too much
        if (result < 0) {
            result = 0;
        } else if (result > 1) {
            result = 1;
        }

        return result;
    }
}
