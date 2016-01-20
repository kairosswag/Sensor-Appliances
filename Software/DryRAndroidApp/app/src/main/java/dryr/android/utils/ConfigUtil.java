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
}
