package dryr.android.dialogs;

import android.content.Context;
import android.preference.DialogPreference;
import android.util.AttributeSet;
import android.view.View;

import dryr.android.R;

/**
 * Dialog to pair a sensor to the BaseStation
 */
public class PairSensorDialogPreference extends DialogPreference {
    public PairSensorDialogPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
        setPersistent(false);
        setDialogLayoutResource(R.layout.dialog_pair_sensor);
    }

    @Override
    protected void onBindDialogView(View view) {
        super.onBindDialogView(view);
        // Update view with preference values
    }

    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {
            // Persist data needed in shared preferences
        }
    }
}
