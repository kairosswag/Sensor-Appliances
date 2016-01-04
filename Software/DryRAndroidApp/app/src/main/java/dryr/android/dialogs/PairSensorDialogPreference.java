package dryr.android.dialogs;

import android.content.Context;
import android.preference.DialogPreference;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;

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
    protected View onCreateView(ViewGroup parent) {
        View v = super.onCreateView(parent);
        // TODO: grey out if no base station connected
        return v;
    }

    @Override
    protected void onBindDialogView(View view) {
        super.onBindDialogView(view);
        // Update view with preference values

        // TODO: Implement dialog with list of paired sensors + option to "unpair", lists of available sensors, progressBars etc.
    }

    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {
            // Persist data needed in shared preferences
        }
    }
}
