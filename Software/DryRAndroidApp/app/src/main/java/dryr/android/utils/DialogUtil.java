package dryr.android.utils;

import android.content.Context;
import android.content.DialogInterface;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;
import android.support.v7.app.AlertDialog;

import dryr.android.R;

/**
 * Utils that work with Android dialogs
 */
public class DialogUtil {

    /**
     * Method to show an error dialog
     * @param context the application context
     * @param messageId the resource id for the error message
     * @param positiveListener a listener to be notified when the user accepts the dialog
     */
    public static void showErrorDialog(Context context, int messageId, DialogInterface.OnClickListener positiveListener) {
        Drawable alertIcon = context.getResources().getDrawable(R.drawable.ic_report_problem);
        alertIcon.setColorFilter(context.getResources().getColor(R.color.colorAccent), PorterDuff.Mode.SRC_IN);

        new AlertDialog.Builder(context)
                .setTitle(R.string.error_dialog_title)
                .setMessage(messageId)
                .setPositiveButton(android.R.string.yes, positiveListener)
                .setIcon(alertIcon)
                .show();
    }
}
