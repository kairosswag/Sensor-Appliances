package dryr.android.background;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

/**
 * Provides access to the DryRBackgroundService, ensures that the Service is running
 */
public class DryRBackgroundServiceProvider extends BroadcastReceiver {

    private static DryRBackgroundServiceProvider instance;

    public static DryRBackgroundServiceProvider getInstance() {
        if (instance == null) {
            instance = new DryRBackgroundServiceProvider();
        }
        return instance;
    }

    public DryRBackgroundService getService(Context context) {

        if (DryRBackgroundService.getInstance() == null) {
            context.startService(new Intent(context, DryRBackgroundService.class));
        }

        return DryRBackgroundService.getInstance();
    }

    public boolean startService(Context context, boolean initialAppRunningState) {
        if (DryRBackgroundService.getInstance() == null) {
            Intent intent = new Intent(context, DryRBackgroundService.class);
            intent.putExtra(DryRBackgroundService.APP_RUNNING_KEY, initialAppRunningState);
            context.startService(intent);
            return true;
        }
        return false;
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        // Start service if a broadcast is received, in this case a broadcast will be sent when the device boots
        startService(context, false);
    }
}
