package dryr.android.background;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.v4.app.NotificationCompat;

import java.util.ArrayList;
import java.util.List;

import dryr.android.R;
import dryr.android.presenter.MainActivity;

/**
 * Background Service that handles communication with the BaseStation that is not requested by the UI
 */
public class DryRBackgroundService extends Service {
    public static final String APP_RUNNING_KEY = "app_running";

    private static DryRBackgroundService instance;
    private static boolean serviceRunning = false;

    public static DryRBackgroundService getInstance() {
        return instance;
    }

    public static boolean isServiceRunning() {
        return serviceRunning;
    }

    // TODO: Find BaseStation via broadcast and get Information regularly, or let BaseStation push information to this service (How?)
    // TODO: Display notification if needed
    // TODO: Save information from BaseStation in database for diagram (in a later Sprint)

    private boolean appRunning = false;
    private List<DryRBackgroundServiceListener> listener = new ArrayList<>();

    public DryRBackgroundService() {
        instance = this;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent != null) {
            appRunning = intent.getBooleanExtra(APP_RUNNING_KEY, false);
        } else {
            appRunning = false;
        }
        serviceRunning = true;

        // Make service sticky, restart it whenever it is stopped
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        serviceRunning = false;
    }

    private void showDryNotification() {
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());
        boolean pushNotifications = sp.getBoolean(getResources().getString(R.string.pref_notification_push_key), true);

        // Shows a notification to the user that the laundry is dry now
        if (pushNotifications && !appRunning) {
            NotificationCompat.Builder builder =
                    new NotificationCompat.Builder(this)
                            .setSmallIcon(R.drawable.ic_menu_shirt)
                            .setContentTitle(getResources().getString(R.string.notification_laundry_dry_title))
                            .setContentText(getResources().getString(R.string.notification_laundry_dry_text));

            Intent resultIntent = new Intent(this, MainActivity.class);
            PendingIntent resultPendingIntent =
                    PendingIntent.getActivity(
                            this,
                            0,
                            resultIntent,
                            PendingIntent.FLAG_UPDATE_CURRENT
                    );

            builder.setContentIntent(resultPendingIntent);

            NotificationManager mNotifyMgr =
                    (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
            mNotifyMgr.notify(1, builder.build());
        }
    }

    public void setAppRunning(boolean running) {
        this.appRunning = running;
    }

    public void addListener(DryRBackgroundServiceListener listener) {
        this.listener.add(listener);
    }

    /**
     * Listener to be informed over events
     */
    public interface DryRBackgroundServiceListener {

    }
}
