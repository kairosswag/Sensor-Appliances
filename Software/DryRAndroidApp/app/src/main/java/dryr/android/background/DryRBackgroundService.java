package dryr.android.background;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.support.v4.app.NotificationCompat;
import android.util.TypedValue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.presenter.MainActivity;
import dryr.common.json.beans.HumiditySensorDataPoint;

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

    // TODO: Save information from BaseStation in database for diagram (in a later Sprint)

    private boolean appRunning = false;
    private List<DryRBackgroundServiceListener> listener = new ArrayList<>();

    private ScheduledThreadPoolExecutor threadPoolExecutor = new ScheduledThreadPoolExecutor(4);

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

        // Regularly check if laundry is dry
        threadPoolExecutor.scheduleAtFixedRate(new Runnable() {
            @Override
            public void run() {
                CommunicationFacade.getInstance(getApplicationContext()).getLaundryState(new CommunicationFacade.CommunicationCallback<HumiditySensorDataPoint>() {
                    @Override
                    public void onResult(HumiditySensorDataPoint result) {
                        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());

                        TypedValue outValue = new TypedValue();
                        getResources().getValue(R.dimen.sensor_humidity_dry_threshold, outValue, true);
                        float threshold = outValue.getFloat();

                        if (result.getHumidity() <= threshold &&
                                sp.getFloat(getString(R.string.pref_notification_last_humidity_key), Float.MAX_VALUE) > threshold) {

                            // Show notification "dry"
                            showDryNotification();
                        }

                        // Make sure the user is only notified again if the laundry got (more) wet again -> new laundry
                        sp.edit().putFloat(getString(R.string.pref_notification_last_humidity_key), result.getHumidity()).apply();
                    }

                    @Override
                    public void onError(CommunicationFacade.CommunicationError error) {
                        // Do nothing
                    }

                    @Override
                    public Object getTag() {
                        return DryRBackgroundService.this;
                    }
                });
            }
        }, 0, getResources().getInteger(R.integer.background_service_check_laundry_state_frequency_period), TimeUnit.SECONDS);

        // Make service sticky, restart it whenever it is stopped
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        serviceRunning = false;
        // cancel sent requests
        CommunicationFacade.getInstance(getApplicationContext()).cancelAllByTag(this);
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
            builder.setAutoCancel(true);
            builder.setOnlyAlertOnce(true);
            builder.setVibrate(new long[] {0, 500, 200, 500});

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
