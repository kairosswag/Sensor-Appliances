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
import dryr.android.db.HumidityTable;
import dryr.android.presenter.MainActivity;
import dryr.android.utils.ConfigUtil;
import dryr.common.json.beans.BluetoothDevice;
import dryr.common.json.beans.Dry;
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
                updateSensors();
                updateDryState();
            }
        }, 0, getResources().getInteger(R.integer.background_service_check_laundry_state_frequency_period), TimeUnit.SECONDS);

        // Make service sticky, restart it whenever it is stopped
        return START_STICKY;
    }

    private void updateSensors() {
        CommunicationFacade.getInstance(getApplicationContext()).getPairedSensors(new CommunicationFacade.CommunicationCallback<List<BluetoothDevice>>() {
            @Override
            public void onResult(List<BluetoothDevice> result) {
                for (BluetoothDevice sensor : result) {
                    updateHumidity(sensor.getMac());
                }
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
            }

            @Override
            public Object getTag() {
                return DryRBackgroundService.this;
            }
        });
    }

    private void updateDryState() {
        CommunicationFacade.getInstance(getApplicationContext()).isDry(new CommunicationFacade.CommunicationCallback<List<Dry>>() {
            @Override
            public void onResult(List<Dry> result) {
                for (Dry d : result) {
                    if (d.getDry()) {
                        showDryNotification(d.getMac());
                    }
                }
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
            }

            @Override
            public Object getTag() {
                return this;
            }
        });
    }

    private void updateHumidity(String mac) {
        CommunicationFacade.getInstance(getApplicationContext()).getHumidity(mac, new CommunicationFacade.CommunicationCallback<HumiditySensorDataPoint>() {
            @Override
            public void onResult(HumiditySensorDataPoint result) {
                HumidityTable.getInstance(getApplicationContext()).addDataPoint(result);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
            }

            @Override
            public Object getTag() {
                return DryRBackgroundService.this;
            }
        });
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        serviceRunning = false;
        // cancel sent requests
        CommunicationFacade.getInstance(getApplicationContext()).cancelAllByTag(this);
    }

    private void showDryNotification(String mac) {
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
            resultIntent.putExtra(MainActivity.SENSOR_MAC_EXTRA, mac);
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
