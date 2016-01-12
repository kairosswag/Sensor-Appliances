package dryr.android.communication;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.preference.PreferenceManager;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.model.BaseStation;
import dryr.android.model.LaundryState;
import dryr.android.model.Sensor;
import dryr.android.model.SensorState;

/**
 * Facade supposed to handle communication
 */
public class CommunicationFacade {
    public enum CommunicationError {
        /**
         * No base station has been connected to this app
         *
        NO_BASE_STATION_CONNECTED,*/
        /**
         * No sensor was paired to the base station
         */
        NO_BASE_STATION_FOUND,
        NO_SENSOR_PAIRED,
        UNKNOWN
    }

    private static CommunicationFacade ourInstance;

    public static CommunicationFacade getInstance(Context context) {
        if (ourInstance == null) {
            ourInstance = new CommunicationFacade(context);
        }
        return ourInstance;
    }

    private Context context;

    private CommunicationFacade(Context context) {
        this.context = context;
    }

    public void getLaundryState(final CommunicationCallback<LaundryState> callback) {
        // TODO: connect / check connection / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {

            @Override
            protected Object doInBackground(Object[] params) {
                // TODO

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onResult(new LaundryState(false));
            }
        };



        asyncTask.execute(null);

    }

    public void getSensorState(final CommunicationCallback<SensorState> callback) {
        // TODO: connect / check connection / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {


            @Override
            protected Object doInBackground(Object[] params) {
                // TODO

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onResult(new SensorState(70,26));
            }
        };

        asyncTask.execute(null);

    }

    public void getAvailableBaseStations(final CommunicationCallback<List<BaseStation>> callback) {
        // TODO: find base stations / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {

            ArrayList<BaseStation> testStations;

            @Override
            protected Object doInBackground(Object[] params) {
                // TODO
                testStations = new ArrayList<>();
                for (int i = 0; i < 10; i++) {
                    testStations.add(new BaseStation("test " + i));
                }

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onResult(testStations);
            }
        };

        asyncTask.execute(null);
    }

    public void tryConnection(BaseStation station, final CommunicationCallbackBinary callback) {
        // TODO: connect to base station / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {

            @Override
            protected Object doInBackground(Object[] params) {
                // TODO

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onSuccess();
            }
        };

        asyncTask.execute(null);
    }

    public void connectPermanently(BaseStation station) {

    }

    public void disconnectFromStation(String identifier) {

    }

    public void getPairedSensors(final CommunicationCallback<List<Sensor>> callback) {
        // TODO: find base stations / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {

            ArrayList<Sensor> testSensors;

            @Override
            protected Object doInBackground(Object[] params) {
                // TODO
                testSensors = new ArrayList<>();
                for (int i = 0; i < 10; i++) {
                    testSensors.add(new Sensor("test " + i, (int) (Math.random() * 100)));
                }

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onResult(testSensors);
            }
        };

        asyncTask.execute(null);
    }

    public void getAvailableSensors(final CommunicationCallback<List<Sensor>> callback) {
        // TODO: find base stations / run in background / return result

        AsyncTask asyncTask = new AsyncTask() {

            ArrayList<Sensor> testSensors;

            @Override
            protected Object doInBackground(Object[] params) {
                // TODO
                testSensors = new ArrayList<>();
                for (int i = 0; i < 10; i++) {
                    testSensors.add(new Sensor("test " + i, (int) (Math.random() * 100)));
                }

                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onResult(testSensors);
            }
        };

        asyncTask.execute(null);
    }

    public void pairAndRemove(List<Sensor> pair, List<Sensor> remove, final CommunicationCallbackBinary callback) {
        AsyncTask asyncTask = new AsyncTask() {
            @Override
            protected Object doInBackground(Object[] params) {
                // TODO
                return null;
            }

            @Override
            protected void onPostExecute(Object o) {
                super.onPostExecute(o);
                callback.onSuccess();
            }
        };

        asyncTask.execute(null);
    }

    /**
     * Callback to inform a caller over a finished communication action
     * @param <T> the type that is expected to be the result
     */
    public interface CommunicationCallback<T> {
        /**
         * Called when the action has been successfully executed
         * @param result the result of the action (if there is one)
         */
        public void onResult(T result);

        /**
         * Called when an error occurred during the communication action
         * @param error the error
         */
        public void onError(CommunicationError error);
    }

    /**
     * Callback to inform a caller over a finished communication action
     */
    public interface CommunicationCallbackBinary {
        /**
         * Called when the action has been successfully executed
         */
        public void onSuccess();

        /**
         * Called when an error occurred during the communication action
         * @param error the error
         */
        public void onError(CommunicationError error);
    }
}
