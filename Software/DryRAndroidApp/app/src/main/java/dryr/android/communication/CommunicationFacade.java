package dryr.android.communication;

import android.os.AsyncTask;

import java.util.ArrayList;
import java.util.List;

import dryr.android.model.BaseStation;
import dryr.android.model.LaundryState;
import dryr.android.model.SensorState;

/**
 * Facade supposed to handle communication
 */
public class CommunicationFacade {
    public enum CommunicationError {
        /**
         * No base station has been connected to this app
         */
        NO_BASE_STATION_CONNECTED,
        /**
         * No sensor was paired to the base station
         */
        NO_SENSOR_PAIRED
    };

    private static CommunicationFacade ourInstance = new CommunicationFacade();

    public static CommunicationFacade getInstance() {
        return ourInstance;
    }

    private CommunicationFacade() {
    }

    public void getLaundryState(CommunicationCallback<LaundryState> callback) {
        // TODO: connect / check connection / run in background / return result
        // TODO: regularly check this...
        callback.onResult(new LaundryState(false));
    }

    public void getSensorState(CommunicationCallback<SensorState> callback) {
        // TODO: connect / check connection / run in background / return result
        // TODO: regularly check this...
        callback.onResult(new SensorState(70,26));
    }

    public void getAvailableBaseStations(CommunicationCallback<List<BaseStation>> callback) {
        // TODO: find base stations / run in background / return result
        ArrayList<BaseStation> testStations = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            testStations.add(new BaseStation("test " + i));
        }
        callback.onResult(testStations);
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
}
