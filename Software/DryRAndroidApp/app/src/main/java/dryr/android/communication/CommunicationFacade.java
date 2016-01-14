package dryr.android.communication;

import android.content.Context;
import android.os.AsyncTask;
import android.util.Log;

import com.android.volley.DefaultRetryPolicy;
import com.android.volley.NoConnectionError;
import com.android.volley.Request;
import com.android.volley.VolleyError;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.fasterxml.jackson.databind.type.SimpleType;
import com.spothero.volley.JacksonRequest;
import com.spothero.volley.JacksonRequestListener;

import java.util.ArrayList;
import java.util.List;

import dryr.android.R;
import dryr.common.json.beans.BluetoothDevice;
import dryr.common.json.beans.HumiditySensorDataPoint;

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
         * The base station was not found in the network
         */
        NO_BASE_STATION_FOUND,
        /**
         * No sensor was paired to the base station
         */
        NO_SENSOR_PAIRED,
        /**
         * There is no connection to any network
         */
        NO_NETWORK,
        UNKNOWN
    }

    private static final String TAG = "CommunicationFacade";

    private static CommunicationFacade ourInstance;

    public static CommunicationFacade getInstance(Context context) {
        if (ourInstance == null) {
            ourInstance = new CommunicationFacade(context);
        }
        return ourInstance;
    }

    private Context context;

    private String defaultUrl;
    private int defaultTimeout;

    private CommunicationFacade(Context context) {
        this.context = context;

        String protocol = context.getString(R.string.default_server_protocol);
        String hostName = context.getString(R.string.default_base_station_host_name);
        String port = context.getString(R.string.default_server_port);
        defaultUrl = protocol + "://" + hostName + ":" + port + context.getString(R.string.default_server_base_url);
        defaultTimeout = context.getResources().getInteger(R.integer.default_timeout_ms);
    }

    public void getLaundryState(final CommunicationCallback<HumiditySensorDataPoint> callback) {
        sendJSON(context.getString(R.string.servlet_latest_dataPoint), new JacksonRequestListener<HumiditySensorDataPoint>() {
            @Override
            public void onResponse(HumiditySensorDataPoint response, int statusCode, VolleyError error) {
                if (response != null) {
                    Log.d(TAG, "response received: " + response.toString());
                    callback.onResult(response);
                } else {
                    // TODO: no sensors connected error
                    callback.onError(convertError(error));
                    Log.e(TAG, "error received: " + error.toString());
                }
            }

            @Override
            public JavaType getReturnType() {
                return SimpleType.construct(HumiditySensorDataPoint.class);
            }
        });

    }

    public void getSensorState(final CommunicationCallback<BluetoothDevice> callback) {
        sendJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 1) { // TODO: status connected and "paired" / in use for laundry status
                            callback.onResult(device);
                            return;
                        }
                    }
                    callback.onError(CommunicationError.NO_SENSOR_PAIRED);
                } else {
                    callback.onError(convertError(error));
                    Log.e(TAG, "error received: " + error.toString());
                }
            }

            @Override
            public JavaType getReturnType() {
                ObjectMapper mapper = new ObjectMapper();
                return mapper.getTypeFactory().constructCollectionType(List.class, BluetoothDevice.class);
            }
        });

    }

    public void getPairedSensors(final CommunicationCallback<List<BluetoothDevice>> callback) {
        sendJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    List<BluetoothDevice> sensors = new ArrayList<>();
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 1) { // TODO: status connected and "paired" / in use for laundry status
                            sensors.add(device);
                        }
                    }
                    if (sensors.size() > 0) {
                        callback.onResult(sensors);
                    } else {
                        callback.onError(CommunicationError.NO_SENSOR_PAIRED);
                    }
                } else {
                    callback.onError(convertError(error));
                    Log.e(TAG, "error received: " + error.toString());
                }
            }

            @Override
            public JavaType getReturnType() {
                ObjectMapper mapper = new ObjectMapper();
                return mapper.getTypeFactory().constructCollectionType(List.class, BluetoothDevice.class);
            }
        });
    }

    public void getAvailableSensors(final CommunicationCallback<List<BluetoothDevice>> callback) {
        sendJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    List<BluetoothDevice> sensors = new ArrayList<>();
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 0) { // TODO: status available and not in use for laundry status
                            sensors.add(device);
                        }
                    }
                    callback.onResult(sensors);
                } else {
                    callback.onError(convertError(error));
                    Log.e(TAG, "error received: " + error.toString());
                }
            }

            @Override
            public JavaType getReturnType() {
                ObjectMapper mapper = new ObjectMapper();
                return mapper.getTypeFactory().constructCollectionType(List.class, BluetoothDevice.class);
            }
        });
    }

    public void pairAndRemove(List<BluetoothDevice> pair, List<BluetoothDevice> remove, final CommunicationCallbackBinary callback) {
        // TODO (how?)

        AsyncTask asyncTask = new AsyncTask() {
            @Override
            protected Object doInBackground(Object[] params) {
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

    private <T> void sendJSON(String remainderUrl, int timeout, JacksonRequestListener<T> listener) {
        JacksonRequest<T> request = new JacksonRequest<T>(Request.Method.GET, defaultUrl + remainderUrl, listener);
        // Set timeout
        request.setRetryPolicy(new DefaultRetryPolicy(timeout, DefaultRetryPolicy.DEFAULT_MAX_RETRIES, DefaultRetryPolicy.DEFAULT_BACKOFF_MULT));
        RequestQueueProvider.getInstance().getRequestQueue().add(request);
    }

    private <T> void sendJSON(String remainderUrl, JacksonRequestListener<T> listener) {
        sendJSON(remainderUrl, defaultTimeout, listener);
    }

    private CommunicationError convertError(VolleyError error) {
        if (error instanceof NoConnectionError) {
            return CommunicationError.NO_BASE_STATION_FOUND;
        } else {
            return CommunicationError.UNKNOWN;
        }
    }

    /**
     * Callback to inform a caller over a finished communication action
     *
     * @param <T> the type that is expected to be the result
     */
    public interface CommunicationCallback<T> {
        /**
         * Called when the action has been successfully executed
         *
         * @param result the result of the action (if there is one)
         */
        public void onResult(T result);

        /**
         * Called when an error occurred during the communication action
         *
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
         *
         * @param error the error
         */
        public void onError(CommunicationError error);
    }
}
