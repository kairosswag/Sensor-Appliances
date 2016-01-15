package dryr.android.communication;

import android.content.Context;
import android.os.AsyncTask;
import android.util.Log;

import com.android.volley.DefaultRetryPolicy;
import com.android.volley.NoConnectionError;
import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.SimpleType;
import com.spothero.volley.JacksonRequest;
import com.spothero.volley.JacksonRequestListener;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

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
        httpGetJSON(context.getString(R.string.servlet_latest_dataPoint), new JacksonRequestListener<List<HumiditySensorDataPoint>>() {
            @Override
            public void onResponse(List<HumiditySensorDataPoint> response, int statusCode, VolleyError error) {
                if (response != null) {
                    Log.d(TAG, "response received: " + response.toString());
                    if (response.size() > 0) {
                        callback.onResult(response.get(0));
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
                return mapper.getTypeFactory().constructCollectionType(List.class, HumiditySensorDataPoint.class);
            }
        });

    }

    public void getSensorState(final CommunicationCallback<BluetoothDevice> callback) {
        httpGetJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 1) {
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
        httpGetJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    List<BluetoothDevice> sensors = new ArrayList<>();
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 1) {
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

    public void getAvailableSensors(final CommunicationCallback<List<BluetoothDevice>> callback) {
        httpGetJSON(context.getString(R.string.servlet_devices), new JacksonRequestListener<List<BluetoothDevice>>() {
            @Override
            public void onResponse(List<BluetoothDevice> response, int statusCode, VolleyError error) {
                if (response != null) {
                    List<BluetoothDevice> sensors = new ArrayList<>();
                    Log.d(TAG, "response received: " + response.toString());
                    for (BluetoothDevice device : response) {
                        if (device.getStatus() == 0) {
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

    public void pairAndRemove(List<BluetoothDevice> pair, List<BluetoothDevice> remove, final CommunicationCallback<ConcurrentHashMap<String, Boolean>> callback) {
        final ConcurrentHashMap<String, Boolean> successfull = new ConcurrentHashMap<>();
        final int size = pair.size() + remove.size();

        for (final BluetoothDevice p : pair) {
            httpGet(context.getString(R.string.servlet_device_connect) + p.getMac(), new Response.Listener<String>() {
                @Override
                public void onResponse(String response) {
                    Log.d(TAG, "response received: " + response.toString());
                    if (response.contains("connected")) { // TODO: make this nicer (future sprint)
                        successfull.put(p.getMac(), true);
                    } else {
                        successfull.put(p.getMac(), false);
                    }
                    checkPairAndRemoveFinished(size, successfull, callback);
                }
            }, new Response.ErrorListener() {
                @Override
                public void onErrorResponse(VolleyError error) {
                    Log.e(TAG, "error received: " + error.toString());
                    successfull.put(p.getMac(), false);
                    checkPairAndRemoveFinished(size, successfull, callback);
                }
            });
        }

        for (final BluetoothDevice r : remove) {
            httpGet(context.getString(R.string.servlet_device_connect) + r.getMac(), new Response.Listener<String>() {
                @Override
                public void onResponse(String response) {
                    Log.d(TAG, "response received: " + response.toString());
                    if (response.contains("disconnected")) { // TODO: make this nicer (future sprint)
                        successfull.put(r.getMac(), true);
                    } else {
                        successfull.put(r.getMac(), false);
                    }
                    checkPairAndRemoveFinished(size, successfull, callback);
                }
            }, new Response.ErrorListener() {
                @Override
                public void onErrorResponse(VolleyError error) {
                    Log.e(TAG, "error received: " + error.toString());
                    successfull.put(r.getMac(), false);
                    checkPairAndRemoveFinished(size, successfull, callback);
                }
            });
        }
    }

    private void checkPairAndRemoveFinished(int size, ConcurrentHashMap<String, Boolean> hashMap, CommunicationCallback<ConcurrentHashMap<String, Boolean>> callback) {
        if (hashMap.size() == size) {
            callback.onResult(hashMap);
        }
    }


    private void httpGet(String remainderUrl, int timeout, Response.Listener<String> listener, Response.ErrorListener errorListener) {
        Request<String> request = new StringRequest(Request.Method.GET, defaultUrl + remainderUrl, listener, errorListener);
        // Set timeout
        request.setRetryPolicy(new DefaultRetryPolicy(timeout, DefaultRetryPolicy.DEFAULT_MAX_RETRIES, DefaultRetryPolicy.DEFAULT_BACKOFF_MULT));
        RequestQueueProvider.getInstance().getRequestQueue().add(request);
    }

    private void httpGet(String remainderUrl, Response.Listener<String> listener, Response.ErrorListener errorListener) {
        httpGet(remainderUrl, defaultTimeout, listener, errorListener);
    }

    private <T> void httpGetJSON(String remainderUrl, int timeout, JacksonRequestListener<T> listener) {
        JacksonRequest<T> request = new JacksonRequest<T>(Request.Method.GET, defaultUrl + remainderUrl, listener);
        // Set timeout
        request.setRetryPolicy(new DefaultRetryPolicy(timeout, DefaultRetryPolicy.DEFAULT_MAX_RETRIES, DefaultRetryPolicy.DEFAULT_BACKOFF_MULT));
        RequestQueueProvider.getInstance().getRequestQueue().add(request);
    }

    private <T> void httpGetJSON(String remainderUrl, JacksonRequestListener<T> listener) {
        httpGetJSON(remainderUrl, defaultTimeout, listener);
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
