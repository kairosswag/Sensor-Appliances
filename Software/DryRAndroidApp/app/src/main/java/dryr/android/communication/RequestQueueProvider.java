package dryr.android.communication;

import android.content.Context;

import com.android.volley.Cache;
import com.android.volley.Network;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.BasicNetwork;
import com.android.volley.toolbox.HurlStack;
import com.android.volley.toolbox.NoCache;
import com.android.volley.toolbox.Volley;

/**
 * Provides and creates a volley RequestQueue if needed. Acts as a Singleton. Provides convenience methods
 */
public class RequestQueueProvider {
    private static RequestQueueProvider instance;
    public static RequestQueueProvider getInstance(/* Context context */) {
        if (instance == null) {
            instance = new RequestQueueProvider(/* context */);
        }
        return instance;
    }

    // private Context context;
    private RequestQueue requestQueue;
    private Cache cache;
    private Network network;

    public RequestQueueProvider(/* Context context */) {
        // this.context = context;

        cache = new NoCache();
        network = new BasicNetwork(new HurlStack());
    }

    /**
     * Get the requestQueue to make requests on
     * @return the RequestQueue
     */
    public RequestQueue getRequestQueue() {
        if (requestQueue == null) {
            requestQueue = new RequestQueue(cache, network);
            requestQueue.start();
        }
        return requestQueue;
    }
}
