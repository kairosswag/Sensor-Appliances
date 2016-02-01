package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.presenter.listener.RefreshListener;
import dryr.android.utils.ConfigUtil;
import dryr.android.utils.ViewUtil;
import dryr.android.views.MessageView;
import dryr.common.json.beans.BluetoothDevice;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link SensorStatusFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.
 */
public class SensorStatusFragment extends Fragment implements RefreshListener {
    public static final String TAG = "sensorStatus";
    private OnFragmentInteractionListener mListener;

    // UI
    private LinearLayout sensorStatusLayout;
    private ProgressBar connectionBar;

    private MessageView messageView;

    private ProgressBar progressBar;

    // Data
    private BluetoothDevice sensorState;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;
    private boolean refreshing;

    public SensorStatusFragment() {
        // Required empty public constructor
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    @Override
    public void onResume() {
        super.onResume();

        // refresh regularly
        if (!refreshingScheduled) {
            int period = getResources().getInteger(R.integer.sensor_fragment_status_refresh_frequency_period);

            refreshingScheduled = true;
            task = scheduledThreadPoolExecutor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    if (!refreshing) refresh(true);
                }
            }, period, period, TimeUnit.SECONDS);
        }

        mListener.registerForRefresh(this);
    }

    @Override
    public void onPause() {
        super.onPause();

        // stop refreshing
        task.cancel(true);
        refreshingScheduled = false;

        // cancel already sent requests
        CommunicationFacade.getInstance(getActivity()).cancelAllByTag(this);

        mListener.unregisterForRefresh(this);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_sensor_status, container, false);

        sensorStatusLayout = (LinearLayout) v.findViewById(R.id.sensor_status_layout);
        connectionBar = (ProgressBar) v.findViewById(R.id.sensor_status_connection_bar);
        connectionBar.setMax(100);

        messageView = (MessageView) v.findViewById(R.id.sensor_status_message_view);
        progressBar = (ProgressBar) v.findViewById(R.id.sensor_status_progress);

        if (sensorState != null) {
            setSensorState(sensorState);
        }

        return v;
    }

    @Override
    public void refresh() {
        refresh(false);
    }

    /**
     * refresh the sensor state with data from the base station
     *
     * @param silent displays no (dialog) error message on error
     */
    public void refresh(final boolean silent) {
        refreshing = true;
        showProgress(true);

        // Refresh Sensor state
        CommunicationFacade.getInstance(getContext()).getSensorState(sensorState.getMac(), new CommunicationFacade.CommunicationCallback<BluetoothDevice>() {
            @Override
            public void onResult(BluetoothDevice result) {
                refreshing = false;

                enableStateLayout(true);
                messageView.showMessage(R.string.laundry_status_base_station_connected, R.color.light_success_color, 0, false, null);
                setSensorState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                refreshing = false;

                switch (error) {
                    case NO_BASE_STATION_FOUND:
                        // Base station was not found in network
                        messageView.showMessage(R.string.error_no_base_station_found, R.string.error_retry, R.color.light_error_text_color, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refresh(false);
                            }
                        });

                    case NO_SENSOR_PAIRED:
                        // Show settings activity with PairSensorDialog open
                        Intent intent = new Intent(getContext(), DryRPreferenceActivity.class);
                        intent.putExtra(DryRPreferenceActivity.OPEN_PREFERENCE_KEY, getString(R.string.pref_sensor_pair_key));
                        getActivity().startActivity(intent);

                        break;

                    default:
                        messageView.showMessage(R.string.error_connection_default, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refresh(false);
                            }
                        });

                }
                showProgress(false);
                enableStateLayout(false);
            }

            @Override
            public Object getTag() {
                return SensorStatusFragment.this;
            }
        });
    }

    private void enableStateLayout(boolean enable) {
        sensorStatusLayout.setEnabled(enable);
        if (!enable) {
            ViewUtil.fadeOut(sensorStatusLayout, getActivity(), 0.2f, View.VISIBLE);
        } else {
            ViewUtil.fadeIn(sensorStatusLayout, getActivity());
        }
    }

    private void showProgress(final boolean show) {
        getActivity().runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if (show) {
                    ViewUtil.fadeIn(progressBar, getActivity());
                } else {
                    // Add delay so the animation doesn't look weird
                    new ScheduledThreadPoolExecutor(1).schedule(new Runnable() {
                        @Override
                        public void run() {
                            getActivity().runOnUiThread(new Runnable() {
                                @Override
                                public void run() {
                                    ViewUtil.fadeOut(progressBar, getActivity(), View.INVISIBLE);
                                }
                            });
                        }
                    }, getResources().getInteger(R.integer.progress_delay), TimeUnit.MILLISECONDS);
                }
            }
        });
    }

    public void setSensorState(BluetoothDevice sensorState) {
        this.sensorState = sensorState;
        if (sensorStatusLayout != null) {
            connectionBar.setProgress((int) (ConfigUtil.convertRssi(sensorState.getRSSI(), getActivity()) * 100));
        }
    }


    @Override
    public void onAttach(Context context) {
        super.onAttach(context);
        if (context instanceof OnFragmentInteractionListener) {
            mListener = (OnFragmentInteractionListener) context;
        } else {
            throw new RuntimeException(context.toString()
                    + " must implement OnFragmentInteractionListener");
        }
    }

    @Override
    public void onDetach() {
        super.onDetach();
        mListener = null;
    }

    /**
     * This interface must be implemented by activities that contain this
     * fragment to allow an interaction in this fragment to be communicated
     * to the activity and potentially other fragments contained in that
     * activity.
     */
    public interface OnFragmentInteractionListener {
        public void registerForRefresh(RefreshListener listener);
        public void unregisterForRefresh(RefreshListener listener);
    }
}
