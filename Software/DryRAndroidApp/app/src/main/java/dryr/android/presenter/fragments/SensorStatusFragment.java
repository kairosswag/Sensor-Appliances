package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.utils.DialogUtil;
import dryr.android.utils.ViewUtil;
import dryr.common.json.beans.BluetoothDevice;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link SensorStatusFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.

 */
public class SensorStatusFragment extends Fragment {
    public static final String TAG = "sensorStatus";
    private OnFragmentInteractionListener mListener;

    // UI
    private LinearLayout sensorStatusLayout;
    private ProgressBar connectionBar;

    private TextView messageView;
    private Button messageButton;

    private ProgressBar progressBar;

    // Data
    private BluetoothDevice sensorState;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;

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
            refreshSensorState(false);
            int period = getResources().getInteger(R.integer.sensor_fragment_status_refresh_frequency_period);

            refreshingScheduled = true;
            task = scheduledThreadPoolExecutor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    refreshSensorState(true);
                }
            }, period, period, TimeUnit.SECONDS);
        }
    }

    @Override
    public void onPause() {
        super.onPause();

        // stop refreshing
        task.cancel(true);
        refreshingScheduled = false;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_sensor_status, container, false);

        sensorStatusLayout = (LinearLayout) v.findViewById(R.id.sensor_status_layout);
        connectionBar = (ProgressBar) v.findViewById(R.id.sensor_status_connection_bar);

        messageView = (TextView) v.findViewById(R.id.sensor_status_message_view);
        messageButton = (Button) v.findViewById(R.id.sensor_status_message_button);

        progressBar = (ProgressBar) v.findViewById(R.id.sensor_status_progress);

        setHasOptionsMenu(true);

        if (sensorState != null) {
            setSensorState(sensorState);
        }

        showProgress(true);


        // TODO: Use tabs for multiple sensors (future sprint)

        return v;
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.sensor_state, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();

        switch (id) {
            case R.id.sensor_state_refresh:
                refreshSensorState(false);
                return true;
        }

        return super.onOptionsItemSelected(item);
    }

    /**
     * refresh the sensor state with data from the base station
     * @param silent displays no error message on error
     */
    private void refreshSensorState(final boolean silent) {
        showProgress(true);

        // Refresh Sensor state
        CommunicationFacade.getInstance(getContext()).getSensorState(new CommunicationFacade.CommunicationCallback<BluetoothDevice>() {
            @Override
            public void onResult(BluetoothDevice result) {
                enableStateLayout(true);
                showMessage(R.string.laundry_status_base_station_connected, R.color.light_success_color, 0, false, null);
                setSensorState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {

                switch (error) {
                    case NO_BASE_STATION_FOUND:
                        // Base station was not found in network
                        showMessage(R.string.error_no_base_station_found, R.string.error_retry, R.color.light_error_text_color, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refreshSensorState(false);
                            }
                        });

                    case NO_SENSOR_PAIRED:
                        // TODO: For multiple devices do this outside of this fragment!!!
                        // Show settings activity with PairSensorDialog open
                        Intent intent = new Intent(getContext(), DryRPreferenceActivity.class);
                        intent.putExtra(DryRPreferenceActivity.OPEN_PREFERENCE_KEY, getString(R.string.pref_sensor_pair_key));
                        getActivity().startActivity(intent);

                        break;

                    default:
                        if (silent) {
                            showMessage(R.string.error_connection_default, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                                @Override
                                public void onClick(View v) {
                                    refreshSensorState(false);
                                }
                            });
                        } else {
                            DialogUtil.showErrorDialog(getActivity(), R.string.error_connection_default, null);
                        }
                }
                showProgress(false);
                enableStateLayout(false);
            }
        });
    }

    private void enableStateLayout(boolean enable) {
        sensorStatusLayout.setEnabled(enable);
        if (enable) {
            ViewUtil.fadeOut(sensorStatusLayout, getActivity(), 0.2f, View.VISIBLE);
        } else {
            ViewUtil.fadeIn(sensorStatusLayout, getActivity());
        }
    }

    private void showMessage(int messageTextId, int messageTextColorId, int buttonTextId, boolean showButton, View.OnClickListener clickListener) {
        messageView.setText(messageTextId);
        messageView.setTextColor(getResources().getColor(messageTextColorId));
        if (showButton) {
            messageButton.setText(buttonTextId);
            messageButton.setOnClickListener(clickListener);
            if (messageButton.getVisibility() == View.GONE) {
                ViewUtil.fadeIn(messageButton, getActivity());
            }
        } else {
            messageButton.setVisibility(View.GONE);
        }
        if (messageView.getVisibility() == View.GONE) {
            ViewUtil.fadeIn(messageView, getActivity());
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
            connectionBar.setProgress(10); // TODO: Get connection level from bluetooth device
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
    }
}
