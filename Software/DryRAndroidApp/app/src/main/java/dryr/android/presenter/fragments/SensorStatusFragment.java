package dryr.android.presenter.fragments;

import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.model.SensorState;
import dryr.android.utils.ViewUtil;

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
    private ProgressBar batteryBar;
    private ProgressBar connectionBar;

    private ProgressBar progressBar;

    // Data
    private SensorState sensorState;

    public SensorStatusFragment() {
        // Required empty public constructor
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_sensor_status, container, false);

        sensorStatusLayout = (LinearLayout) v.findViewById(R.id.sensor_status_layout);
        batteryBar = (ProgressBar) v.findViewById(R.id.sensor_status_battery_bar);
        connectionBar = (ProgressBar) v.findViewById(R.id.sensor_status_connection_bar);

        progressBar = (ProgressBar) v.findViewById(R.id.sensor_status_progress);

        if (sensorState != null) {
            setSensorState(sensorState);
        }

        showProgress(true);
        // Refresh Sensor state
        CommunicationFacade.getInstance().getSensorState(new CommunicationFacade.CommunicationCallback<SensorState>() {
            @Override
            public void onResult(SensorState result) {
                setSensorState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    case NO_BASE_STATION:
                        // TODO: Show settings activity with ConnectBaseStationDialog open
                        break;

                    case NO_SENSOR_PAIRED:
                        // TODO: Show settings activity with PairSensorDialog open
                        break;

                    default:
                        // TODO: Show no network connection error
                        showProgress(false);
                }
            }
        });

        return v;
    }

    private void showProgress(boolean show) {
        if (show) {
            ViewUtil.fade(sensorStatusLayout, progressBar, getActivity());
        } else {
            ViewUtil.fade(progressBar, sensorStatusLayout, getActivity());
        }
    }

    public void setSensorState(SensorState sensorState) {
        this.sensorState = sensorState;
        if (sensorStatusLayout != null) {
            batteryBar.setProgress(sensorState.getBatteryLevel());
            connectionBar.setProgress(sensorState.getConnectionLevel());
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
