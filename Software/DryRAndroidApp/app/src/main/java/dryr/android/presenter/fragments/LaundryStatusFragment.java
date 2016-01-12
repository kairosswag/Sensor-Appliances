package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.model.LaundryState;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.utils.DialogUtil;
import dryr.android.utils.ViewUtil;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link LaundryStatusFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.
 */
public class LaundryStatusFragment extends Fragment {
    public static final String TAG = "main";
    private OnFragmentInteractionListener mListener;

    // UI
    private ProgressBar progressBar;
    private RelativeLayout laundryStateLayout;
    private TextView laundryStateText;

    private TextView messageView;
    private Button messageButton;

    // Data
    private LaundryState laundryState;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;

    public LaundryStatusFragment() {
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
            refreshState(false);
            int period = getResources().getInteger(R.integer.sensor_fragment_status_refresh_frequency_period);

            refreshingScheduled = true;
            task = scheduledThreadPoolExecutor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    // TODO: Show smaller progress bar for auto refresh (future sprint)
                    refreshState(true);
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
        View v = inflater.inflate(R.layout.fragment_laundry_status, container, false);

        progressBar = (ProgressBar) v.findViewById(R.id.laundry_status_progress);
        laundryStateLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_layout);
        laundryStateText = (TextView) v.findViewById(R.id.laundry_status_displayText);

        messageView = (TextView) v.findViewById(R.id.laundry_status_message_view);
        messageView.setVisibility(View.GONE); // TODO: Show more messages about connection etc. in a future Sprint
        messageButton = (Button) v.findViewById(R.id.laundry_status_message_button);

        setHasOptionsMenu(true);


        if (laundryState != null) {
            setLaundryState(laundryState);
        }

        return v;
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.laundry_state, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();

        switch (id) {
            case R.id.laundry_status_refresh:
                refreshState(false);
                return true;
        }

        return super.onOptionsItemSelected(item);
    }

    /**
     * Refresh laundry state
     * @param silent don't display error messages
     */
    private void refreshState(final boolean silent) {
        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).getLaundryState(new CommunicationFacade.CommunicationCallback<LaundryState>() {
            @Override
            public void onResult(LaundryState result) {

                setLaundryState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                if (silent) {
                    showProgress(false);
                    return;
                }

                switch (error) {
                    /*case NO_BASE_STATION_CONNECTED:

                        Intent intent = new Intent(getContext(), DryRPreferenceActivity.class);
                        intent.putExtra(DryRPreferenceActivity.OPEN_PREFERENCE_KEY, getString(R.string.pref_baseStation_connect_key));
                        getActivity().startActivity(intent);

                        break;*/
                    case NO_BASE_STATION_FOUND:

                        // Base station was not found in network
                        showMessage(R.string.error_no_base_station_found, R.string.error_no_bs_retry, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refreshState(false);
                            }
                        });

                        break;

                    case NO_SENSOR_PAIRED:

                        // Show message informing about the fact that there are no sensors paired and
                        // display button to pair some
                        showMessage(R.string.laundry_status_no_sensor, R.string.pref_sensor_pair_title, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                // Show settings activity with PairSensorDialog open
                                Intent intent = new Intent(getContext(), DryRPreferenceActivity.class);
                                intent.putExtra(DryRPreferenceActivity.OPEN_PREFERENCE_KEY, getString(R.string.pref_sensor_pair_key));
                                getActivity().startActivity(intent);
                            }
                        });

                        break;

                    default:
                        DialogUtil.showErrorDialog(getActivity(), R.string.error_connection_default, null);
                }

                showProgress(false);
            }
        });
    }

    private void showMessage(int messageTextId, int buttonTextId, boolean showButton, View.OnClickListener clickListener) {
        messageView.setText(messageTextId);
        if (showButton) {
            messageButton.setText(buttonTextId);
            messageButton.setOnClickListener(clickListener);
            ViewUtil.fadeIn(messageButton, getActivity());
        }
        ViewUtil.fadeIn(messageView, getActivity());
        laundryStateLayout.setEnabled(false);

    }

    public void setLaundryState(LaundryState laundryState) {
        this.laundryState = laundryState;
        if (laundryStateText != null) {
            if (laundryState.isDry()) {
                laundryStateText.setText(R.string.laundry_status_dry);
                laundryStateText.setTextColor(getResources().getColor(R.color.laundry_status_dry_color));
            } else {
                laundryStateText.setText(R.string.laundry_status_drying);
                laundryStateText.setTextColor(getResources().getColor(R.color.laundry_status_drying_color));
            }
        }
    }

    private void showProgress(final boolean show) {
        getActivity().runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if (show) {
                    ViewUtil.fade(laundryStateLayout, progressBar, getActivity());
                } else {
                    ViewUtil.fade(progressBar, laundryStateLayout, getActivity());
                    ViewUtil.fadeOut(messageView, getActivity());
                    ViewUtil.fadeOut(messageButton, getActivity());
                    laundryStateLayout.setEnabled(true);
                }
            }
        });
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
