package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.helper.DateAsXAxisLabelFormatter;
import com.jjoe64.graphview.helper.StaticLabelsFormatter;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;

import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.db.HumidityTable;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.utils.ConfigUtil;
import dryr.android.utils.FormatUtil;
import dryr.android.utils.ViewUtil;
import dryr.common.json.beans.HumiditySensorDataPoint;

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
    private ProgressBar smallProgress;

    private RelativeLayout laundryStateLayout;
    private TextView laundryStateText;
    private GraphView graphView;
    private LinearLayout predictionLayout;
    private TextView predictionTime;

    private TextView messageView;
    private Button messageButton;

    // Data
    private HumiditySensorDataPoint laundryState;
    private LineGraphSeries<DataPoint>  series;
    LineGraphSeries<DataPoint> predictionSeries;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;
    private boolean refreshing = false;

    // TODO: use tabs for multiple sensors

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
                    if (!refreshing) refreshState(true);
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

        // cancel already sent requests
        CommunicationFacade.getInstance(getActivity()).cancelAllByTag(this);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_laundry_status, container, false);

        smallProgress = (ProgressBar) v.findViewById(R.id.laundry_status_progress);

        laundryStateLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_layout);
        laundryStateText = (TextView) v.findViewById(R.id.laundry_status_displayText);

        // Set up graph
        graphView = (GraphView) v.findViewById(R.id.laundry_status_graph);
        graphView.getViewport().setScrollable(false);
        graphView.getViewport().setMaxY(100);
        graphView.getViewport().setMinY(ConfigUtil.getDryThreshold(getActivity()));
        // Set colors
        graphView.getGridLabelRenderer().setHorizontalAxisTitleColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setVerticalAxisTitleColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setHorizontalLabelsColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setVerticalLabelsColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setGridColor(getResources().getColor(R.color.graph_grid_color));

        // Show dates on x axis and show static labels on y axis
        StaticLabelsFormatter labelsFormatter = new StaticLabelsFormatter(graphView, new DateAsXAxisLabelFormatter(getActivity())); // TODO: Show exact time?
        labelsFormatter.setVerticalLabels(new String[] {getString(R.string.laundry_status_humidity_dry), getString(R.string.laundry_status_humidity_wet)});
        graphView.getGridLabelRenderer().setLabelFormatter(labelsFormatter);

        // Load data points from database
        List<HumiditySensorDataPoint> humiditySensorDataPoints = HumidityTable.getInstance(getActivity()).getDataPoints("*" ); // TODO
        DataPoint[] dataPoints = new DataPoint[humiditySensorDataPoints.size()];
        int i = 0;
        for (HumiditySensorDataPoint dataPoint : humiditySensorDataPoints) {
            dataPoints[i] = new DataPoint(FormatUtil.getDateFromHDT(dataPoint, getActivity()).getTime(), dataPoint.getHumidity());
        }
        series = new LineGraphSeries<>(dataPoints);
        series.setColor(getResources().getColor(R.color.graph_humidity_color));
        graphView.addSeries(series);

        // TODO: do this in setLaundryState / setPrediction
        // TODO: maybe save last prediction for smoothness
        predictionSeries = new LineGraphSeries<>(new DataPoint[]{
                new DataPoint(4 /* TODO: laundryState.getDate */, /* TODO: laundryState.getHumidity()*/ 60),
                new DataPoint(7, 50)
        });
        predictionSeries.setColor(getResources().getColor(R.color.graph_predicted_humidity_color));
        graphView.addSeries(predictionSeries);

        // TODO: refresh prediction, hide if none available

        predictionLayout = (LinearLayout) v.findViewById(R.id.laundry_status_prediction_layout);
        predictionTime = (TextView) v.findViewById(R.id.laundry_status_prediction_time);

        messageView = (TextView) v.findViewById(R.id.laundry_status_message_view);
        messageView.setVisibility(View.GONE); // TODO: Show more messages about connection etc. in a future Sprint
        messageButton = (Button) v.findViewById(R.id.laundry_status_message_button);

        setHasOptionsMenu(true);


        if (laundryState != null) {
            setLaundryState(laundryState);
        }

        enableStateLayout(false);

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
     *
     * @param silent don't display error messages
     */
    private void refreshState(final boolean silent) {
        refreshing = true;
        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).getLaundryState(new CommunicationFacade.CommunicationCallback<HumiditySensorDataPoint>() {
            @Override
            public void onResult(HumiditySensorDataPoint result) {
                refreshing = false;

                enableStateLayout(true);
                showMessage(R.string.laundry_status_base_station_connected, R.color.light_success_color, 0, false, null);
                HumidityTable.getInstance(getActivity()).addDataPoint(result);
                setLaundryState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                refreshing = false;

                switch (error) {
                    case NO_BASE_STATION_FOUND:

                        // Base station was not found in network
                        showMessage(R.string.error_no_base_station_found, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refreshState(false);
                            }
                        });

                        break;

                    case NO_SENSOR_PAIRED:

                        // Show message informing about the fact that there are no sensors paired and
                        // display button to pair some
                        showMessage(R.string.laundry_status_no_sensor, R.color.light_error_text_color, R.string.pref_sensor_pair_title, true, new View.OnClickListener() {
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
                        showMessage(R.string.error_connection_default, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refreshState(false);
                            }
                        });
                }

                showProgress(false);
                enableStateLayout(false);
            }

            @Override
            public Object getTag() {
                return LaundryStatusFragment.this;
            }
        });
    }

    private void enableStateLayout(boolean enable) {
        laundryStateLayout.setEnabled(enable);
        if (!enable) {
            ViewUtil.fadeOut(laundryStateLayout, getActivity(), 0.2f, View.VISIBLE);
        } else {
            ViewUtil.fadeIn(laundryStateLayout, getActivity());
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

    public void setLaundryState(HumiditySensorDataPoint laundryState) {
        this.laundryState = laundryState;
        if (laundryStateText != null) {

            TypedValue outValue = new TypedValue();
            getResources().getValue(R.dimen.sensor_humidity_dry_threshold, outValue, true);
            float threshold = outValue.getFloat();

            if (laundryState.getHumidity() <= threshold) {
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
                    ViewUtil.fadeIn(smallProgress, getActivity());
                } else {
                    // Add delay so the animation doesn't look weird
                    new ScheduledThreadPoolExecutor(1).schedule(new Runnable() {
                        @Override
                        public void run() {
                            getActivity().runOnUiThread(new Runnable() {
                                @Override
                                public void run() {
                                    ViewUtil.fadeOut(smallProgress, getActivity(), View.INVISIBLE);
                                }
                            });
                        }
                    }, getResources().getInteger(R.integer.progress_delay), TimeUnit.MILLISECONDS);
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
