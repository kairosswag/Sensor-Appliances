package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.helper.DateAsXAxisLabelFormatter;
import com.jjoe64.graphview.helper.StaticLabelsFormatter;
import com.jjoe64.graphview.series.DataPoint;
import com.jjoe64.graphview.series.LineGraphSeries;

import java.util.Date;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.db.HumidityTable;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.presenter.listener.RefreshListener;
import dryr.android.utils.ConfigUtil;
import dryr.android.utils.FormatUtil;
import dryr.android.utils.ViewUtil;
import dryr.android.views.MessageView;
import dryr.common.json.beans.BluetoothDevice;
import dryr.common.json.beans.HumiditySensorDataPoint;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link LaundryStatusFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.
 */
public class LaundryStatusFragment extends Fragment implements RefreshListener {
    public static final String TAG = "laundryStatus";
    private OnFragmentInteractionListener mListener;

    // UI
    private ProgressBar smallProgress;

    private RelativeLayout laundryStateLayout;
    private TextView laundryStateText;
    private GraphView graphView;
    private RelativeLayout predictionLayout;
    private TextView predictionTime;

    private MessageView messageView;

    // Data
    private BluetoothDevice sensor;
    private HumiditySensorDataPoint laundryState;
    private LineGraphSeries<DataPoint>  series;
    LineGraphSeries<DataPoint> predictionSeries;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;
    private boolean refreshing = false;

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

            refresh(false);
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
        View v = inflater.inflate(R.layout.fragment_laundry_status, container, false);

        smallProgress = (ProgressBar) v.findViewById(R.id.laundry_status_progress);

        laundryStateLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_layout);
        laundryStateText = (TextView) v.findViewById(R.id.laundry_status_displayText);

        // graphView = (GraphView) v.findViewById(R.id.laundry_status_graph);
        // setupGraph(); TODO: Working graph

        predictionLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_prediction_layout);
        predictionTime = (TextView) v.findViewById(R.id.laundry_status_prediction_time);

        messageView = (MessageView) v.findViewById(R.id.laundry_status_message_view); // TODO: Show more messages about connection etc. in a future Sprint

        if (laundryState != null) {
            setLaundryState(laundryState);
        }

        enableStateLayout(false);

        return v;
    }

    private void setupGraph() {
        // TODO: fix graph
        // Set up graph
        graphView.getViewport().setScrollable(false);
        graphView.getViewport().setMaxY(100);
        graphView.getViewport().setMinY(ConfigUtil.getDryThreshold(getActivity()));
        // Set colors
        graphView.getGridLabelRenderer().setHorizontalAxisTitleColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setVerticalAxisTitleColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setHorizontalLabelsColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setVerticalLabelsColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setGridColor(getResources().getColor(R.color.graph_grid_color));
        graphView.getGridLabelRenderer().setNumHorizontalLabels(3);

        // Show dates on x axis and show static labels on y axis
        StaticLabelsFormatter labelsFormatter = new StaticLabelsFormatter(graphView, new DateAsXAxisLabelFormatter(getActivity())); // TODO: Show exact time?
        labelsFormatter.setVerticalLabels(new String[] {getString(R.string.laundry_status_humidity_dry), getString(R.string.laundry_status_humidity_wet)});
        graphView.getGridLabelRenderer().setLabelFormatter(labelsFormatter);

        // Add series
        series = new LineGraphSeries<>();
        series.setColor(getResources().getColor(R.color.graph_humidity_color));


        // Connect with db
        HumidityTable humidityTable = HumidityTable.getInstance(getActivity());

        graphView.getViewport().setXAxisBoundsManual(true);

        if (sensor != null) {
            List<HumiditySensorDataPoint> humiditySensorDataPoints = humidityTable.getDataPoints(sensor.getMac());
            if (humiditySensorDataPoints.size() != 0) {
                DataPoint[] dataPoints = new DataPoint[humiditySensorDataPoints.size()];
                int i = 0;
                for (HumiditySensorDataPoint dataPoint : humiditySensorDataPoints) {
                    dataPoints[i] = new DataPoint(FormatUtil.getDateFromHDT(dataPoint, getActivity()), dataPoint.getHumidity());
                    i++;
                }
                series = new LineGraphSeries<>(dataPoints);

                graphView.getViewport().setMinX(dataPoints[0].getX());
                graphView.getViewport().setMinX(dataPoints[dataPoints.length - 1].getX());
            } else {
                series = new LineGraphSeries<>();

                graphView.getViewport().setMinX(new Date().getTime());
                graphView.getViewport().setMaxX(new Date().getTime() + 1000000);
            }
        }
        graphView.addSeries(series);

        // Update from db
        humidityTable.setListener(new HumidityTable.HumidityDbListener() {
            @Override
            public void dataPointAdded(HumiditySensorDataPoint dataPoint, String mac) {
                if (mac.equals(sensor.getMac())) {
                    DataPoint dp = new DataPoint(FormatUtil.getDateFromHDT(dataPoint, getActivity()), dataPoint.getHumidity());
                    series.appendData(dp, false, 200);
                    graphView.getViewport().setMaxX(dp.getX()); // TODO: set this to prediction or whatever is larger
                }
            }

            @Override
            public void pointsDeleted(String mac) {
                if (mac.equals(sensor.getMac())) {
                    series.resetData(new DataPoint[0]);
                }
            }
        });

        // TODO: do this in setLaundryState / setPrediction
        // TODO: maybe save last prediction for smoothness
        predictionSeries = new LineGraphSeries<>(new DataPoint[]{
                new DataPoint(4 /* TODO: laundryState.getDate */, /* TODO: laundryState.getHumidity()*/ 60),
                new DataPoint(7, 50)
        });
        predictionSeries.setColor(getResources().getColor(R.color.graph_predicted_humidity_color));
        graphView.addSeries(predictionSeries);

        // TODO: refresh prediction, hide if none available
    }

    @Override
    public void refresh() {
        refresh(false);
    }

    /**
     * Refresh laundry state
     *
     * @param silent don't display error messages
     */
    public void refresh(final boolean silent) {
        if (sensor == null) {
            return;
        }

        refreshing = true;
        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).getLaundryState(sensor.getMac(), new CommunicationFacade.CommunicationCallback<HumiditySensorDataPoint>() {
            @Override
            public void onResult(HumiditySensorDataPoint result) {
                refreshing = false;

                enableStateLayout(true);
                messageView.showMessage(R.string.laundry_status_base_station_connected, R.color.light_success_color, 0, false, null);
                setLaundryState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                refreshing = false;

                switch (error) {
                    case NO_BASE_STATION_FOUND:

                        // Base station was not found in network
                        messageView.showMessage(R.string.error_no_base_station_found, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                refresh(false);
                            }
                        });

                        break;

                    case NO_SENSOR_PAIRED:

                        // Show message informing about the fact that there are no sensors paired and
                        // display button to pair some
                        messageView.showMessage(R.string.laundry_status_no_sensor, R.color.light_error_text_color, R.string.pref_sensor_pair_title, true, new View.OnClickListener() {
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

    public void setSensor(BluetoothDevice sensor) {
        this.sensor = sensor;
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
        public void registerForRefresh(RefreshListener listener);
        public void unregisterForRefresh(RefreshListener listener);
    }
}
