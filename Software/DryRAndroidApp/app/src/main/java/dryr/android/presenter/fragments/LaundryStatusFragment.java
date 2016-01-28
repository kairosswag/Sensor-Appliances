package dryr.android.presenter.fragments;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.androidplot.xy.BoundaryMode;
import com.androidplot.xy.LineAndPointFormatter;
import com.androidplot.xy.PointLabelFormatter;
import com.androidplot.xy.SimpleXYSeries;
import com.androidplot.xy.XYPlot;
import com.androidplot.xy.XYStepMode;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.util.ArrayList;
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
import dryr.common.json.beans.Dry;
import dryr.common.json.beans.HumiditySensorDataPoint;
import dryr.common.json.beans.HumidityTreshold;

/**
 * A simple {@link Fragment} subclass.
 * Activities that contain this fragment must implement the
 * {@link LaundryStatusFragment.OnFragmentInteractionListener} interface
 * to handle interaction events.
 */
public class LaundryStatusFragment extends Fragment implements RefreshListener, HumidityTable.HumidityDbListener {
    public static final String TAG = "laundryStatus";
    private OnFragmentInteractionListener mListener;

    // UI
    private ProgressBar smallProgress;
    private int progressCounter = 0;

    private RelativeLayout laundryStateLayout;
    private TextView laundryStateText;
    private TextView noGraphData;
    private XYPlot graph;
    private RelativeLayout predictionLayout;
    private TextView predictionTime;

    private MessageView messageView;

    // Data
    private BluetoothDevice sensor;
    private Dry laundryState;
    private SimpleXYSeries data;

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

            refresh(false);
            int period = getResources().getInteger(R.integer.sensor_fragment_status_refresh_frequency_period);

            refreshingScheduled = true;
            task = scheduledThreadPoolExecutor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    refresh(true);
                }
            }, period, period, TimeUnit.SECONDS);
        }

        mListener.registerForRefresh(this);
        // Update from db
        HumidityTable.getInstance(getActivity()).registerListener(this);
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
        HumidityTable.getInstance(getActivity()).unregisterListener(this);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_laundry_status, container, false);

        smallProgress = (ProgressBar) v.findViewById(R.id.laundry_status_progress);

        laundryStateLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_layout);
        laundryStateText = (TextView) v.findViewById(R.id.laundry_status_displayText);

        noGraphData = (TextView) v.findViewById(R.id.laundry_status_enough_graph_data);
        graph = (XYPlot) v.findViewById(R.id.laundry_status_graph);
        setupGraph();

        predictionLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_prediction_layout);
        predictionTime = (TextView) v.findViewById(R.id.laundry_status_prediction_time);

        messageView = (MessageView) v.findViewById(R.id.laundry_status_message_view);
        // TODO: maybe show more detailed messages about connection etc.

        if (laundryState != null) {
            setLaundryState(laundryState);
        }

        enableStateLayout(false);

        return v;
    }

    private void setupGraph() {
        // Connect with db
        HumidityTable humidityTable = HumidityTable.getInstance(getActivity());

        List<Long> xValues = new ArrayList<>();
        List<Float> yValues = new ArrayList<>();
        if (sensor != null) {
            List<HumiditySensorDataPoint> humiditySensorDataPoints = humidityTable.getDataPoints(sensor.getMac());
            for (HumiditySensorDataPoint dataPoint : humiditySensorDataPoints) {
                xValues.add(FormatUtil.getDateFromHDT(dataPoint, getActivity()).getTime() / 1000); // Use seconds
                yValues.add(dataPoint.getHumidity());
            }
        }

        setupDataSeries(xValues, yValues);

        // 3 steps on x axis 2 on y axis
        graph.setDomainStep(XYStepMode.SUBDIVIDE, 3);
        graph.setRangeStep(XYStepMode.INCREMENT_BY_VAL, 1);

        // Boundaries
        graph.setRangeBoundaries(25, 101, BoundaryMode.FIXED);

        // Colors that cannot be defined in xml
        graph.getGraphWidget().getDomainOriginLinePaint().setColor(getResources().getColor(R.color.graph_grid_color));
        graph.getGraphWidget().getDomainGridLinePaint().setColor(getResources().getColor(R.color.graph_grid_color));
        graph.getGraphWidget().getRangeOriginLinePaint().setColor(getResources().getColor(R.color.graph_grid_color));
        graph.getGraphWidget().getRangeGridLinePaint().setColor(Color.TRANSPARENT);

        // Show dates on x axis and show static labels on y axis
        setGraphThreshold(ConfigUtil.getDryThreshold(getActivity()));
        graph.setDomainValueFormat(new Format() {

            private DateFormat dateFormat = android.text.format.DateFormat.getTimeFormat(getActivity());

            @Override
            public StringBuffer format(Object object, StringBuffer buffer, FieldPosition field) {
                long time = ((Number) object).longValue() * 1000;
                Date date = new Date(time);
                return dateFormat.format(date, buffer, field);
            }

            @Override
            public Object parseObject(String string, ParsePosition position) {
                return null;
            }
        });

        if (data.size() <= 1) {
            showNotEnoughGraphData(true);
        }

        // TODO: add series for prediction
        // TODO: refresh prediction, hide if none available
        // style with ie series2Format.getLinePaint().setPathEffect( new DashPathEffect(new float[] { PixelUtils.dpToPix(20), PixelUtils.dpToPix(15)}, 0));
    }

    private void setGraphThreshold(final float threshold) {
        graph.setRangeValueFormat(new Format() {
            @Override
            public StringBuffer format(Object object, StringBuffer buffer, FieldPosition field) {
                Number n = (Number) object;
                int roundNum = (int) (n.floatValue() + 0.5f);
                if (roundNum == (int) threshold) {
                    buffer.append(getString(R.string.laundry_status_humidity_dry));
                } else if (roundNum == 100) {
                    buffer.append(getString(R.string.laundry_status_humidity_wet));
                } else {
                    buffer.append("");
                }
                return buffer;
            }

            @Override
            public Object parseObject(String string, ParsePosition position) {
                return null;
            }
        });
    }

    private void setupDataSeries(List<Long> xValues, List<Float> yValues) {
        data = new SimpleXYSeries(xValues, yValues, getString(R.string.laundry_status_humidity));
        final LineAndPointFormatter dataFormat = new LineAndPointFormatter();
        dataFormat.setPointLabelFormatter(new PointLabelFormatter());
        dataFormat.configure(getActivity(), R.xml.laundry_status_graph_data_format);
        graph.addSeries(data, dataFormat);
    }

    private void showNotEnoughGraphData(boolean show) {
        if (show) {
            if (noGraphData.getVisibility() != View.VISIBLE) {
                ViewUtil.fadeIn(noGraphData, getActivity());
                ViewUtil.fadeOut(graph, getActivity());
            }
        } else {
            if (noGraphData.getVisibility() == View.VISIBLE) {
                ViewUtil.fadeOut(noGraphData, getActivity());
                ViewUtil.fadeIn(graph, getActivity());
            }
        }
    }

    @Override
    public void dataPointAdded(HumiditySensorDataPoint dataPoint, String mac) {
        data.addLast(FormatUtil.getDateFromHDT(dataPoint, getActivity()).getTime() / 1000,
                dataPoint.getHumidity());

        if (data.size() <= 1) {
            showNotEnoughGraphData(true);
        } else {
            showNotEnoughGraphData(false);
        }
        graph.redraw();
    }

    @Override
    public void pointsDeleted(String mac) {
        graph.removeSeries(data);
        setupDataSeries(new ArrayList<Long>(), new ArrayList<Float>());
        showNotEnoughGraphData(true);
        graph.redraw();
    }

    private void enableStateLayout(boolean enable) {
        laundryStateLayout.setEnabled(enable);
        if (!enable) {
            ViewUtil.fadeOut(laundryStateLayout, getActivity(), 0.2f, View.VISIBLE);
        } else {
            ViewUtil.fadeIn(laundryStateLayout, getActivity());
        }
    }

    public void setLaundryState(Dry laundryState) {
        this.laundryState = laundryState;
        if (laundryStateText != null) {
            if (laundryState.getDry()) {
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
                    progressCounter++;
                } else {
                    progressCounter--;
                }

                if (progressCounter > 0) {
                    if (smallProgress.getVisibility() != View.VISIBLE) {
                        ViewUtil.fadeIn(smallProgress, getActivity());
                    }
                } else {
                    if (smallProgress.getVisibility() == View.VISIBLE) {
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
            }
        });
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
    public void refresh(boolean silent) {
        if (progressCounter == 0) {
            refreshDryState(silent);
            refreshPrediction(silent);
            refreshHumidityGraph(silent);
            refreshDryInformation(silent);
        }
    }

    private void refreshDryState(final boolean silent) {
        if (sensor == null) {
            return;
        }

        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).isDry(sensor.getMac(), new CommunicationFacade.CommunicationCallback<Dry>() {
            @Override
            public void onResult(Dry result) {

                enableStateLayout(true);
                messageView.showMessage(R.string.laundry_status_base_station_connected, R.color.light_success_color, 0, false, null);
                setLaundryState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                showError(silent, error);
                showProgress(false);
                enableStateLayout(false);
            }

            @Override
            public Object getTag() {
                return LaundryStatusFragment.this;
            }
        });
    }

    private void refreshPrediction(boolean silent) {
        // TODO
    }

    private void refreshDryInformation(final boolean silent) {
        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).getDryInformation(new CommunicationFacade.CommunicationCallback<HumidityTreshold>() {
            @Override
            public void onResult(HumidityTreshold result) {
                setGraphThreshold(result.getTreshold());
                graph.redraw();
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                showError(silent, error);
                showProgress(false);
            }

            @Override
            public Object getTag() {
                return LaundryStatusFragment.this;
            }
        });
    }

    private void refreshHumidityGraph(final boolean silent) {
        showProgress(true);
        CommunicationFacade.getInstance(getActivity()).getHumidity(sensor.getMac(), new CommunicationFacade.CommunicationCallback<HumiditySensorDataPoint>() {
            @Override
            public void onResult(HumiditySensorDataPoint result) {
                HumidityTable.getInstance(getActivity()).addDataPoint(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                showError(silent, error);
                showProgress(false);
                enableStateLayout(false);
            }

            @Override
            public Object getTag() {
                return LaundryStatusFragment.this;
            }
        });
    }

    private void showError(boolean silent, CommunicationFacade.CommunicationError error) {
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
