package dryr.android.dialogs;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.preference.DialogPreference;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.utils.FormatUtil;
import dryr.android.utils.ViewUtil;
import dryr.common.json.beans.BluetoothDevice;

// TODO: make dialogs stay one size (user friendliness)

/**
 * Dialog to pair a sensor to the BaseStation
 */
public class PairSensorDialogPreference extends DialogPreference {

    // Dialog ui
    private LinearLayout mainContentLayout;

    private LinearLayout pairedLayout;
    private TextView nonePaired;
    private RecyclerView pairedRecycler;
    private SensorAdapter pairedAdapter;
    private Button removeSensor;
    private Button pairNew;

    private LinearLayout addLayout;
    private TextView errorText;
    private Button retry;
    private Button refresh;
    private TextView noneAvailable;
    private RecyclerView availableRecycler;
    private SensorAdapter availableAdapter;

    private LinearLayout connectionQLayout;
    private TextView connectWithTitle;
    private LinearLayout connectWithLayout;
    private TextView removeTitle;
    private LinearLayout removeLayout;
    private Button addAnother;

    private ProgressBar progressBar;

    // Data
    private boolean pairedDeleteMode = false;
    private List<BluetoothDevice> paired;
    private List<BluetoothDevice> available;
    private List<BluetoothDevice> delete = new ArrayList<>();
    private List<BluetoothDevice> added = new ArrayList<>();

    public PairSensorDialogPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
        setPersistent(false);
        setDialogLayoutResource(R.layout.dialog_pair_sensor);
    }

    @Override
    protected View onCreateView(ViewGroup parent) {
        final View v = super.onCreateView(parent);

        return v;
    }

    @Override
    protected void showDialog(Bundle state) {
        super.showDialog(state);
        // Override OnCLickListener of positive button to do network action before dialog closes
        Button pos = ((AlertDialog) getDialog()).getButton(DialogInterface.BUTTON_POSITIVE);
        pos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                applyChanges();
            }
        });
    }

    @Override
    protected void onBindDialogView(View view) {
        super.onBindDialogView(view);
        // Reset data
        paired = null;
        available = null;
        delete = new ArrayList<>();
        added = new ArrayList<>();
        availableAdapter = null;
        pairedAdapter = null;

        // Set up dialog ui
        mainContentLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_main_content_layout);

        pairedLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_paired_sensors_layout);
        nonePaired = (TextView) view.findViewById(R.id.dialog_pair_sensor_paired_sensors_none);
        pairedRecycler = (RecyclerView) view.findViewById(R.id.dialog_pair_sensor_paired_sensors_recycler);
        pairedRecycler.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL, false));
        removeSensor = (Button) view.findViewById(R.id.dialog_pair_sensor_paired_sensors_delete);
        removeSensor.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                togglePairedDeleteMode();
            }
        });
        pairNew = (Button) view.findViewById(R.id.dialog_pair_sensor_paired_sensors_pair_new);
        pairNew.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                switchToAdd(pairedLayout);
            }
        });


        addLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_add_layout);
        errorText = (TextView) view.findViewById(R.id.dialog_pair_sensor_error_text);
        retry = (Button) view.findViewById(R.id.dialog_pair_sensor_retry);
        refresh = (Button) view.findViewById(R.id.dialog_pair_sensor_add_refresh);
        refresh.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                searchForSensors();
            }
        });
        noneAvailable = (TextView) view.findViewById(R.id.dialog_pair_sensor_add_available_none);
        availableRecycler = (RecyclerView) view.findViewById(R.id.dialog_pair_sensor_available_recycler);
        availableRecycler.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL, false));


        connectionQLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_connection_q_layout);
        connectWithTitle = (TextView) view.findViewById(R.id.dialog_pair_sensor_connection_q_connect_with_title);
        connectWithLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_connection_q_add_ids);
        removeTitle = (TextView) view.findViewById(R.id.dialog_pair_sensor_connection_q_remove_title);
        removeLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_connection_q_remove_ids);
        addAnother = (Button) view.findViewById(R.id.dialog_pair_sensor_connection_q_add_another);
        addAnother.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                filterAvailable();
                // TODO: handle onCLick, refresh list (don't display added sensors, display removed ones?)
            }
        });
        addAnother.setVisibility(View.GONE); // Only allow one sensor for now


        progressBar = (ProgressBar) view.findViewById(R.id.dialog_pair_sensor_progress);

        // Show paired sensors
        getPairedSensors();
    }

    private void togglePairedDeleteMode() {
        pairedDeleteMode = !pairedDeleteMode;
        if (pairedDeleteMode) {
            pairedRecycler.setBackgroundColor(getContext().getResources().getColor(R.color.delete_mode_background));
            removeSensor.setText(R.string.pref_sensors_disconnect_cancel);
        } else {
            pairedRecycler.setBackgroundColor(getContext().getResources().getColor(android.R.color.transparent));
            removeSensor.setText(R.string.pref_sensors_disconnect);
        }
    }

    private void pairedSensorSelected(int pos) {
        if (pos < 0 || pos >= paired.size()) {
            return;
        }

        if (pairedDeleteMode) {
            delete.add(paired.get(pos));
            paired.remove(pos);
            pairedAdapter.notifyItemRemoved(pos);

            if (paired.isEmpty()) {
                togglePairedDeleteMode();

                // No paired sensors
                ViewUtil.fadeIn(nonePaired, getContext());
                showRecycler(pairedRecycler, false);

                pairNew.setEnabled(true); // Only allow one Sensor for now
                removeSensor.setEnabled(false);
            }
        }
    }

    private void getPairedSensors() {
        pairNew.setEnabled(false);
        removeSensor.setEnabled(false);
        hideError();

        ViewUtil.fadeIn(progressBar, getContext());
        CommunicationFacade.getInstance(getContext()).getPairedSensors(new CommunicationFacade.CommunicationCallback<List<BluetoothDevice>>() {
            @Override
            public void onResult(final List<BluetoothDevice> result) {

                paired = result;
                if (result.isEmpty()) {
                    // No paired sensors
                    ViewUtil.fadeIn(nonePaired, getContext());
                    showRecycler(pairedRecycler, false);

                    pairNew.setEnabled(true); // Only allow one Sensor for now
                    removeSensor.setEnabled(false);
                } else {
                    // Display paired sensors
                    ViewUtil.fadeOut(nonePaired, getContext());
                    if (pairedAdapter == null) {
                        pairedAdapter = new SensorAdapter(result, new SensorAdapter.SensorAdapterListener() {
                            @Override
                            public void onSensorSelected(int pos) {
                                pairedSensorSelected(pos);
                            }
                        });
                        pairedRecycler.setAdapter(pairedAdapter);
                    } else {
                        pairedAdapter.setSensors(result);
                        pairedAdapter.notifyDataSetChanged();
                    }

                    removeSensor.setEnabled(true);
                    pairNew.setEnabled(false); // Only allow one Sensor for now
                    showRecycler(pairedRecycler, true);
                }
                ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
            }

            @Override
            public void onError(final CommunicationFacade.CommunicationError error) {

                switch (error) {
                    default:
                        errorText.setText(R.string.error_connection_default);
                        retry.setVisibility(View.VISIBLE);
                        retry.setOnClickListener(new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                getPairedSensors();
                            }
                        });
                        errorText.setVisibility(View.VISIBLE);
                }

                pairNew.setEnabled(false);
                removeSensor.setEnabled(false);
                ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
            }

            @Override
            public Object getTag() {
                return PairSensorDialogPreference.this;
            }
        });
    }

    private void showRecycler(RecyclerView recyclerView, boolean show) {
        // expand or collapse recycler
        if (show) {
            if (recyclerView.getVisibility() == View.GONE) {
                recyclerView.setVisibility(View.VISIBLE);
                ViewUtil.animateHeight(recyclerView, 1, (int) ViewUtil.dipToPx(100, getContext()), View.VISIBLE);
            }
        } else {
            if (recyclerView.getVisibility() == View.VISIBLE) {
                ViewUtil.animateHeight(recyclerView, (int) ViewUtil.dipToPx(100, getContext()), 1, View.GONE);
            }
        }
    }

    private void switchToAdd(View from) {
        ViewUtil.fade(from, addLayout, getContext());
        searchForSensors();
    }

    private void searchForSensors() {
        hideError();

        ViewUtil.fadeIn(progressBar, getContext());

        CommunicationFacade.getInstance(getContext()).
                getAvailableSensors(
                        new CommunicationFacade.CommunicationCallback<List<BluetoothDevice>>() {
                            @Override
                            public void onResult(final List<BluetoothDevice> result) {

                                available = result;
                                result.addAll(delete);
                                filterAvailable();

                                if (result.isEmpty()) {
                                    // No available sensors
                                    ViewUtil.fadeIn(noneAvailable, getContext());
                                    showRecycler(availableRecycler, false);
                                } else {
                                    // Display available sensors in a recycler react to onCLick
                                    noneAvailable.setVisibility(View.GONE);

                                    if (availableAdapter == null) {
                                        availableAdapter = new SensorAdapter(result, new SensorAdapter.SensorAdapterListener() {
                                            @Override
                                            public void onSensorSelected(int pos) {
                                                addSensor(pos);
                                            }
                                        });
                                        availableRecycler.setAdapter(availableAdapter);
                                    } else {
                                        availableAdapter.setSensors(result);
                                        availableAdapter.notifyDataSetChanged();
                                    }

                                    showRecycler(availableRecycler, true);
                                }

                                ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
                            }


                            @Override
                            public void onError(final CommunicationFacade.CommunicationError error) {

                                switch (error) {
                                    default:
                                        // Display error
                                        errorText.setText(R.string.error_connection_default);
                                        ViewUtil.fadeIn(errorText, getContext());
                                }

                                // Give option to retry
                                ViewUtil.fadeIn(retry, getContext());
                                retry.setOnClickListener(new View.OnClickListener() {
                                    @Override
                                    public void onClick(View v) {
                                        searchForSensors();
                                    }
                                });

                                ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
                                ViewUtil.fadeIn(addLayout, getContext());
                            }

                            @Override
                            public Object getTag() {
                                return PairSensorDialogPreference.this;
                            }
                        }

                );
    }

    private void applyChanges() {
        ViewUtil.fadeIn(progressBar, getContext());
        hideError();

        // Remove sensors selected to be removed and pair sensors to be paired
        CommunicationFacade.getInstance(getContext()).pairAndRemove(added, delete, new CommunicationFacade.CommunicationCallback<ConcurrentHashMap<String, Boolean>>() {
            @Override
            public void onResult(ConcurrentHashMap<String, Boolean> result) {
                boolean success = true;
                for (Map.Entry<String, Boolean> e : result.entrySet()) {
                    if (e.getValue()) {
                        removeFromAddedOrDelete(e.getKey());
                    } else {
                        success = false;
                    }
                }
                if (success) {
                    getDialog().dismiss();
                } else {
                    errorText.setText(R.string.error_connection_default);
                    ViewUtil.fadeIn(errorText, getContext());
                    retry.setVisibility(View.VISIBLE);
                    ViewUtil.fadeIn(retry, getContext());
                    retry.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) {
                            applyChanges();
                        }
                    });

                    ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
                }
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    default:
                        errorText.setText(R.string.error_connection_default);
                        ViewUtil.fadeIn(errorText, getContext());
                        retry.setVisibility(View.VISIBLE);
                        ViewUtil.fadeIn(retry, getContext());
                        retry.setOnClickListener(new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                applyChanges();
                            }
                        });
                }
                ViewUtil.fadeOut(progressBar, getContext(), View.INVISIBLE);
                // TODO: somehow a second error is not displayed correctly (future sprint)
            }

            @Override
            public Object getTag() {
                return PairSensorDialogPreference.this;
            }
        });
    }

    private void hideError() {
        if (errorText.getVisibility() == View.VISIBLE) {
            ViewUtil.fadeOut(errorText, getContext());
            ViewUtil.fadeOut(retry, getContext());
        }
    }

    private void addSensor(final int pos) {
        // Add sensor switch to connectionQLayout
        added.add(available.get(pos));
        filterDelete(available.get(pos));

        if (added.isEmpty()) {
            connectWithTitle.setVisibility(View.GONE);
        } else {
            connectWithTitle.setVisibility(View.VISIBLE);
        }
        addLayout.removeAllViews();
        for (BluetoothDevice a : added) {
            TextView t = new TextView(getContext());
            t.setText(a.getMac());
            connectWithLayout.addView(t);
        }

        if (delete.isEmpty()) {
            removeTitle.setVisibility(View.GONE);
        } else {
            removeTitle.setVisibility(View.VISIBLE);
        }
        removeLayout.removeAllViews();
        for (BluetoothDevice d : delete) {
            TextView t = new TextView(getContext());
            t.setText(d.getMac());
            removeLayout.addView(t);
        }
        ViewUtil.fade(addLayout, connectionQLayout, getContext());
    }

    private void filterDelete(BluetoothDevice a) {
        for (Iterator<BluetoothDevice> iterator = delete.iterator(); iterator.hasNext(); ) {
            if (a.equals(iterator.next().getMac())) {
                iterator.remove();
                return;
            }
        }
    }

    private void filterAvailable() {
        // Don't display already paired (or to be paired) sensors as available
        // Not efficient but there is no use case with thousands of sensors in these lists
        for (BluetoothDevice p : paired) {
            for (Iterator<BluetoothDevice> iterator = available.iterator(); iterator.hasNext(); ) {
                if (p.getMac().equals(iterator.next().getMac())) {
                    iterator.remove();
                }
            }
        }

        for (BluetoothDevice p : added) {
            for (Iterator<BluetoothDevice> iterator = available.iterator(); iterator.hasNext(); ) {
                if (p.getMac().equals(iterator.next().getMac())) {
                    iterator.remove();
                }
            }
        }
    }

    private void removeFromAddedOrDelete(String mac) {
        for (Iterator<BluetoothDevice> iterator = added.iterator(); iterator.hasNext(); ) {
            BluetoothDevice device = iterator.next();
            if (device.getMac().equals(mac)) {
                iterator.remove();
                return;
            }
        }

        for (Iterator<BluetoothDevice> iterator = delete.iterator(); iterator.hasNext(); ) {
            BluetoothDevice device = iterator.next();
            if (device.getMac().equals(mac)) {
                iterator.remove();
                return;
            }
        }
    }

    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {
        }
        // cancel already sent requests
        CommunicationFacade.getInstance(getContext()).cancelAllByTag(this);
    }
}
