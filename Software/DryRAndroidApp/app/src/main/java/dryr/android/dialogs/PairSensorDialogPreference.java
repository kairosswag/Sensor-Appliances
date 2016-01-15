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
                ViewUtil.fadeOut(pairedLayout, getContext());
                searchForSensors();
            }
        });


        addLayout = (LinearLayout) view.findViewById(R.id.dialog_pair_sensor_add_layout);
        errorText = (TextView) view.findViewById(R.id.dialog_pair_sensor_error_text);
        retry = (Button) view.findViewById(R.id.dialog_pair_sensor_retry);
        refresh = (Button) view.findViewById(R.id.dialog_pair_sensor_add_refresh);
        refresh.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ViewUtil.fadeOut(addLayout, getContext());
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
        ViewUtil.fadeOut(pairedLayout, getContext());
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
                nonePaired.setVisibility(View.VISIBLE);
                pairedRecycler.setVisibility(View.GONE);

                pairNew.setEnabled(true); // Only allow one Sensor for now
                removeSensor.setEnabled(false);
            }
        }
    }

    private void getPairedSensors() {
        hideError();

        ViewUtil.fadeIn(progressBar, getContext());
        CommunicationFacade.getInstance(getContext()).getPairedSensors(new CommunicationFacade.CommunicationCallback<List<BluetoothDevice>>() {
            @Override
            public void onResult(final List<BluetoothDevice> result) {

                paired = result;
                if (result.isEmpty()) {
                    // No paired sensors
                    nonePaired.setVisibility(View.VISIBLE);
                    pairedRecycler.setVisibility(View.GONE);

                    pairNew.setEnabled(true); // Only allow one Sensor for now
                    removeSensor.setEnabled(false);
                } else {
                    // Display paired sensors
                    nonePaired.setVisibility(View.GONE);
                    pairedRecycler.setVisibility(View.VISIBLE);
                    pairedAdapter = new SensorAdapter(result, new SensorAdapter.SensorAdapterListener() {
                        @Override
                        public void onSensorSelected(int pos) {
                            pairedSensorSelected(pos);
                        }
                    });
                    pairedRecycler.setAdapter(pairedAdapter);

                    removeSensor.setEnabled(true);
                    pairNew.setEnabled(false); // Only allow one Sensor for now
                }
                ViewUtil.fade(progressBar, pairedLayout, getContext());

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
                ViewUtil.fade(progressBar, pairedLayout, getContext());

            }
        });
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
                                    noneAvailable.setVisibility(View.VISIBLE);
                                    availableRecycler.setVisibility(View.GONE);
                                } else {
                                    // Display available sensors in a recycler react to onCLick
                                    noneAvailable.setVisibility(View.GONE);
                                    availableRecycler.setVisibility(View.VISIBLE);

                                    SensorAdapter adapter = new SensorAdapter(result, new SensorAdapter.SensorAdapterListener() {
                                        @Override
                                        public void onSensorSelected(int pos) {
                                            addSensor(pos);
                                        }
                                    });
                                    availableRecycler.setAdapter(adapter);
                                }

                                ViewUtil.fade(progressBar, addLayout, getContext());
                            }


                            @Override
                            public void onError(final CommunicationFacade.CommunicationError error) {

                                switch (error) {
                                    default:
                                        // Display error
                                        ViewUtil.fade(progressBar, addLayout, getContext());
                                        errorText.setText(R.string.error_connection_default);
                                        errorText.setVisibility(View.VISIBLE);
                                }

                                // Give option to retry
                                retry.setVisibility(View.VISIBLE);
                                retry.setOnClickListener(new View.OnClickListener() {
                                    @Override
                                    public void onClick(View v) {
                                        searchForSensors();
                                    }
                                });

                                ViewUtil.fade(progressBar, addLayout, getContext());
                            }


                        }

                );
    }

    private void addSensor(final int pos) {
        // Add sensor switch to connectionQLayout
        added.add(available.get(pos));

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

    private void filterAvailable() {
        // Don't display already paired (or to be paired) sensors as available
        // Not efficient but there is no use case with thousands of sensors in these lists
        for (BluetoothDevice p : paired) {
            for (int i = 0; i < available.size(); i++) {
                if (p.getMac().equals(available.get(i).getMac())) {
                    available.remove(i);
                }
            }
        }

        for (BluetoothDevice p : added) {
            for (int i = 0; i < available.size(); i++) {
                if (p.getMac().equals(available.get(i).getMac())) {
                    available.remove(i);
                }
            }
        }
    }

    private void hideError() {
        ViewUtil.fadeOut(errorText, getContext());
        ViewUtil.fadeOut(retry, getContext());
    }

    private void applyChanges() {
        ViewUtil.fade(mainContentLayout, progressBar, getContext());
        hideError();

        pairedLayout.setVisibility(View.GONE);
        addLayout.setVisibility(View.GONE);
        connectionQLayout.setVisibility(View.GONE);

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
                } else  {
                    errorText.setText(R.string.error_connection_default);
                    errorText.setVisibility(View.VISIBLE);
                    retry.setVisibility(View.VISIBLE);
                    retry.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) {
                            applyChanges();
                        }
                    });

                    mainContentLayout.setVisibility(View.VISIBLE);
                    ViewUtil.fadeIn(mainContentLayout, getContext());
                    ViewUtil.fade(progressBar, connectionQLayout, getContext());
                }
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    default:
                        errorText.setText(R.string.error_connection_default);
                        errorText.setVisibility(View.VISIBLE);
                        retry.setVisibility(View.VISIBLE);
                        retry.setOnClickListener(new View.OnClickListener() {
                            @Override
                            public void onClick(View v) {
                                applyChanges();
                            }
                        });
                }
                mainContentLayout.setVisibility(View.VISIBLE);
                ViewUtil.fadeIn(mainContentLayout, getContext());
                ViewUtil.fade(progressBar, connectionQLayout, getContext());
                // TODO: somehow a second error is not displayed correctly (future sprint)
            }
        });
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
    }
}
