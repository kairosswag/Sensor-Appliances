package dryr.android.dialogs;

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.DialogPreference;
import android.preference.PreferenceManager;
import android.app.AlertDialog;
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
import java.util.List;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.model.Sensor;
import dryr.android.utils.ViewUtil;

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
    private List<Sensor> paired;
    private List<Sensor> available;
    private List<Sensor> delete = new ArrayList<>();
    private List<Sensor> added = new ArrayList<>();

    public PairSensorDialogPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
        setPersistent(false);
        setDialogLayoutResource(R.layout.dialog_pair_sensor);
    }

    @Override
    protected View onCreateView(ViewGroup parent) {
        final View v = super.onCreateView(parent);
        // Get notified over sp changes
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getContext());
        sp.registerOnSharedPreferenceChangeListener(new SharedPreferences.OnSharedPreferenceChangeListener() {
            @Override
            public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
                refreshEnabledState(sharedPreferences, v);
            }
        });
        refreshEnabledState(sp, v);

        return v;
    }

    private void refreshEnabledState(SharedPreferences sp, View v) {
        // Find out if a BaseStation is connected
        if (sp.contains(getContext().getString(R.string.pref_baseStation_connect_key))) {
            v.setEnabled(true);
        } else {
            v.setEnabled(false);
        }

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
        CommunicationFacade.getInstance().getPairedSensors(new CommunicationFacade.CommunicationCallback<List<Sensor>>() {
            @Override
            public void onResult(final List<Sensor> result) {
                ((Activity) getContext()).runOnUiThread(new Runnable() {
                    @Override
                    public void run() {

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
                });
            }

            @Override
            public void onError(final CommunicationFacade.CommunicationError error) {
                ((Activity) getContext()).runOnUiThread(new Runnable() {
                    @Override
                    public void run() {

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
        });
    }

    private void searchForSensors() {
        hideError();

        ViewUtil.fadeIn(progressBar, getContext());

        CommunicationFacade.getInstance().
                getAvailableSensors(
                        new CommunicationFacade.CommunicationCallback<List<Sensor>>() {
                            @Override
                            public void onResult(final List<Sensor> result) {
                                ((Activity) getContext()).runOnUiThread(new Runnable() {
                                    @Override
                                    public void run() {
                                        available = result;
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
                                });
                            }

                            @Override
                            public void onError(final CommunicationFacade.CommunicationError error) {
                                ((Activity) getContext()).runOnUiThread(new Runnable() {
                                    @Override
                                    public void run() {
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

                                });
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
        for (Sensor a : added) {
            TextView t = new TextView(getContext());
            t.setText(a.getIdentifier());
            connectWithLayout.addView(t);
        }

        if (delete.isEmpty()) {
            removeTitle.setVisibility(View.GONE);
        } else {
            removeTitle.setVisibility(View.VISIBLE);
        }
        removeLayout.removeAllViews();
        for (Sensor d : delete) {
            TextView t = new TextView(getContext());
            t.setText(d.getIdentifier());
            removeLayout.addView(t);
        }
        ViewUtil.fade(addLayout, connectionQLayout, getContext());
    }

    private void filterAvailable() {
        // Don't display already paired sensors as available
        // Not efficient but there is no use case with thousands of sensors in these lists
        for (Sensor p : paired) {
            for (int i = 0; i < available.size(); i++) {
                if (p.getIdentifier().equals(available.get(i).getIdentifier())) {
                    available.remove(i);
                }
            }
        }

        for (Sensor p : added) {
            for (int i = 0; i < available.size(); i++) {
                if (p.getIdentifier().equals(available.get(i).getIdentifier())) {
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
        // TODO: loading and error message here! maybe retry button!
        CommunicationFacade.getInstance().pairAndRemove(added, delete, new CommunicationFacade.CommunicationCallbackBinary() {
            @Override
            public void onSuccess() {
                ((Activity) getContext()).runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        getDialog().dismiss();
                    }
                });
            }

            @Override
            public void onError(final CommunicationFacade.CommunicationError error) {
                ((Activity) getContext()).runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        // TODO: show error
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
                        ViewUtil.fade(progressBar, connectionQLayout, getContext());
                    }
                });
            }
        });
    }

    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {

        }
    }
}
