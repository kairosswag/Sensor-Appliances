package dryr.android.dialogs;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.DialogPreference;
import android.preference.PreferenceManager;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import java.util.List;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.model.BaseStation;
import dryr.android.utils.ViewUtil;

/**
 * Dialog to connect a BaseStation to the app
 */
public class ConnectBaseStationDialogPreference extends DialogPreference {

    // Dialog ui
    private LinearLayout connectedLayout;
    private TextView connectedStationView;
    private Button disconnectStation;
    private Button connectNew;

    private LinearLayout connectLayout;
    private TextView errorText;
    private Button retry;
    private Button refresh;
    private TextView noneAvailable;
    private RecyclerView availableRecycler;

    private LinearLayout connectionSuccessLayout;
    private TextView successIdentifier;

    private ProgressBar progressBar;

    // Data
    private String connectedStation;
    private String deleteBaseStation;
    private BaseStation connectTo;

    public ConnectBaseStationDialogPreference(Context context, AttributeSet attrs) {
        super(context, attrs);
        setPersistent(false);
        setDialogLayoutResource(R.layout.dialog_connect_base_station);
    }

    @Override
    protected void onBindDialogView(View view) {
        super.onBindDialogView(view);
        // Update view with preference values
        connectedLayout = (LinearLayout) view.findViewById(R.id.dialog_connect_base_station_connected_station_layout);
        connectedStationView = (TextView) view.findViewById(R.id.dialog_connect_base_station_connected_station);
        disconnectStation = (Button) view.findViewById(R.id.dialog_connect_base_station_disconnect);
        disconnectStation.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                deleteBaseStation = connectedStation;
                connectedStation = null;
                connectedStationView.setText(R.string.pref_no_connected_base_station);
                disconnectStation.setVisibility(View.GONE);
            }
        });
        connectNew = (Button) view.findViewById(R.id.dialog_connect_base_station_new_bs);
        connectNew.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ViewUtil.fadeOut(connectedLayout, getContext());
                searchForBaseStations();
            }
        });


        connectLayout = (LinearLayout) view.findViewById(R.id.dialog_connect_base_station_connect_layout);
        errorText = (TextView) view.findViewById(R.id.dialog_connect_base_station_error_text);
        retry = (Button) view.findViewById(R.id.dialog_connect_base_station_retry);
        refresh = (Button) view.findViewById(R.id.dialog_connect_base_station_refresh);
        refresh.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ViewUtil.fadeOut(connectLayout, getContext());
                searchForBaseStations();
            }
        });
        noneAvailable = (TextView) view.findViewById(R.id.dialog_connect_base_station_available_none);
        availableRecycler = (RecyclerView) view.findViewById(R.id.dialog_connect_base_station_available_recycler);
        availableRecycler.setLayoutManager(new LinearLayoutManager(getContext(), LinearLayoutManager.VERTICAL, false));


        connectionSuccessLayout = (LinearLayout) view.findViewById(R.id.dialog_connect_base_station_connection_successful_layout);
        successIdentifier = (TextView) view.findViewById(R.id.dialog_connect_base_station_success_id);


        progressBar = (ProgressBar) view.findViewById(R.id.dialog_connect_base_station_progress);


        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getContext());
        String baseStation = sp.getString(getContext().getString(R.string.pref_baseStation_connect_key), null);

        if (baseStation != null) {
            connectedStation = baseStation;
            connectedStationView.setText(baseStation);
            disconnectStation.setVisibility(View.VISIBLE);
        } else {
            connectedStation = null;
            connectedStationView.setText(R.string.pref_no_connected_base_station);
            disconnectStation.setVisibility(View.GONE);
        }
    }

    private void searchForBaseStations() {
        ViewUtil.fadeIn(progressBar, getContext());
        CommunicationFacade.getInstance().getAvailableBaseStations(new CommunicationFacade.CommunicationCallback<List<BaseStation>>() {
            @Override
            public void onResult(List<BaseStation> result) {
                if (result.isEmpty()) {
                    // No available stations
                    noneAvailable.setVisibility(View.VISIBLE);
                    availableRecycler.setVisibility(View.GONE);
                } else {
                    // Display available stations in a recycler react to onCLick
                    noneAvailable.setVisibility(View.GONE);
                    availableRecycler.setVisibility(View.VISIBLE);

                    BaseStationAdapter adapter = new BaseStationAdapter(result, new BaseStationAdapter.BaseStationAdapterListener() {
                        @Override
                        public void baseStationSelected(BaseStation station) {
                            ViewUtil.fadeOut(connectLayout, getContext());
                            connectToBaseStation(station);
                        }
                    });
                    availableRecycler.setAdapter(adapter);
                }

                ViewUtil.fade(progressBar, connectLayout, getContext());
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    default:
                        // Display error
                        ViewUtil.fade(progressBar, connectLayout, getContext());
                        errorText.setText(R.string.error_connection_default);
                        errorText.setVisibility(View.VISIBLE);
                }

                // Give option to retry
                retry.setVisibility(View.VISIBLE);
                retry.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        searchForBaseStations();
                    }
                });

                ViewUtil.fade(progressBar, connectLayout, getContext());
            }
        });
    }

    private void connectToBaseStation(final BaseStation station) {
        connectTo = station;

        hideError();
        ViewUtil.fadeIn(progressBar, getContext());
        CommunicationFacade.getInstance().tryConnection(station, new CommunicationFacade.CommunicationCallbackBinary() {
            @Override
            public void onSuccess() {
                successIdentifier.setText(connectTo.getIdentifier());
                ViewUtil.fade(progressBar, connectionSuccessLayout, getContext());
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    default:
                        errorText.setVisibility(View.VISIBLE);
                        errorText.setText(R.string.error_connection_default);
                }
                ViewUtil.fade(progressBar, connectionSuccessLayout, getContext());
            }
        });
    }

    private void hideError() {
        ViewUtil.fadeOut(errorText, getContext());
        ViewUtil.fadeOut(retry, getContext());
    }


    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {
            SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getContext());
            if (deleteBaseStation != null) {
                sp.edit().remove(getContext().getString(R.string.pref_baseStation_connect_key)).commit();
                CommunicationFacade.getInstance().disconnectFromStation(deleteBaseStation);
            }

            if (connectTo != null) {
                sp.edit().putString(getContext().getString(R.string.pref_baseStation_connect_key), connectTo.getIdentifier()).commit();
                CommunicationFacade.getInstance().connectPermanently(connectTo);
            }
        }
    }
}
