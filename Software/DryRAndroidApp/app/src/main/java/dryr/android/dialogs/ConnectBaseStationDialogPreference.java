package dryr.android.dialogs;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.DialogPreference;
import android.preference.PreferenceManager;
import android.support.v7.widget.RecyclerView;
import android.util.AttributeSet;
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
    private TextView connectedStation;
    private Button disconnectStation;
    private Button connectNew;

    private LinearLayout connectLayout;
    private TextView errorText;
    private TextView noneAvailable;
    private RecyclerView availableRecycler;

    private ProgressBar progressBar;

    // Data
    private String deleteBaseStation;

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
        connectedStation = (TextView) view.findViewById(R.id.dialog_connect_base_station_connected_station);
        disconnectStation = (Button) view.findViewById(R.id.dialog_connect_base_station_disconnect);
        disconnectStation.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                deleteBaseStation = (String) connectedStation.getTag();
            }
        });
        connectNew = (Button) view.findViewById(R.id.dialog_connect_base_station_new_bs);
        connectNew.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ViewUtil.fade(connectedLayout, progressBar, getContext());
                CommunicationFacade.getInstance().getAvailableBaseStations(new CommunicationFacade.CommunicationCallback<List<BaseStation>>() {
                    @Override
                    public void onResult(List<BaseStation> result) {
                        // TODO: show in recycler with click action
                        ViewUtil.fade(progressBar, connectLayout, getContext());
                    }

                    @Override
                    public void onError(CommunicationFacade.CommunicationError error) {
                        switch (error) {
                            default:
                                ViewUtil.fade(progressBar, connectLayout, getContext());
                                // TODO: show error + retry button!
                        }
                    }
                });
            }
        });

        connectLayout = (LinearLayout) view.findViewById(R.id.dialog_connect_base_station_connect_layout);
        errorText = (TextView) view.findViewById(R.id.dialog_connect_base_station_error_text);
        noneAvailable = (TextView) view.findViewById(R.id.dialog_connect_base_station_available_none);
        availableRecycler = (RecyclerView) view.findViewById(R.id.dialog_connect_base_station_available_recycler);

        progressBar = (ProgressBar) view.findViewById(R.id.dialog_connect_base_station_progress);

        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getContext());
        String baseStation = sp.getString(getContext().getString(R.string.pref_baseStation_connect_key), null);

        if (baseStation != null) {
            connectedStation.setText(baseStation);
            connectedStation.setTag(baseStation);
            disconnectStation.setVisibility(View.VISIBLE);
        } else {
            connectedStation.setText(R.string.pref_no_connected_base_station);
            disconnectStation.setVisibility(View.GONE);
        }
    }

    @Override
    protected void onDialogClosed(boolean positiveResult) {
        super.onDialogClosed(positiveResult);
        if (positiveResult) {
            // Persist data needed in shared preferences
            // TODO: connect new base station
            // TODO: delete old base station
        }
    }
}
