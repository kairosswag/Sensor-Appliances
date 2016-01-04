package dryr.android.presenter.fragments;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.model.LaundryState;
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

    // Data
    private LaundryState laundryState;

    public LaundryStatusFragment() {
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
        View v = inflater.inflate(R.layout.fragment_laundry_status, container, false);

        progressBar = (ProgressBar) v.findViewById(R.id.laundry_status_progress);
        laundryStateLayout = (RelativeLayout) v.findViewById(R.id.laundry_status_layout);
        laundryStateText = (TextView) v.findViewById(R.id.laundry_status_displayText);

        if (laundryState != null) {
            setLaundryState(laundryState);
        }

        refreshState();

        return v;
    }

    private void refreshState() {
        showProgress(true);
        CommunicationFacade.getInstance().getLaundryState(new CommunicationFacade.CommunicationCallback<LaundryState>() {
            @Override
            public void onResult(LaundryState result) {
                setLaundryState(result);
                showProgress(false);
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                switch (error) {
                    case NO_BASE_STATION_CONNECTED:
                        // TODO: Show settings activity with ConnectBaseStationDialog open
                        break;

                    default:
                        // TODO: Show no network connection error
                        showProgress(false);
                }
            }
        });
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

    private void showProgress(boolean show) {
        if (show) {
            ViewUtil.fade(laundryStateLayout, progressBar, getActivity());
        } else {
            ViewUtil.fade(progressBar, laundryStateLayout, getActivity());
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
