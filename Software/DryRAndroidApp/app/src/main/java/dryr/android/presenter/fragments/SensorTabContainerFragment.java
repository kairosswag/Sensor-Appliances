package dryr.android.presenter.fragments;


import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.design.widget.TabLayout;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentStatePagerAdapter;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import dryr.android.R;
import dryr.android.communication.CommunicationFacade;
import dryr.android.presenter.DryRPreferenceActivity;
import dryr.android.presenter.listener.RefreshListener;
import dryr.android.utils.ViewUtil;
import dryr.android.views.MessageView;
import dryr.common.json.beans.BluetoothDevice;

/**
 * A Fragment with tabs representing different sensors connected to the base station.
 * Supposed to be used with fragments that display sensor specific information
 */
public class SensorTabContainerFragment extends Fragment implements RefreshListener {

    public static final String TAG = "tab_container";
    private OnFragmentInteractionListener mListener;

    private TabLayout tabs;
    private ViewPager pager;
    private ViewPagerAdapter adapter;
    private ProgressBar progressBar;

    private MessageView messageView;

    private SensorTabContainerContentProvider contentProvider;
    private String sensorToDisplayMac;

    // Regularly refresh state
    private ScheduledThreadPoolExecutor scheduledThreadPoolExecutor = new ScheduledThreadPoolExecutor(1);
    private boolean refreshingScheduled = false;
    private ScheduledFuture<?> task;
    private boolean refreshing = false;

    public SensorTabContainerFragment() {
        // Required empty public constructor
    }

    public void setContentProvider(SensorTabContainerContentProvider contentProvider) {
        this.contentProvider = contentProvider;
    }

    public void setSensorToDisplay(String mac) {
        this.sensorToDisplayMac = mac;
    }

    @Override
    public void onResume() {
        super.onResume();

        // refresh regularly
        if (!refreshingScheduled) {

            refresh();
            int period = getResources().getInteger(R.integer.sensor_tab_fragment_refresh_frequency_period);

            refreshingScheduled = true;
            task = scheduledThreadPoolExecutor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    if (!refreshing) refresh();
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

        CommunicationFacade.getInstance(getActivity()).cancelAllByTag(this);

        mListener.unregisterForRefresh(this);
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.main, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        setHasOptionsMenu(true);

        // Inflate the layout for this fragment
        View v = inflater.inflate(R.layout.fragment_sensor_tab_container, container, false);

        Toolbar toolbar = (Toolbar) v.findViewById(R.id.toolbar);
        ((AppCompatActivity) getActivity()).setSupportActionBar(toolbar);

        pager = (ViewPager) v.findViewById(R.id.tab_container_viewPager);
        adapter = new ViewPagerAdapter(getActivity().getSupportFragmentManager(), contentProvider);
        pager.setAdapter(adapter);
        tabs = (TabLayout) v.findViewById(R.id.tab_container_tabs);
        tabs.setupWithViewPager(pager);

        progressBar = (ProgressBar) v.findViewById(R.id.tab_container_tabs_progress);

        messageView = (MessageView) v.findViewById(R.id.tab_container_message_view);

        return v;
    }

    public void refresh() {
        refreshing = true;

        CommunicationFacade.getInstance(getActivity()).getPairedSensors(new CommunicationFacade.CommunicationCallback<List<BluetoothDevice>>() {
            @Override
            public void onResult(List<BluetoothDevice> result) {
                refreshing = false;

                if (result.size() == 0) {
                    onError(CommunicationFacade.CommunicationError.NO_SENSOR_PAIRED);
                } else {
                    // TODO: maybe keep from changing to first tab if it updates
                    if (!sensorListEquals(adapter.sensors, result)) {
                        adapter.sensors = result;
                        adapter.notifyDataSetChanged();
                        tabs.setTabsFromPagerAdapter(adapter);

                        if (result.size() > 3) {
                            tabs.setTabMode(TabLayout.MODE_SCROLLABLE);
                        } else {
                            tabs.setTabMode(TabLayout.MODE_FIXED);
                        }

                        if (result.size() == 1) {
                            ViewUtil.fadeOut(tabs, getActivity());
                        } else {
                            ViewUtil.fadeIn(tabs, getActivity());
                        }
                        ViewUtil.fadeIn(pager, getActivity());
                    }

                    switchToPage(sensorToDisplayMac);
                    sensorToDisplayMac = null;
                    showProgress(false);
                }
            }

            @Override
            public void onError(CommunicationFacade.CommunicationError error) {
                refreshing = false;

                if (adapter.sensors.size() == 0) {
                    switch (error) {
                        case NO_BASE_STATION_FOUND:

                            // Base station was not found in network
                            messageView.showMessage(R.string.error_no_base_station_found, R.color.light_error_text_color, R.string.error_retry, true, new View.OnClickListener() {
                                @Override
                                public void onClick(View v) {
                                    refresh();
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
                                    refresh();
                                }
                            });
                    }
                }

                showProgress(false);
            }

            @Override
            public Object getTag() {
                return SensorTabContainerFragment.this;
            }
        });
    }

    private void switchToPage(String mac) {
        for (int i = 0; i < adapter.sensors.size(); i++) {
            if (adapter.sensors.get(i).getMac().equals(mac)) {
                pager.setCurrentItem(i, true);
            }
        }
    }

    private boolean sensorListEquals(List<BluetoothDevice> l1, List<BluetoothDevice> l2) {
        if (l1.size() != l2.size()) {
            return false;
        } else {
            for (BluetoothDevice d1 : l1) {
                boolean contains = false;
                for (BluetoothDevice d2 : l2) {
                    if (d1.getMac().equals(d2.getMac())) {
                        contains = true;
                        break;
                    }
                }
                if (!contains) {
                    return false;
                }
            }
            return true;
        }
    }

    private void showProgress(boolean show) {
        if (show) {
            ViewUtil.fadeIn(progressBar, getActivity());
        } else {
            ViewUtil.fadeOut(progressBar, getActivity());
        }
    }

    private static class ViewPagerAdapter extends FragmentStatePagerAdapter {
        private SensorTabContainerContentProvider contentProvider;
        private List<BluetoothDevice> sensors = new ArrayList<>();

        public ViewPagerAdapter(FragmentManager manager, SensorTabContainerContentProvider contentProvider) {
            super(manager);
            this.contentProvider = contentProvider;
        }

        @Override
        public Fragment getItem(int position) {

            return contentProvider.getNewFragment(sensors.get(position));
        }

        @Override
        public int getCount() {
            return sensors.size();
        }

        @Override
        public CharSequence getPageTitle(int position) {
            return sensors.get(position).getMac();
        }
    }

    public interface SensorTabContainerContentProvider {
        public Fragment getNewFragment(BluetoothDevice sensor);
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
