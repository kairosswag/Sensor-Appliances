package dryr.android.presenter;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.design.widget.NavigationView;
import android.support.v4.app.Fragment;
import android.support.v4.app.ServiceCompat;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import java.util.ArrayList;
import java.util.List;

import dryr.android.R;
import dryr.android.background.DryRBackgroundService;
import dryr.android.background.DryRBackgroundServiceProvider;
import dryr.android.presenter.fragments.LaundryStatusFragment;
import dryr.android.presenter.fragments.SensorStatusFragment;
import dryr.android.presenter.fragments.SensorTabContainerFragment;
import dryr.android.presenter.listener.RefreshListener;
import dryr.common.json.beans.BluetoothDevice;

public class MainActivity extends AppCompatActivity
        implements NavigationView.OnNavigationItemSelectedListener, LaundryStatusFragment.OnFragmentInteractionListener,
                SensorStatusFragment.OnFragmentInteractionListener, SensorTabContainerFragment.OnFragmentInteractionListener {

    public static String SENSOR_MAC_EXTRA = "dryr.android.mac";

    private DrawerLayout drawer;

    private List<RefreshListener> listeners = new ArrayList<>();

    @Override
    protected void onPause() {
        super.onPause();
        // Inform the BackgroundService about app status
        if (DryRBackgroundService.isServiceRunning()) {
            DryRBackgroundServiceProvider.getInstance().getService(this).setAppRunning(false);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (DryRBackgroundService.isServiceRunning()) {
            DryRBackgroundServiceProvider.getInstance().getService(this).setAppRunning(true);
        }

        // Get extras
        String sensorMac = null;
        if (getIntent().getExtras() != null) {
            sensorMac = getIntent().getExtras().getString(SENSOR_MAC_EXTRA, null);
        }

        switchToLaundryStatus(sensorMac);
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        setIntent(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Start background service
        DryRBackgroundServiceProvider.getInstance().startService(this, true);

        // Setup toolbar and UI
        setContentView(R.layout.activity_main);

        // Set up navigation drawer
        drawer = (DrawerLayout) findViewById(R.id.drawer_layout);

        NavigationView navigationView = (NavigationView) findViewById(R.id.nav_view);
        navigationView.setNavigationItemSelectedListener(this);

        // Select first item and put the fragment in the container
        navigationView.getMenu().getItem(0).setChecked(true);
    }

    @Override
    public void setSupportActionBar(@Nullable Toolbar toolbar) {
        super.setSupportActionBar(toolbar);

        ActionBarDrawerToggle toggle = new ActionBarDrawerToggle(
                this, drawer, toolbar, R.string.navigation_drawer_open, R.string.navigation_drawer_close);
        drawer.setDrawerListener(toggle);
        toggle.syncState();
    }

    @Override
    public void onBackPressed() {
        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.drawer_layout);
        if (drawer.isDrawerOpen(GravityCompat.START)) {
            drawer.closeDrawer(GravityCompat.START);
        } else {
            super.onBackPressed();
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        if (id == R.id.refresh) {
            for (RefreshListener listener : listeners) {
                listener.refresh();
            }
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    @SuppressWarnings("StatementWithEmptyBody")
    @Override
    public boolean onNavigationItemSelected(MenuItem item) {
        // Handle navigation view item clicks here.
        int id = item.getItemId();

        if (id == R.id.nav_laundry_status) {
            switchToLaundryStatus(null);
        } else if (id == R.id.nav_sensor_status) {
            switchToSensorStatus();
        } else if (id == R.id.nav_settings) {
            Intent i = new Intent(this, DryRPreferenceActivity.class);
            startActivity(i);
        }

        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.drawer_layout);
        drawer.closeDrawer(GravityCompat.START);
        return true;
    }

    private void switchToLaundryStatus(String switchToSensorMac) {
        // Show LaundryStatusFragment (tabs)
        String tag = SensorTabContainerFragment.TAG + LaundryStatusFragment.TAG;
        SensorTabContainerFragment containerFragment = (SensorTabContainerFragment) getSupportFragmentManager().findFragmentByTag(tag);
        if (containerFragment == null) {
            containerFragment = new SensorTabContainerFragment();
            containerFragment.setContentProvider(new SensorTabContainerFragment.SensorTabContainerContentProvider() {
                @Override
                public Fragment getNewFragment(BluetoothDevice sensor) {
                    LaundryStatusFragment laundryStatusFragment = new LaundryStatusFragment();
                    laundryStatusFragment.setSensor(sensor);
                    return laundryStatusFragment;
                }
            });
        }
        containerFragment.setSensorToDisplay(switchToSensorMac);
        getSupportFragmentManager().beginTransaction()
                .replace(R.id.main_fragment_container, containerFragment, tag).commit();
    }

    private void switchToSensorStatus() {
        // Show SensorStatusFragment (tabs)
        String tag = SensorTabContainerFragment.TAG + SensorStatusFragment.TAG;
        SensorTabContainerFragment containerFragment = (SensorTabContainerFragment) getSupportFragmentManager().findFragmentByTag(tag);
        if (containerFragment == null) {
            containerFragment = new SensorTabContainerFragment();
            containerFragment.setContentProvider(new SensorTabContainerFragment.SensorTabContainerContentProvider() {
                @Override
                public Fragment getNewFragment(BluetoothDevice sensor) {
                    SensorStatusFragment sensorStatusFragment = new SensorStatusFragment();
                    sensorStatusFragment.setSensorState(sensor);
                    return sensorStatusFragment;
                }
            });
        }
        getSupportFragmentManager().beginTransaction()
                .replace(R.id.main_fragment_container, containerFragment, tag).commit();
    }

    @Override
    public void registerForRefresh(RefreshListener listener) {
        listeners.add(listener);
    }

    @Override
    public void unregisterForRefresh(RefreshListener listener) {
        listeners.remove(listener);
    }
}
