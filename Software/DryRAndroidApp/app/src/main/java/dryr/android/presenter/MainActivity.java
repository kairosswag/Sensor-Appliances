package dryr.android.presenter;

import android.os.Bundle;
import android.support.design.widget.NavigationView;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;

import dryr.android.R;
import dryr.android.presenter.fragments.LaundryStatusFragment;
import dryr.android.presenter.fragments.SensorStatusFragment;

public class MainActivity extends AppCompatActivity
        implements NavigationView.OnNavigationItemSelectedListener, LaundryStatusFragment.OnFragmentInteractionListener,
                SensorStatusFragment.OnFragmentInteractionListener {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        // Set up navigation drawer
        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.drawer_layout);
        ActionBarDrawerToggle toggle = new ActionBarDrawerToggle(
                this, drawer, toolbar, R.string.navigation_drawer_open, R.string.navigation_drawer_close);
        drawer.setDrawerListener(toggle);
        toggle.syncState();

        NavigationView navigationView = (NavigationView) findViewById(R.id.nav_view);
        navigationView.setNavigationItemSelectedListener(this);

        // Select first item and put the fragment in the container
        navigationView.getMenu().getItem(0).setChecked(true);
        getSupportFragmentManager().beginTransaction().
                replace(R.id.main_fragment_container, new LaundryStatusFragment(), LaundryStatusFragment.TAG).commit();
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
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        return super.onOptionsItemSelected(item);
    }

    @SuppressWarnings("StatementWithEmptyBody")
    @Override
    public boolean onNavigationItemSelected(MenuItem item) {
        // Handle navigation view item clicks here.
        int id = item.getItemId();

        if (id == R.id.nav_laundry_status) {
            LaundryStatusFragment laundryStatusFragment = (LaundryStatusFragment) getSupportFragmentManager().findFragmentByTag(LaundryStatusFragment.TAG);
            if (laundryStatusFragment == null) {
                laundryStatusFragment = new LaundryStatusFragment();
            }
            getSupportFragmentManager().beginTransaction()
                    .replace(R.id.main_fragment_container, laundryStatusFragment, LaundryStatusFragment.TAG).commit();
        } else if (id == R.id.nav_sensor_status) {
            SensorStatusFragment sensorStatusFragment = (SensorStatusFragment) getSupportFragmentManager().findFragmentByTag(SensorStatusFragment.TAG);
            if (sensorStatusFragment == null) {
                sensorStatusFragment = new SensorStatusFragment();
            }
            getSupportFragmentManager().beginTransaction()
                    .replace(R.id.main_fragment_container, sensorStatusFragment, SensorStatusFragment.TAG).commit();
        } else if (id == R.id.nav_settings) {

        }

        DrawerLayout drawer = (DrawerLayout) findViewById(R.id.drawer_layout);
        drawer.closeDrawer(GravityCompat.START);
        return true;
    }
}
