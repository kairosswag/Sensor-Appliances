package dryr.android.presenter;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.Preference;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.support.v7.app.AppCompatActivity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import dryr.android.R;

public class DryRPreferenceActivity extends AppCompatActivity{

    public static String OPEN_PREFERENCE_KEY = "open_preference";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();

        // Show PreferenceFragment and supply it with Intent arguments
        DryRPreferenceFragment dryRPreferenceFragment = new DryRPreferenceFragment();
        dryRPreferenceFragment.setArguments(intent.getExtras());
        getFragmentManager().beginTransaction().replace(android.R.id.content, dryRPreferenceFragment).commit();
    }

    public static class DryRPreferenceFragment extends PreferenceFragment {

        @Override
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            addPreferencesFromResource(R.xml.preferences);
        }

        @Override
        public void onResume() {
            super.onResume();
        }

        @Override
        public void onPause() {
            super.onPause();
        }

        @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            View v = super.onCreateView(inflater, container, savedInstanceState);

            // Open preference matching the key that was supplied in the arguments
            Bundle arguments = getArguments();

            if (arguments != null) {
                String openPreferenceKey = arguments.getString(OPEN_PREFERENCE_KEY, null);
                if (openPreferenceKey != null) {

                    PreferenceScreen screen = (PreferenceScreen) findPreference("screen");
                    Preference preference = findPreference(openPreferenceKey);
                    int pos = preference.getOrder();

                    screen.onItemClick(null, null, pos + 1, 0);
                }
            }

            return v;
        }
    }
}
