package dryr.android.db;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

import java.util.ArrayList;
import java.util.List;

import dryr.android.utils.ConfigUtil;
import dryr.common.json.beans.HumiditySensorDataPoint;

/**
 * Class creating and managing a database table to store humidity information
 */
public class HumidityTable {
    public static final String TAG = "db_humidity_table";

    private static final String TABLE_NAME = "humidity";

    private static final String ID = "_id";
    private static final String DATE_TIME = "date_time";
    private static final String MAC = "mac";
    private static final String HUMIDITY = "value";


    /**
     * @return String to create this table
     */
    public static String createSql() {
        return "CREATE TABLE " + TABLE_NAME + "(" +
                ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                DATE_TIME + " String, " +
                MAC + " TEXT, " +
                HUMIDITY + " REAL, " +
                "UNIQUE (" + DATE_TIME + ", " + MAC + ") ON CONFLICT REPLACE" +
                ");";
    }

    /**
     * @return a String to drop the table
     */
    public static String dropSQL() {
        return "DROP TABLE IF EXISTS " + TABLE_NAME + ";";
    }

    private static HumidityTable instance;

    public static HumidityTable getInstance(Context context) {
        if (instance == null) {
            instance = new HumidityTable(context);
        }
        return instance;
    }

    private Context context;
    private List<HumidityDbListener> listeners = new ArrayList<>();

    public HumidityTable(Context context) {
        this.context = context;
    }

    public void registerListener(HumidityDbListener listener) {
        listeners.add(listener);
    }

    public void unregisterListener(HumidityDbListener listener) {
        listeners.remove(listener);
    }

    private void informListenersDPAdded(HumiditySensorDataPoint dataPoint) {
        for (HumidityDbListener listener : listeners) {
            listener.dataPointAdded(dataPoint, dataPoint.getSensor());
        }
    }

    private void informListenersDPDeleted(String mac) {
        for (HumidityDbListener listener : listeners) {
            listener.pointsDeleted(mac);
        }
    }

    synchronized public void addDataPoint(HumiditySensorDataPoint dataPoint) {
        HumiditySensorDataPoint latest = getLatestDataPoint(dataPoint.getSensor());
        // If humidity made a jump greater than a threshold from configuration delete old data points
        // -> new laundry
        if (latest != null && dataPoint.getDate().compareTo(latest.getDate()) >= 0
                && dataPoint.getHumidity() - latest.getHumidity() > ConfigUtil.getJumpThreshold(context)) {
            deleteDataPointsByMac(dataPoint.getSensor());
            DryTable.getInstance(context).deleteEntry(dataPoint.getSensor());
        }

        // Insert the data point
        ContentValues contentValues = new ContentValues();
        contentValues.put(DATE_TIME, dataPoint.getDate());
        contentValues.put(MAC, dataPoint.getSensor());
        contentValues.put(HUMIDITY, dataPoint.getHumidity());

        SQLiteDatabase db = new DryRDbHelper(context).getWritableDatabase();
        long insertResultCode = db.insert(TABLE_NAME, null, contentValues);
        db.close();
        if (insertResultCode < 0) {
            Log.e(TAG, "Error inserting data point: " + insertResultCode);
        } else {
            if (latest == null || !latest.getDate().equals(dataPoint.getDate())) {
                informListenersDPAdded(dataPoint);
            }
        }
    }

    /**
     * Returns all data points in the database from the sensor
     * The data of a sensor is automatically reset once the humidity is above the threshold again, so only
     * important data is kept and displayed
     *
     * @param mac the mac of the sensor in question
     * @return the data points
     */
    public List<HumiditySensorDataPoint> getDataPoints(String mac) {
        ArrayList<HumiditySensorDataPoint> dataPoints = new ArrayList<>();
        SQLiteDatabase db = new DryRDbHelper(context).getReadableDatabase();

        Cursor cursor = db.query(TABLE_NAME, null, MAC + " = " + "'" + mac + "'", null, null, null, DATE_TIME + " ASC");
        if (cursor != null && cursor.moveToFirst()) {
            do {
                dataPoints.add(dataPointFromCursor(cursor));
            } while (cursor.moveToNext());

            cursor.close();
        } else {
            Log.e(TAG, "Error getting data points");
        }
        db.close();

        return dataPoints;
    }

    public HumiditySensorDataPoint getLatestDataPoint(String mac) {
        HumiditySensorDataPoint dataPoint = null;
        SQLiteDatabase db = new DryRDbHelper(context).getReadableDatabase();

        Cursor cursor = db.query(TABLE_NAME, null, MAC + " = " + "'" + mac + "'", null, null, null, DATE_TIME + " DESC", "1");
        if (cursor != null && cursor.moveToFirst()) {
            dataPoint = dataPointFromCursor(cursor);
            cursor.close();
        } else {
            Log.e(TAG, "Error getting latest data point");
        }
        db.close();

        return dataPoint;
    }

    synchronized public void deleteDataPointsByMac(String mac) {
        SQLiteDatabase db = new DryRDbHelper(context).getWritableDatabase();
        db.delete(TABLE_NAME, MAC + " = '" + mac + "'", null);
        db.close();

        informListenersDPDeleted(mac);
    }

    private HumiditySensorDataPoint dataPointFromCursor(Cursor cursor) {
        int id = cursor.getInt(0);
        String dateTime = cursor.getString(1);
        String mac = cursor.getString(2);
        float humidity = cursor.getFloat(3);

        HumiditySensorDataPoint dataPoint = new HumiditySensorDataPoint();
        dataPoint.setDate(dateTime);
        dataPoint.setSensor(mac);
        dataPoint.setHumidity(humidity);

        return dataPoint;
    }

    public interface HumidityDbListener {
        public void dataPointAdded(HumiditySensorDataPoint dataPoint, String mac);

        public void pointsDeleted(String mac);
    }
}
