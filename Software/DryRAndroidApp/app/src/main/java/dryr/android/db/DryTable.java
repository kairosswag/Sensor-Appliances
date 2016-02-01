package dryr.android.db;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

import java.util.ArrayList;
import java.util.List;

import dryr.android.utils.ConfigUtil;
import dryr.common.json.beans.Dry;
import dryr.common.json.beans.HumiditySensorDataPoint;

/**
 * Class creating and managing a database table to store humidity information
 */
public class DryTable {
    public static final String TAG = "db_dry_table";

    private static final String TABLE_NAME = "dry";

    private static final String ID = "_id";
    private static final String MAC = "mac";
    private static final String DRY = "dry";


    /**
     * @return String to create this table
     */
    public static String createSql() {
        return "CREATE TABLE " + TABLE_NAME + "(" +
                ID + " INTEGER PRIMARY KEY AUTOINCREMENT, " +
                MAC + " TEXT, " +
                DRY + " INTEGER, " +
                "UNIQUE (" + MAC + ") ON CONFLICT REPLACE" +
                ");";
    }

    /**
     * @return a String to drop the table
     */
    public static String dropSQL() {
        return "DROP TABLE IF EXISTS " + TABLE_NAME + ";";
    }

    private static DryTable instance;

    public static DryTable getInstance(Context context) {
        if (instance == null) {
            instance = new DryTable(context);
        }
        return instance;
    }

    private Context context;

    public DryTable(Context context) {
        this.context = context;
    }

    public void putEntry(Dry entry) {

        // Insert the data point
        ContentValues contentValues = new ContentValues();
        contentValues.put(MAC, entry.getMac());
        contentValues.put(DRY, entry.getDry() ? 1 : 0);

        SQLiteDatabase db = new DryRDbHelper(context).getWritableDatabase();
        long insertResultCode = db.insert(TABLE_NAME, null, contentValues);
        db.close();
        if (insertResultCode < 0) {
            Log.e(TAG, "Error inserting data point: " + insertResultCode);
        }
    }

    public Dry getEntry(String mac) {
        Dry entry = null;
        SQLiteDatabase db = new DryRDbHelper(context).getReadableDatabase();

        Cursor cursor = db.query(TABLE_NAME, null, MAC + " = " + "'" + mac + "'", null, null, null, null);
        if (cursor != null && cursor.moveToFirst()) {
            entry = entryFromCursor(cursor);
            cursor.close();
        } else {
            Log.e(TAG, "Error getting latest data point");
        }
        db.close();

        return entry;
    }

    public void deleteEntry(String mac) {
        SQLiteDatabase db = new DryRDbHelper(context).getWritableDatabase();
        db.delete(TABLE_NAME, MAC + " = '" + mac + "'", null);
        db.close();
    }

    private Dry entryFromCursor(Cursor cursor) {
        int id = cursor.getInt(0);
        String mac = cursor.getString(1);
        boolean dry = cursor.getInt(2) == 1;

        Dry dry1 = new Dry(mac, dry);

        return dry1;
    }

}
