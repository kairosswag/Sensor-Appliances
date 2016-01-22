package dryr.android.db;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

/**
 * Helper class to handle database create and upgrade
 */
public class DryRDbHelper extends SQLiteOpenHelper {
    private static String DB_NAME = "dryr.db";
    private static final int DB_VERSION = 4;

    public DryRDbHelper(Context context) {
        super(context, DB_NAME, null, DB_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL(HumidityTable.createSql());
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        db.execSQL(HumidityTable.dropSQL());
        onCreate(db);
    }
}
