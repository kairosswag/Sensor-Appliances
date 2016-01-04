package dryr.android.model;

/**
 * Class modelling the state of a sensor
 */
public class SensorState {
    private int batteryLevel;
    private int connectionLevel;

    public SensorState(int batteryLevel, int connectionLevel) {
        this.batteryLevel = batteryLevel;
        this.connectionLevel = connectionLevel;
    }

    public int getBatteryLevel() {
        return batteryLevel;
    }

    public int getConnectionLevel() {
        return connectionLevel;
    }
}
