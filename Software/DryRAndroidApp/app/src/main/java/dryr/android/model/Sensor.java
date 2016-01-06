package dryr.android.model;

/**
 * Class modelling a Sensor
 */
public class Sensor {
    public static final int MAX_RECEPTION = 100;

    private String identifier;
    private int receptionLevel;

    public Sensor(String identifier, int receptionLevel) {
        this.identifier = identifier;
        this.receptionLevel = receptionLevel;
    }

    public int getReceptionLevel() {
        return receptionLevel;
    }

    public String getIdentifier() {
        return identifier;
    }
}
