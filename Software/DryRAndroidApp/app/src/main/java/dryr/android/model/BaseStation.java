package dryr.android.model;

/**
 * Class modelling a BaseStation
 */
public class BaseStation {
    private String identifier;

    public BaseStation(String identifier) {
        this.identifier = identifier;
    }

    public String getIdentifier() {
        return identifier;
    }
}
