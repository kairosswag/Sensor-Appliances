package dryr.android.model;

/**
 * Class modeling the state of the laundry
 */
public class LaundryState {
    private boolean dry = false;

    public LaundryState(boolean dry) {
        this.dry = dry;
    }

    public boolean isDry() {
        return dry;
    }
}
