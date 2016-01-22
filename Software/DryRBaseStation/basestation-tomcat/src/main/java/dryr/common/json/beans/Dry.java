package dryr.common.json.beans;

public class Dry {
	private final String mac;
	private final boolean dry;

	public Dry(String mac, boolean dry) {
		this.mac = mac;
		this.dry = dry;
	}

	public String getMac() {
		return mac;
	}

	public boolean getDry() {
		return dry;
	}
}
