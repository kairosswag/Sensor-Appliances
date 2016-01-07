package dryr.base.bluetooth.chained_handler;

public interface SubHandler<T> {
	boolean handle(T in);
}
