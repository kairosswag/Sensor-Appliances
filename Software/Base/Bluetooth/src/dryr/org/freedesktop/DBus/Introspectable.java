package dryr.org.freedesktop.DBus;
import org.freedesktop.dbus.DBusInterface;
import org.freedesktop.dbus.DBusInterfaceName;

@DBusInterfaceName("dryr.org.freedesktop.DBus.Introspectable")
public interface Introspectable extends DBusInterface
{

  public String Introspect();

}
