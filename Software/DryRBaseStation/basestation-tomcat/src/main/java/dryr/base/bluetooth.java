package dryr.base;
import org.freedesktop.dbus.DBusInterface;
public interface bluetooth extends DBusInterface
{

  public void ConnectDevice(String a);
  public void DisconnectDevice(String a);
  public void RemoveDevice(String a);

}
