package org.bluez;
import java.util.List;
import java.util.Map;
import org.freedesktop.dbus.DBusInterface;
import org.freedesktop.dbus.Variant;
public interface GattManager1 extends DBusInterface
{
  @SuppressWarnings("rawtypes")
  public void RegisterService(DBusInterface service, Map<String,Variant> options);
  public void UnregisterService(DBusInterface service);
  @SuppressWarnings("rawtypes")
  public void RegisterProfile(DBusInterface profile, List<String> UUIDs, Map<String,Variant> options);
  public void UnregisterProfile(DBusInterface profile);

}
