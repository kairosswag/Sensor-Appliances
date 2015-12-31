package org.bluez;
import java.util.Map;
import org.freedesktop.dbus.DBusInterface;
import org.freedesktop.dbus.Variant;
public interface Media1 extends DBusInterface
{
  @SuppressWarnings("rawtypes")
  public void RegisterEndpoint(DBusInterface endpoint, Map<String,Variant> properties);
  public void UnregisterEndpoint(DBusInterface endpoint);
  @SuppressWarnings("rawtypes")
  public void RegisterPlayer(DBusInterface player, Map<String,Variant> properties);
  public void UnregisterPlayer(DBusInterface player);

}
