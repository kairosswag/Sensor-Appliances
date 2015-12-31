package dryr.org.freedesktop.DBus;
import java.util.List;
import java.util.Map;
import org.freedesktop.dbus.DBusInterface;
import org.freedesktop.dbus.DBusInterfaceName;
import org.freedesktop.dbus.DBusSignal;
import org.freedesktop.dbus.Variant;
import org.freedesktop.dbus.exceptions.DBusException;

@DBusInterfaceName("org.freedesktop.DBus.Properties")
public interface Properties extends DBusInterface
{
   public static class PropertiesChanged extends DBusSignal
   {
      public final String _interface;
      public final Map<String,Variant> changed_properties;
      public final List<String> invalidated_properties;
      public PropertiesChanged(String path, String _interface, Map<String,Variant> changed_properties, List<String> invalidated_properties) throws DBusException
      {
         super(path, _interface, changed_properties, invalidated_properties);
         this._interface = _interface;
         this.changed_properties = changed_properties;
         this.invalidated_properties = invalidated_properties;
      }
   }

  public Variant Get(String _interface, String name);
  public void Set(String _interface, String name, Variant value);
  public Map<String,Variant> GetAll(String _interface);

}
