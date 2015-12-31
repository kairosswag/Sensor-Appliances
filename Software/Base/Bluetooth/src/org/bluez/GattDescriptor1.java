package org.bluez;
import java.util.List;
import org.freedesktop.dbus.DBusInterface;
public interface GattDescriptor1 extends DBusInterface
{

  public List<Byte> ReadValue();
  public void WriteValue(List<Byte> value);

}
