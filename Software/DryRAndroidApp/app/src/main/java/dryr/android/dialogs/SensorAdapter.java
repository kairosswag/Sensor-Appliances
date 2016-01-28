package dryr.android.dialogs;

import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.List;

import dryr.android.R;
import dryr.android.utils.FormatUtil;
import dryr.common.json.beans.BluetoothDevice;

/**
 * Adapter to supply a RecyclerView with views representing a list of Sensors.
 */
public class SensorAdapter extends RecyclerView.Adapter<SensorAdapter.SensorViewHolder> {

    private List<BluetoothDevice> sensors;
    private SensorAdapterListener listener;

    public SensorAdapter(List<BluetoothDevice> sensors, SensorAdapterListener listener) {
        this.sensors = sensors;
        this.listener = listener;
    }

    @Override
    public SensorViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        View itemView = inflater.inflate(R.layout.view_sensor_list_item, parent, false);
        return new SensorViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(SensorViewHolder holder, int position) {
        BluetoothDevice sensor = sensors.get(position);
        holder.title.setText(sensor.getMac());

        int reception = sensor.getRSSI();
        int step = holder.icon.getContext().getResources().getInteger(R.integer.sensor_bssi_max) / 5;
        if (reception < step) {
            holder.icon.setBackgroundResource(R.drawable.ic_signal_cellular_0_bar);
        } else if (reception < 2 * step) {
            holder.icon.setBackgroundResource(R.drawable.ic_signal_cellular_1_bar);
        } else if (reception < 3 * step) {
            holder.icon.setBackgroundResource(R.drawable.ic_signal_cellular_2_bar);
        } else if (reception < 4 * step) {
            holder.icon.setBackgroundResource(R.drawable.ic_signal_cellular_3_bar);
        } else {
            holder.icon.setBackgroundResource(R.drawable.ic_signal_cellular_4_bar);
        }
    }

    @Override
    public int getItemCount() {
        return sensors.size();
    }

    public class SensorViewHolder extends RecyclerView.ViewHolder {
        private TextView title;
        private View icon;

        public SensorViewHolder(View itemView) {
            super(itemView);
            title = (TextView) itemView.findViewById(R.id.sensor_list_item_title);
            icon = itemView.findViewById(R.id.sensor_list_item_reception_icon);

            itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (listener != null) {
                        listener.onSensorSelected(getAdapterPosition());
                    }
                }
            });
        }
    }

    public void setSensors(List<BluetoothDevice> sensors) {
        this.sensors = sensors;
    }

    public interface SensorAdapterListener {
        /**
         * Called when the users clicks a sensor from the displayed list
         * @param pos position of the sensor in the list
         */
        public void onSensorSelected(int pos);
    }
}
