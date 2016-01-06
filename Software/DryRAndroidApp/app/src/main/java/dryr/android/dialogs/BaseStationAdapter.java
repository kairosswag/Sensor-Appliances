package dryr.android.dialogs;

import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.List;

import dryr.android.R;
import dryr.android.model.BaseStation;

/**
 * Adapter to supply a RecyclerView with views representing  a list of BaseStations.
 */
public class BaseStationAdapter extends RecyclerView.Adapter<BaseStationAdapter.BaseStationViewHolder> {

    private List<BaseStation> stations;
    private BaseStationAdapterListener listener;

    public BaseStationAdapter(List<BaseStation> stations, BaseStationAdapterListener listener) {
        this.stations = stations;
        this.listener = listener;
    }

    @Override
    public BaseStationViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        View itemView = inflater.inflate(R.layout.view_base_station_list_item, parent, false);
        return new BaseStationViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(BaseStationViewHolder holder, int position) {
        BaseStation station = stations.get(position);
        holder.title.setText(station.getIdentifier());
    }

    @Override
    public int getItemCount() {
        return stations.size();
    }

    public class BaseStationViewHolder extends RecyclerView.ViewHolder {

        TextView title;

        public BaseStationViewHolder(View itemView) {
            super(itemView);
            title = (TextView) itemView.findViewById(R.id.base_station_list_item_title);
            itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (listener != null) {
                        listener.onBaseStationSelected(stations.get(getAdapterPosition()));
                    }
                }
            });
        }
    }

    public interface BaseStationAdapterListener {
        /**
         * Called when the users clicks a Base station from the displayed list
         * @param station the station
         */
        public void onBaseStationSelected(BaseStation station);
    }

}
