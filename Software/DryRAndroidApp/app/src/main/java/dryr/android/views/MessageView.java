package dryr.android.views;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.TextView;

import dryr.android.R;
import dryr.android.utils.ViewUtil;

/**
 * Simple message view with retry button
 */
public class MessageView extends FrameLayout {

    private TextView message;
    private Button messageButton;

    public MessageView(Context context) {
        super(context);
        init();
    }

    public MessageView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public MessageView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init();
    }

    private void init() {
        LayoutInflater inflater = LayoutInflater.from(getContext());
        inflater.inflate(R.layout.view_message, this, true);

        message = (TextView) findViewById(R.id.view_message_message);
        messageButton = (Button) findViewById(R.id.view_message_btn);
    }

    public void showMessage(int messageTextId, int messageTextColorId, int buttonTextId, boolean showButton, View.OnClickListener clickListener) {
        if (getVisibility() != VISIBLE) {
            ViewUtil.fadeIn(this, getContext());
        }

        message.setText(messageTextId);
        message.setTextColor(getResources().getColor(messageTextColorId));
        if (showButton) {
            messageButton.setText(buttonTextId);
            messageButton.setOnClickListener(clickListener);
            if (messageButton.getVisibility() == View.GONE) {
                ViewUtil.fadeIn(messageButton, getContext());
            }
        } else {
            messageButton.setVisibility(View.GONE);
        }
        if (message.getVisibility() == View.GONE) {
            ViewUtil.fadeIn(message, getContext());
        }
    }

    public void hide() {
        ViewUtil.fadeOut(this, getContext());
    }
}
