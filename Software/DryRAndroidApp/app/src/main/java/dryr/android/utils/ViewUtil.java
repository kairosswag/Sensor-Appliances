package dryr.android.utils;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.content.Context;
import android.view.View;

/**
 * Utils that work with android views
 */
public class ViewUtil {
    /**
     * Fades a view in and another one out
     *
     * @param out view being faded out
     * @param in  view being faded in
     */
    public static final void fade(final View out, final View in, Context context) {
        int shortAnimTime = context.getResources().getInteger(android.R.integer.config_shortAnimTime);

        fadeOut(out, context);
        fadeIn(in, context);
    }

    /**
     * Fades a view in
     *
     * @param in view being faded in
     */
    public static final void fadeIn(final View in, Context context) {
        int shortAnimTime = context.getResources().getInteger(android.R.integer.config_shortAnimTime);
        in.setVisibility(View.VISIBLE);
        in.animate().setDuration(shortAnimTime).alpha(
                1).setListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                in.setVisibility(View.VISIBLE);
            }
        });
    }

    /**
     * Fades a view in and another one out
     *
     * @param out view being faded out
     */
    public static final void fadeOut(final View out, Context context) {
        if (out.getVisibility() != View.VISIBLE) {
            return;
        }

        int shortAnimTime = context.getResources().getInteger(android.R.integer.config_shortAnimTime);

        out.setVisibility(View.VISIBLE);
        out.animate().setDuration(shortAnimTime).alpha(
                0).setListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                out.setVisibility(View.GONE);
            }
        });

    }


}
