package dryr.android.utils;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.content.Context;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Transformation;
import android.widget.FrameLayout;

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
                1f).setListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                in.setVisibility(View.VISIBLE);
            }
        });
    }

    /**
     * Fades a view out
     *
     * @param out view being faded out
     */
    public static final void fadeOut(final View out, Context context, int visbilityAfter) {
        fadeOut(out, context, 0f, visbilityAfter);
    }

    /**
     * Fades a view out
     *
     * @param out view being faded out
     */
    public static final void fadeOut(final View out, Context context) {
        fadeOut(out, context, 0f, View.GONE);
    }

    /**
     * Fades a view out
     *
     * @param out view being faded out
     */
    public static final void fadeOut(final View out, Context context, final float toAlpha, final int visbilityAfter) {
        int shortAnimTime = context.getResources().getInteger(android.R.integer.config_shortAnimTime);

        out.setVisibility(View.VISIBLE);
        out.animate().setDuration(shortAnimTime).alpha(
                toAlpha).setListener(new AnimatorListenerAdapter() {
            @Override
            public void onAnimationEnd(Animator animation) {
                out.setVisibility(visbilityAfter);
            }
        });
    }

    public static void expand(final View v) {
        final int initialHeight = v.getHeight();
        v.measure(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.WRAP_CONTENT);
        final int targetHeight = v.getMeasuredHeight();

        animateHeight(v, initialHeight, targetHeight);
    }

    public static void collapse(final View v) {
        final int initialHeight = v.getHeight();
        final int targetHeight = 1;

        animateHeight(v, initialHeight, targetHeight);
    }

    public static void animateHeight(final View v, final int initialHeight, final int targetHeight) {
        Animation a = new Animation()
        {
            @Override
            protected void applyTransformation(float interpolatedTime, Transformation t) {
                if (interpolatedTime == 1) {
                    v.getLayoutParams().height = FrameLayout.LayoutParams.WRAP_CONTENT;
                } else {
                    v.getLayoutParams().height =
                            (int) (initialHeight + (targetHeight - initialHeight) * interpolatedTime);
                }
                v.requestLayout();
            }

            @Override
            public boolean willChangeBounds() {
                return true;
            }
        };

        // 1dp / ms
        a.setDuration((int)(targetHeight / v.getContext().getResources().getDisplayMetrics().density));
        v.startAnimation(a);
    }

}
