/**
 * @(#)JAIRotatePanel.java	15.2 03/05/20
 *
 * Copyright (c) 2003 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * -Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * -Redistribution in binary form must reproduct the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
 * ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS
 * SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF
 * USING, MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO
 * EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT
 * OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR
 * PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF
 * LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN
 * IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that Software is not designed,licensed or intended for
 * use in the design, construction, operation or maintenance of any nuclear
 * facility.
 */


import java.awt.*;
import java.awt.event.*;
import java.awt.image.renderable.ParameterBlock;
import java.util.Hashtable;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;
import javax.swing.event.*;

public class JAIRotatePanel extends JAIDemoPanel
    implements ChangeListener, ItemListener {

    float theta = 0.0F;
    Interpolation interp;

    JSlider slider;
    JRadioButton nearest;
    JRadioButton linear;
    JRadioButton cubic;

    public JAIRotatePanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Rotate";
    }

    public void makeControls(JPanel controls) {
        slider = new JSlider(JSlider.HORIZONTAL, -180, 180, 0);

        Hashtable labels = new Hashtable();
        slider.setMajorTickSpacing(45);
        slider.setMinorTickSpacing(5);
        slider.setSnapToTicks(false);
        slider.setPaintTicks(true);
        slider.setPaintLabels(true);
        
        slider.addChangeListener(this);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.X_AXIS));
        sliderPanel.add(new JLabel("Angle"));
        sliderPanel.add(slider);

        nearest = new JRadioButton("Nearest Neighbor", true);
        linear = new JRadioButton("Bilinear", false);
        cubic = new JRadioButton("Bicubic", false);
        ButtonGroup bgroup = new ButtonGroup();
        bgroup.add(nearest);
        bgroup.add(linear);
        bgroup.add(cubic);

        nearest.addItemListener(this);
        linear.addItemListener(this);
        cubic.addItemListener(this);

        JPanel interpPanel = new JPanel();
        interpPanel.setLayout(new BoxLayout(interpPanel, BoxLayout.Y_AXIS));
        interpPanel.add(nearest);
        interpPanel.add(linear);
        interpPanel.add(cubic);

        controls.setLayout(new BorderLayout());
        controls.add("Center", sliderPanel);
        controls.add("East", interpPanel);
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);

        ParameterBlock pb = new ParameterBlock();
        pb.addSource(im);
        pb.add(0.0F);
        pb.add(0.0F);
        pb.add(theta);
        if (interp == null) {
            interp = Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        }
        pb.add(interp);
        return JAI.create("Rotate", pb, renderHints);
    }

    public void startAnimation() {
    }

    int sliderDelta = 5;

    public void animate() {
        int value = slider.getValue();
        int newValue = value + sliderDelta;

        if (newValue < slider.getMinimum() ||
            newValue > slider.getMaximum()) {
            sliderDelta = -sliderDelta;
        }
        slider.setValue(value + sliderDelta);
    }

    public void reset() {
        theta = 0.0F;
        nearest.setSelected(true);
        interp = Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        slider.setValue(0);
    }

    public void stateChanged(ChangeEvent e) {
        JSlider source = (JSlider)e.getSource();
        if (source.getValueIsAdjusting()) {
            return;
        }
        int value = slider.getValue();

        theta = (float)(value*(Math.PI/180.0F));
        repaint();
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.DESELECTED) {
            return;
        }

        if (e.getSource() == nearest) {
            interp =
                Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        } else if (e.getSource() == linear) {
            interp =
                Interpolation.getInstance(Interpolation.INTERP_BILINEAR);
        } else {
            interp =
                Interpolation.getInstance(Interpolation.INTERP_BICUBIC);
        }
        repaint();
    }
}
