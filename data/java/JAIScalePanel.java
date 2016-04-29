/**
 * @(#)JAIScalePanel.java	15.2 03/05/20
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

public class JAIScalePanel extends JAIDemoPanel
    implements ChangeListener, ItemListener {

    float xscale = 1.0F;
    float yscale = 1.0F;
    float xtrans = 0.0F;
    float ytrans = 0.0F;
    Interpolation interp;

    JSlider xySlider;
    JSlider ySlider;
    JSlider xTSlider;
    JSlider yTSlider;
    JRadioButton nearest;
    JRadioButton linear;
    JRadioButton cubic;

    public JAIScalePanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Scale";
    }

    public void makeControls(JPanel controls) {
        xySlider = new JSlider(JSlider.HORIZONTAL, -40, 90, 0);
        ySlider = new JSlider(JSlider.HORIZONTAL, -40, 90, 0);
        xTSlider = new JSlider(JSlider.HORIZONTAL, 0, 99, 0);
        yTSlider = new JSlider(JSlider.HORIZONTAL, 0, 99, 0);

        Hashtable labels = new Hashtable();
        labels.put(new Integer(-40), new JLabel("1/5"));
        labels.put(new Integer(-30), new JLabel("1/4"));
        labels.put(new Integer(-20), new JLabel("1/3"));
        labels.put(new Integer(-10), new JLabel("1/2"));
        labels.put(new Integer(0), new JLabel("1"));
        labels.put(new Integer(10), new JLabel("2"));
        labels.put(new Integer(20), new JLabel("3"));
        labels.put(new Integer(30), new JLabel("4"));
        labels.put(new Integer(40), new JLabel("5"));
        labels.put(new Integer(50), new JLabel("6"));
        labels.put(new Integer(60), new JLabel("7"));
        labels.put(new Integer(70), new JLabel("8"));
        labels.put(new Integer(80), new JLabel("9"));
        labels.put(new Integer(90), new JLabel("10"));
        xySlider.setLabelTable(labels);
        ySlider.setLabelTable(labels);
        xySlider.setPaintLabels(true);
        ySlider.setPaintLabels(true);
        
        xySlider.addChangeListener(this);
        ySlider.addChangeListener(this);
        xTSlider.addChangeListener(this);
        yTSlider.addChangeListener(this);

        JPanel xySliderPanel = new JPanel();
        xySliderPanel.setLayout(new BoxLayout(xySliderPanel,
                                              BoxLayout.X_AXIS));
        JLabel xyLabel = new JLabel("X/Y Scale");
        xySliderPanel.add(xyLabel);
        xySliderPanel.add(xySlider);

        JPanel ySliderPanel = new JPanel();
        ySliderPanel.setLayout(new BoxLayout(ySliderPanel, BoxLayout.X_AXIS));
        JLabel yLabel = new JLabel("Y Scale");
        yLabel.setPreferredSize(xyLabel.getPreferredSize());
        ySliderPanel.add(yLabel);
        ySliderPanel.add(ySlider);

        /*
        // Translation factors
        JPanel xTSliderPanel = new JPanel();
        xTSliderPanel.setLayout(new BoxLayout(xTSliderPanel,
                                              BoxLayout.X_AXIS));
        JLabel xTLabel = new JLabel("X Translate");
        xTLabel.setPreferredSize(xyLabel.getPreferredSize());
        xTSliderPanel.add(xTLabel);
        xTSliderPanel.add(xTSlider);

        JPanel yTSliderPanel = new JPanel();
        yTSliderPanel.setLayout(new BoxLayout(yTSliderPanel,
                                              BoxLayout.X_AXIS));
        JLabel yTLabel = new JLabel("Y Translate");
        yTLabel.setPreferredSize(xyLabel.getPreferredSize());
        yTSliderPanel.add(xTLabel);
        yTSliderPanel.add(xTSlider);
        */



        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.Y_AXIS));
        sliderPanel.add(xySliderPanel);
        sliderPanel.add(ySliderPanel);
        // sliderPanel.add(xTSliderPanel);
        // sliderPanel.add(yTSliderPanel);

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
        pb.add(xscale);
        pb.add(yscale);
        pb.add(0.0F);
        pb.add(0.0F);
        // pb.add(xtrans);
        // pb.add(ytrans);
        if (interp == null) {
            interp = Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        }
        pb.add(interp);
        return JAI.create("scale", pb, renderHints);
    }

    int xySliderDelta = 1;

    public void startAnimation() {
    }

    public void animate() {
        int value = xySlider.getValue();
        int newValue = value + xySliderDelta;

        if (newValue < xySlider.getMinimum() || 
            newValue > xySlider.getMaximum()) {
            xySliderDelta = -xySliderDelta;
        }
        xySlider.setValue(value + xySliderDelta);
    }

    public void reset() {
        xscale = yscale = 1.0F;
        nearest.setSelected(true);
        interp = Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        xySlider.setValue(0);
        ySlider.setValue(0);
    }

    public void stateChanged(ChangeEvent e) {
        JSlider source = (JSlider)e.getSource();
        if (!(interp instanceof InterpolationNearest) &&
            source.getValueIsAdjusting()) {
            return;
        }
        int value = source.getValue();

        float fvalue = Math.abs(value)/10.0F + 1.0F;
        if (value < 0) {
            fvalue = 1.0F/fvalue;
        }

        if (source == xySlider) {
            ySlider.setValue(value);
            xscale = yscale = fvalue;
        } else if (source == ySlider) {
            yscale = fvalue;
        } else if (source == xTSlider) {
            xtrans = value/100.0F;
        } else if (source == yTSlider) {
            ytrans = value/100.0F;
        }
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
