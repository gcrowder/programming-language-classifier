/**
 * @(#)JAISamplePanel.java	15.2 03/05/20
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
import java.awt.image.renderable.RenderedImageFactory;
import java.util.Hashtable;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.media.jai.registry.RIFRegistry;

public class JAISamplePanel extends JAIDemoPanel
    implements ChangeListener, ItemListener {

    int param1 = 0;
    int param2 = 255;
    int sliderDelta = 4;

    JSlider p1Slider;
    JSlider p2Slider;

    public JAISamplePanel(Vector sourceVec) {
        super(sourceVec);

        SampleDescriptor sampleDescriptor = new SampleDescriptor();
        OperationDescriptor odesc = sampleDescriptor;
        RenderedImageFactory rif = sampleDescriptor;

        String operationName = "sample";
        String productName = "com.mycompany";
        OperationRegistry or = JAI.getDefaultInstance().getOperationRegistry();
        or.registerDescriptor(odesc);
        RIFRegistry.register(or, operationName,productName,rif);
        masterSetup();
    }

    public String getDemoName() {
        return "Sample";
    }

    public void makeControls(JPanel controls) {
        p1Slider = new JSlider(JSlider.HORIZONTAL, 0, 255, 15);
        p2Slider = new JSlider(JSlider.HORIZONTAL, 0, 255, 240);

        Hashtable labels = new Hashtable();
        labels.put(new Integer(0), new JLabel("0"));
        labels.put(new Integer(128), new JLabel("128"));
        labels.put(new Integer(255), new JLabel("255"));
        p1Slider.setLabelTable(labels);
        p2Slider.setLabelTable(labels);
        p1Slider.setPaintLabels(true);
        p2Slider.setPaintLabels(true);
        
        p1Slider.addChangeListener(this);
        p2Slider.addChangeListener(this);

        JPanel p1SliderPanel = new JPanel();
        p1SliderPanel.setLayout(new BoxLayout(p1SliderPanel,
                                              BoxLayout.X_AXIS));
        JLabel p1Label = new JLabel("Lower bound");
        p1SliderPanel.add(p1Label);
        p1SliderPanel.add(p1Slider);

        JPanel p2SliderPanel = new JPanel();
        p2SliderPanel.setLayout(new BoxLayout(p2SliderPanel, BoxLayout.X_AXIS));
        JLabel p2Label = new JLabel("Upper bound");
        p2Label.setPreferredSize(p1Label.getPreferredSize());
        p2SliderPanel.add(p2Label);
        p2SliderPanel.add(p2Slider);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.Y_AXIS));
        sliderPanel.add(p1SliderPanel);
        sliderPanel.add(p2SliderPanel);

        controls.setLayout(new BorderLayout());
        controls.add("Center", sliderPanel);
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);

        ParameterBlock pb = new ParameterBlock();
        pb.addSource(im);
        pb.add(param1);
        pb.add(param2);
        return JAI.create("sample", pb, renderHints);
    }

    public void startAnimation() {
    }

    public void animate() {
        int value = p1Slider.getValue();
        int temp = p2Slider.getValue();
        int newValue = value + sliderDelta;

        if ( newValue < p1Slider.getMinimum() ||
             newValue >= temp ) {
            sliderDelta = -sliderDelta;
        }

        p1Slider.setValue(value + sliderDelta);
    }

    public void reset() {
        param1 = 0;
        param2 = 255;
        p1Slider.setValue(param1);
        p2Slider.setValue(param2);
    }

    public void stateChanged(ChangeEvent e) {
        JSlider source = (JSlider)e.getSource();
        int value = source.getValue();

        if (source == p1Slider) {
             param1 = value;
             if (value > p2Slider.getValue()) {
                 p2Slider.setValue(value);
             }
        } else {
             param2 = value;
             if (value < p1Slider.getValue()) {
                 p1Slider.setValue(value);
             }
        }
        repaint();
    }

    public void itemStateChanged(ItemEvent e) {
    }
}
