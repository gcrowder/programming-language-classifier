/**
 * @(#)JAIPatternPanel.java	15.2 03/05/20
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
import java.awt.image.*;
import java.awt.image.renderable.ParameterBlock;
import java.util.Hashtable;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;
import javax.swing.event.*;

public class JAIPatternPanel extends JAIDemoPanel
    implements ChangeListener {

    JSlider widthSlider;
    JSlider heightSlider;
    int width = 200;
    int height = 200;

    PlanarImage source;

    public JAIPatternPanel(Vector sourceVec) {
        super(sourceVec);
        
        source = getSource(0);
        masterSetup();
    }

    public String getDemoName() {
        return "Pattern";
    }

    public void makeControls(JPanel controls) {
        widthSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 200);
        heightSlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 200);

        widthSlider.setMajorTickSpacing(100);
        widthSlider.setPaintTicks(true);
        widthSlider.setPaintLabels(true);

        heightSlider.setMajorTickSpacing(100);
        heightSlider.setPaintTicks(true);
        heightSlider.setPaintLabels(true);
        
        widthSlider.addChangeListener(this);
        heightSlider.addChangeListener(this);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.X_AXIS));

        JLabel widthLabel = new JLabel("Width");
        sliderPanel.add(widthLabel);
        sliderPanel.add(widthSlider);

        JLabel heightLabel = new JLabel("Height");
        sliderPanel.add(heightLabel);
        sliderPanel.add(heightSlider);

        controls.setLayout(new BorderLayout());
        controls.add("Center", sliderPanel);
    }

    public void setSource(int sourceNum, PlanarImage source) {
        if (sourceNum == 0) {
            this.source = source;
        }
        super.setSource(sourceNum, source);
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        ParameterBlock pb = new ParameterBlock();
        pb.addSource(source);
        pb.add(width);
        pb.add(height);
        return JAI.create("pattern", pb, renderHints);
    }

    public void startAnimation() {
    }

    int widthSliderDelta = 20;
    int heightSliderDelta = 20;

    public void animate() {
        int value = widthSlider.getValue();
        int newValue = value + widthSliderDelta;

        if (newValue < widthSlider.getMinimum() ||
            newValue > widthSlider.getMaximum()) {
            widthSliderDelta = -widthSliderDelta;
        }
        widthSlider.setValue(value + widthSliderDelta);

        value = heightSlider.getValue();
        newValue = value + heightSliderDelta;

        if (newValue < heightSlider.getMinimum() ||
            newValue > heightSlider.getMaximum()) {
            heightSliderDelta = -heightSliderDelta;
        }
        heightSlider.setValue(value + heightSliderDelta);
    }

    public void reset() {
        width = height = 200;
        widthSlider.setValue(width);
        heightSlider.setValue(height);
    }

    public void stateChanged(ChangeEvent e) {
        JSlider slider = (JSlider)e.getSource();
        if (slider == widthSlider) {
            width = slider.getValue();
            if (width == 0) {
                width = 1;
            }
        } else {
            height = slider.getValue();
            if (height == 0) {
                height = 1;
            }
        }
        repaint();
    }
}
