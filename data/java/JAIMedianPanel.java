/**
 * @(#)JAIMedianPanel.java	15.2 03/05/20
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
import java.awt.image.renderable.ParameterBlock;
import java.awt.event.*;
import java.util.Vector;
import javax.media.jai.*;
import javax.media.jai.operator.*;
import javax.swing.*;
import javax.swing.event.*;

public class JAIMedianPanel extends JAIDemoPanel implements ItemListener {

    JComboBox box;
    int type = -1;
    String[] labels = { "Original Image",
                        "3x3 Square",
                        "3x3 Separable square",
                        "3x3 Plus",
                        "3x3 X",
                        "5x5 Square",
                        "5x5 Separable square",
                        "5x5 Plus",
                        "5x5 X",
                        "7x7 Square",
                        "7x7 Separable square",
                        "7x7 Plus",
                        "7x7 X",
                        "9x9 Square",
                        "9x9 Separable square",
                        "9x9 Plus",
                        "9x9 X",
    };



    public JAIMedianPanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Median";
    }

    public void makeControls(JPanel controls) {
        box = new JComboBox();
        for (int i = 0; i < labels.length; i++) {
            box.addItem(labels[i]);
        }

        box.addItemListener(this);
        controls.add(box);
    }

    private static MedianFilterShape[] medianShapes = {
        MedianFilterDescriptor.MEDIAN_MASK_SQUARE,
        MedianFilterDescriptor.MEDIAN_MASK_SQUARE_SEPARABLE,
        MedianFilterDescriptor.MEDIAN_MASK_PLUS,
        MedianFilterDescriptor.MEDIAN_MASK_X
    };

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);

        if (type == -1) {
            return im;
        } else {
            int size = 2*(type/4) + 3;
            MedianFilterShape shape = medianShapes[type % 4];

            // Median operation
            ParameterBlock pb = new ParameterBlock();
            pb.addSource(im);
            pb.add(shape);
            pb.add(size);
            return JAI.create("medianfilter", pb, renderHints);
        }
    }

    public void startAnimation() {
    }

    public void animate() {
        try {
            int current = box.getSelectedIndex() + 1;

            if ( current >= labels.length ) {
                current = 0;
            }

            box.setSelectedIndex(current);
            Thread.sleep(1000);
        } catch( InterruptedException e ) {
        }
    }

    public void reset() {
        box.setSelectedIndex(0);
        type = -1;
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.DESELECTED) {
            return;
        }

        for (int i = 0; i < labels.length; i++) {
            if (e.getItem().equals(labels[i])) {
                type = i - 1;
                break;
            }
        }
        
        repaint();
    }
}
