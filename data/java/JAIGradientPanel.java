/**
 * @(#)JAIGradientPanel.java	15.2 03/05/20
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
import java.awt.image.renderable.*;
import java.awt.event.*;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;
import javax.swing.event.*;

public class JAIGradientPanel extends JAIDemoPanel implements ItemListener {

    static final String[] kernelLabels = { "Original Image",
                                           "Sobel",
                                           "Roberts",
                                           "Prewitt",
                                           "Frei-chen"
    };

    JComboBox kernelBox;

    KernelJAI[] kernels;
    KernelJAI kern_h, kern_v;

    public JAIGradientPanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Gradient";
    }

    public void makeControls(JPanel controls) {
        kernelBox = new JComboBox();
        for (int i = 0; i < kernelLabels.length; i++) {
            kernelBox.addItem(kernelLabels[i]);
        }

        kernelBox.addItemListener(this);
        controls.add(kernelBox);
    }

    private void initKernels() {
        kernels = new KernelJAI[kernelLabels.length*2];

        float[] normal_h_data       = { 1.0F };
        float[] normal_v_data       = { 0.0F };
        
        float[] sobel_h_data        = { 1.0F,  0.0F, -1.0F,
                                        2.0F,  0.0F, -2.0F,
                                        1.0F,  0.0F, -1.0F
        };
        float[] sobel_v_data        = { -1.0F,  -2.0F, -1.0F,
                                         0.0F,   0.0F,  0.0F,
                                         1.0F,   2.0F,  1.0F
        };

        float[] roberts_h_data        = { 0.0F,  0.0F, -1.0F,
                                          0.0F,  1.0F,  0.0F,
                                          0.0F,  0.0F,  0.0F
        };
        float[] roberts_v_data        = { -1.0F,  0.0F, 0.0F,
                                           0.0F,  1.0F, 0.0F,
                                           0.0F,  0.0F, 0.0F
        };

        float[] prewitt_h_data        = { 1.0F,  0.0F, -1.0F,
                                          1.0F,  0.0F, -1.0F,
                                          1.0F,  0.0F, -1.0F
        };
        float[] prewitt_v_data        = { -1.0F, -1.0F, -1.0F,
                                           0.0F,  0.0F,  0.0F,
                                           1.0F,  1.0F,  1.0F
        };

        float[] freichen_h_data        = { 1.0F,   0.0F, -1.0F,
                                           1.414F, 0.0F, -1.414F,
                                           1.0F,   0.0F, -1.0F
        };
        float[] freichen_v_data        = { -1.0F,  -1.414F, -1.0F,
                                            0.0F,   0.0F,    0.0F,
                                            1.0F,   1.414F,  1.0F
        };

        kernels[0] = new KernelJAI(1, 1, normal_h_data);
        kernels[1] = new KernelJAI(1, 1, normal_v_data);
        kernels[2] = new KernelJAI(3, 3, sobel_h_data);
        kernels[3] = new KernelJAI(3, 3, sobel_v_data);
        kernels[4] = new KernelJAI(3, 3, roberts_h_data);
        kernels[5] = new KernelJAI(3, 3, roberts_v_data);
        kernels[6] = new KernelJAI(3, 3, prewitt_h_data);
        kernels[7] = new KernelJAI(3, 3, prewitt_v_data);
        kernels[8] = new KernelJAI(3, 3, freichen_h_data);
        kernels[9] = new KernelJAI(3, 3, freichen_v_data);
        kern_h = kernels[0];
        kern_v = kernels[1];
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);
        
        // Gradient operation
        if ((kern_h == null) || (kern_v == null)) {
            initKernels();
        }
        ParameterBlock paramBlock = new ParameterBlock();
        paramBlock.addSource(im);
        paramBlock.add(kern_h);
        paramBlock.add(kern_v);
        return JAI.create("gradientmagnitude", paramBlock, renderHints);
    }

    public void startAnimation() {
    }

    public void animate() {
        try {
            int current = kernelBox.getSelectedIndex() + 1;

            if ( current >= kernelLabels.length ) {
                current = 0;
            }

            kernelBox.setSelectedIndex(current);
            Thread.sleep(1000);
        } catch( InterruptedException e ) {
        }
    }

    public void reset() {
        kernelBox.setSelectedIndex(0);
        kern_h = kernels[0];
        kern_v = kernels[1];
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.DESELECTED) {
            return;
        }

        String item = (String)e.getItem();
        for (int i = 0; i < kernelLabels.length; i++) {
            if (item.equals(kernelLabels[i])) {
                // Set the appropriate kernel and do Gradient
                int tmp = i * 2;
                kern_h = kernels[tmp];
                kern_v = kernels[tmp+1];
                break;
            }
        }
        repaint();
    }
}
