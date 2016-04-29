/**
 * @(#)JAIConvolvePanel.java	15.2 03/05/20
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

public class JAIConvolvePanel extends JAIDemoPanel implements ItemListener {

    static final String[] kernelLabels = { "Normal",
                                           "Blur",
                                           "Blur More",
                                           "Sharpen",
                                           "Sharpen More",
                                           "Detect Edges",
                                           "Emboss",
                                           "Gaussian (3x3)",
                                           "Gaussian (5x5)",
                                           "Gaussian (7x7)",
                                           "Gaussian (9x9)",
                                           "Gaussian (11x11)",
                                           "Gaussian (21x21)",
                                           "Gaussian (31x31)",
                                           "Gaussian (41x41)",
                                           "Gaussian (51x51)",
                                           "Gaussian (61x61)",
                                           "Gaussian (71x71)"
    };

    JComboBox kernelBox;

    KernelJAI[] kernels;
    KernelJAI kernel;

    public JAIConvolvePanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Convolve";
    }

    public void makeControls(JPanel controls) {
        kernelBox = new JComboBox();
        for (int i = 0; i < kernelLabels.length; i++) {
            kernelBox.addItem(kernelLabels[i]);
        }

        kernelBox.addItemListener(this);
        controls.add(kernelBox);
    }

    private KernelJAI makeGaussianKernel(int radius) {
        int diameter = 2*radius + 1;
        float invrsq = 1.0F/(radius*radius);

        float[] gaussianData = new float[diameter];

        float sum = 0.0F;
        for (int i = 0; i < diameter; i++) {
            float d = i - radius;
            float val = (float)Math.exp(-d*d*invrsq);
            gaussianData[i] = val;
            sum += val;        
        }

        // Normalize
        float invsum = 1.0F/sum;
        for (int i = 0; i < diameter; i++) {
            gaussianData[i] *= invsum;
        }

        return new KernelJAI(diameter, diameter, radius, radius,
                             gaussianData, gaussianData);
    }

    private void initKernels() {
        kernels = new KernelJAI[kernelLabels.length];

        float[] normalData      = {  1.0F };

        float[] blurData        = {  0.0F,        1.0F/ 8.0F,  0.0F,
                                     1.0F/ 8.0F,  4.0F/ 8.0F,  1.0F/ 8.0F,
                                     0.0F,        1.0F/ 8.0F,  0.0F
        };

        float[] blurMoreData    = {  1.0F/14.0F,  2.0F/14.0F,  1.0F/14.0F,
                                     2.0F/14.0F,  2.0F/14.0F,  2.0F/14.0F,
                                     1.0F/14.0F,  2.0F/14.0F,  1.0F/14.0F
        };

        float[] sharpenData     = {  0.0F,       -1.0F/ 4.0F,  0.0F,
                                    -1.0F/ 4.0F,  8.0F/ 4.0F, -1.0F/ 4.0F,
                                     0.0F,       -1.0F/ 4.0F,  0.0F
        };

        float[] sharpenMoreData = { -1.0F/ 4.0F, -1.0F/ 4.0F, -1.0F/ 4.0F,
                                    -1.0F/ 4.0F, 12.0F/ 4.0F, -1.0F/ 4.0F,
                                    -1.0F/ 4.0F, -1.0F/ 4.0F, -1.0F/ 4.0F
        };

        float[] edgeData =        {  0.0F,       -1.0F,        0.0F,
                                    -1.0F,        4.0F,       -1.0F,
                                     0.0F,       -1.0F,        0.0F
        };

        float[] embossData =      { -5.0F,        0.0F,        0.0F,
                                     0.0F,        1.0F,        0.0F,
                                     0.0F,        0.0F,        5.0F
        };

        kernels[0] = new KernelJAI(1, 1, 0, 0, normalData);
        kernels[1] = new KernelJAI(3, 3, 1, 1, blurData);
        kernels[2] = new KernelJAI(3, 3, 1, 1, blurMoreData);
        kernels[3] = new KernelJAI(3, 3, 1, 1, sharpenData);
        kernels[4] = new KernelJAI(3, 3, 1, 1, sharpenMoreData);
        kernels[5] = new KernelJAI(3, 3, 1, 1, edgeData);
        kernels[6] = new KernelJAI(3, 3, 1, 1, embossData);
        kernels[7] = makeGaussianKernel(1);
        kernels[8] = makeGaussianKernel(2);
        kernels[9] = makeGaussianKernel(3);
        kernels[10] = makeGaussianKernel(4);
        kernels[11] = makeGaussianKernel(5);
        kernels[12] = makeGaussianKernel(10);
        kernels[13] = makeGaussianKernel(15);
        kernels[14] = makeGaussianKernel(20);
        kernels[15] = makeGaussianKernel(25);
        kernels[16] = makeGaussianKernel(30);
        kernels[17] = makeGaussianKernel(35);
        kernel = kernels[0];
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);

        if (kernel == null) {
            initKernels();
        }
        ParameterBlock paramBlock = new ParameterBlock();
        paramBlock.addSource(im);
        paramBlock.add(kernel);
        return JAI.create("convolve", paramBlock, renderHints);
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
            Thread.sleep(500);
        } catch( InterruptedException e ) {
        }
    }

    public void reset() {
        kernelBox.setSelectedIndex(0);
        kernel = kernels[0];
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.DESELECTED) {
            return;
        }

        String item = (String)e.getItem();
        for (int i = 0; i < kernelLabels.length; i++) {
            if (item.equals(kernelLabels[i])) {
                kernel = kernels[i];
                break;
            }
        }
        repaint();
    }
}
