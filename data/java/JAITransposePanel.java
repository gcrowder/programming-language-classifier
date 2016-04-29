/**
 * @(#)JAITransposePanel.java	15.2 03/05/20
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
import javax.media.jai.operator.TransposeDescriptor;
import javax.swing.*;
import javax.swing.event.*;

public class JAITransposePanel extends JAIDemoPanel implements ItemListener {

    JComboBox box;
    EnumeratedParameter type = null;
    String[] labels = { "Original Image",
                        "FLIP_VERTICAL",
                        "FLIP_HORIZONTAL",
                        "FLIP_DIAGONAL",
                        "FLIP_ANTIDIAGONAL",
                        "ROTATE_90",
                        "ROTATE_180",
                        "ROTATE_270"
    };
    EnumeratedParameter[] transposeTypes = {
        null,
        TransposeDescriptor.FLIP_VERTICAL,
        TransposeDescriptor.FLIP_HORIZONTAL,
        TransposeDescriptor.FLIP_DIAGONAL,
        TransposeDescriptor.FLIP_ANTIDIAGONAL,
        TransposeDescriptor.ROTATE_90,
        TransposeDescriptor.ROTATE_180,
        TransposeDescriptor.ROTATE_270
    };


    public JAITransposePanel(Vector sourceVec) {
        super(sourceVec);
        masterSetup();
    }

    public String getDemoName() {
        return "Transpose";
    }

    public void makeControls(JPanel controls) {
        box = new JComboBox();
        for (int i = 0; i < labels.length; i++) {
            box.addItem(labels[i]);
        }

        box.addItemListener(this);
        controls.add(box);
    }

    public boolean supportsAutomatic() {
        return true;
    }

    public PlanarImage process() {
        PlanarImage im = getSource(0);

        if (type == null) {
            // Original image
            return im;
        } else {
            // Transpose operation
            ParameterBlock pb = new ParameterBlock();
            pb.addSource(im);
            pb.add(type);
            return JAI.create("transpose", pb, renderHints);
        }
    }

    public void startAnimation() {
    }

    public void animate() {
        int current = box.getSelectedIndex() + 1;

        if ( current >= labels.length ) {
            current = 0;
        }

        box.setSelectedIndex(current);
    }

    public void reset() {
        box.setSelectedIndex(0);
        type = null;
    }

    public void itemStateChanged(ItemEvent e) {
        if (e.getStateChange() == ItemEvent.DESELECTED) {
            return;
        }

        for (int i = 0; i < labels.length; i++) {
            if (e.getItem().equals(labels[i])) {
                type = transposeTypes[i];
                break;
            }
        }
        
        repaint();
    }
}
