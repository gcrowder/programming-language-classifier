/**
 * @(#)JAISourceAdjPanel.java	15.2 03/05/20
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
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;

public abstract class JAISourceAdjPanel extends JPanel implements ActionListener {

    Vector sourceVec = null;
     /** Rendering hints to be used by subclasses. */


    protected RenderingHints renderHints = null;
    
    public JAISourceAdjPanel(Vector sourceVec) {
        this.sourceVec = (Vector)sourceVec.clone();
    }

    /**
     * Performs the standard setup.  Subclasses should call this method
     * immediately after calling super() in their constructors.
     * The main image pane is constructed and placed in a JScrollPane.
     * A control panel with a reset button is also constructed, and
     * the makeControls method of the subclass is called to fill in
     * the custom portion of the panel.
     */


    public void masterSetup() {
     
        setLayout(new BorderLayout());

        JPanel controlPanel = new JPanel();
        controlPanel.setLayout(new BorderLayout());

        JButton resetButton = new JButton("Reset");
        resetButton.addActionListener(this);

        controlPanel.add("West", resetButton);

        JPanel customPanel = new JPanel();
        controlPanel.add("Center", customPanel);
	JPanel adjPanel = new JPanel();
        makeControls(adjPanel);

        add("South", controlPanel);
	add("Center",adjPanel);
    }
    
    /** Returns the current source image. */


    public PlanarImage getSource(int index) {
        return (PlanarImage)sourceVec.elementAt(index);
    }

    /** Sets the source and performs processing. */


    public void setSource(int sourceNum, PlanarImage source) {
        sourceVec.setElementAt(source, sourceNum);
        repaint();
    }
    
    public void setRenderingHints(RenderingHints renderHints) {
        this.renderHints = renderHints;
	repaint();
    }
 

    /** Creates a control panel that will affect the operation parameters. */


    public void makeControls(JPanel controls) {
    }
  
    /**
     * Repaints the image panel.  The process() method is called
     * to generate a new output image, which is then wrapped by an
     * ImageIcon and set as the icon of the JLabel imageLabel.
     */


    public void repaint(){
      if (sourceVec == null) {
	return;
      }
      process();
    }
  
    /** Default method when any action is performed. */


    public void actionPerformed(ActionEvent e) {
        reset();
    }


    /** Returns the result of processing the source image. */


    public abstract PlanarImage process();

    /** Called when the reset button is pressed. */


    public void reset() {
    }
}
