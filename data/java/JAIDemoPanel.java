/**
 * @(#)JAIDemoPanel.java	15.2 03/05/20
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
import java.awt.image.RenderedImage;
import java.awt.event.*;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;

class AutoThread extends Thread {
    
    JAIDemoPanel panel;
    boolean suspended = false;
    boolean stopped = false;

    public AutoThread(JAIDemoPanel panel) {
        this.panel = panel;
    }
    
    public void run() {
        while (!stopped) {
            if (!suspended) {
                panel.animate();
            }
            yield();
            try {
                sleep(10);
            } catch (InterruptedException e) {
            }
        }
    }

    public void mySuspend() {
        suspended = true;
    }

    public void myResume() {
        suspended = false;
    }

    public void myStop() {
        suspended = true;
        stopped = true;
    }
}


public abstract class JAIDemoPanel extends JPanel implements ActionListener {

    Vector sourceVec = null;
    Icon imageIcon = null;
    JLabel imageLabel = null;
    
    boolean autoMode = false;
    AutoThread autoThread = null;
    
    /** Rendering hints to be used by subclasses. */


    protected RenderingHints renderHints = null;
    
    public JAIDemoPanel(Vector sourceVec) {
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
        imageIcon = new IconJAI(process());
        imageLabel = new JLabel(imageIcon);
        JScrollPane scrollPane =
            new JScrollPane(imageLabel,
                           ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                           ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        setLayout(new BorderLayout());
        add("Center", scrollPane);

        JPanel controlPanel = new JPanel();
        controlPanel.setLayout(new BorderLayout());

        JButton resetButton = new JButton("Reset");
        resetButton.addActionListener(this);

        JToggleButton autoButton = new JToggleButton("Automatic");
        autoButton.addActionListener(this);
        if (!supportsAutomatic()) {
            autoButton.setEnabled(false);
        }

        JPanel controlButtonPanel = new JPanel();
        controlButtonPanel.setLayout(new GridLayout(1, 2));
        controlButtonPanel.add(resetButton);
        controlButtonPanel.add(autoButton);

        controlPanel.add("West", controlButtonPanel);

        JPanel customPanel = new JPanel();
        controlPanel.add("Center", customPanel);
        makeControls(customPanel);

        add("South", controlPanel);
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

    /**
     * Repaints the image panel.  The process() method is called
     * to generate a new output image, which is then wrapped by an
     * ImageIcon and set as the icon of the JLabel imageLabel.
     */


    public void repaint() {
        // Beware of a race condition when Swing tries to repaint
        // before we've finished initialization.
        if (sourceVec == null) {
            return;
        }

        if(imageIcon instanceof IconJAI) {
            RenderedImage previousImage  = ((IconJAI)imageIcon).getImage();
            if(previousImage instanceof PlanarImage) {
                ((PlanarImage)previousImage).dispose();
            }
        }

        imageIcon = new IconJAI(process());
        imageLabel.setIcon(imageIcon);
        Dimension iconSize = new Dimension(imageIcon.getIconWidth(),
                                           imageIcon.getIconHeight());
        imageLabel.setPreferredSize(iconSize);
        imageLabel.revalidate();
    }

    /** Creates a control panel that will affect the operation parameters. */


    public void makeControls(JPanel controls) {
    }

    /** Default method when any action is performed. */


    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        if (command.equals("Reset")) {
            reset();
            repaint();
        } else if (command.equals("Automatic")) {
            autoMode = !autoMode;
            if (autoMode) {
                autoThread = new AutoThread(this);
                autoThread.start();
            } else {
                autoThread.interrupt();
                autoThread.myStop();
                autoThread = null;
            }
        }
    }

    /** Returns the name of the sub-demo. */


    public abstract String getDemoName();

    /** Returns true if the sub-demo supports auto mode. */


    public boolean supportsAutomatic() {
        return false;
    }

    public void startAnimation() {
    }

    public void animate() {
    }

    public void activate() {
        if (autoMode) {
            autoThread = new AutoThread(this);
            autoThread.start();
        }
    }

    public void deactivate() {
        if (autoMode) {
            autoThread.interrupt();
            autoThread.myStop();
            autoThread = null;
        }
    }

    /** Returns the result of processing the source image. */


    public abstract PlanarImage process();

    /** Called when the reset button is pressed. */


    public void reset() {
    }
}
