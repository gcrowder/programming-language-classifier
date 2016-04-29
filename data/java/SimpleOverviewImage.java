/**
 * @(#)SimpleOverviewImage.java	15.2 03/05/20
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


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.MouseAdapter;
import java.awt.image.DataBuffer;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.ParameterBlock;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.media.jai.JAI;
import javax.media.jai.ImageLayout;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;

/** SimpleOverviewImage display and event handling class that includes a popup menu for enabling
 *  window leveling and hiding the overview display. This class contains a 
 *  <code>SimpleOverviewImageDisplayPane</code> which manages an overview image contents.
 *
 */



public class SimpleOverviewImage extends JFrame implements ActionListener {

    // Constants
    private static final int MENU_ITEM_INVALID        = -1;
    private static final int MENU_ITEM_WIN_LEVEL      = 0;
    private static final int MENU_ITEM_HIDE_OVERVIEW  = 1;

    private JPopupMenu popupMenu;

    private static final String WIN_LEVEL_STRING = "Adjust Display Intensity";
    private static final String OVERVIEW_STRING  = "Hide Overview";
    private SimpleOverviewImageDisplayPane overviewDisplayPane;

    /** Constructor
     *
     * @param ovwDisplayPane the image display panel containing an overview display
     * @param imagePath the file path of the overview image
     */


    public SimpleOverviewImage(SimpleOverviewImageDisplayPane ovwDisplayPane, String imagePath) {
	super("Overview: " + imagePath);
        PopupListener popupListener = new PopupListener();
        this.addWindowListener(new WindowHandler()); // Add window listener   
        this.overviewDisplayPane = ovwDisplayPane;
        this.overviewDisplayPane.addMouseListener(popupListener);
        this.overviewDisplayPane.mtw.setVisible(false);
	this.getContentPane().add(ovwDisplayPane);
        this.buildPopup();
        this.pack();
        this.setVisible(false);
	this.setResizable(false);
    }

    // Build a popup menu (follow pattern to add new items)
    private void buildPopup() {

        //Create a popup menu for a pane
        popupMenu = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem(WIN_LEVEL_STRING);
        menuItem.addActionListener(this);
        popupMenu.add(menuItem);

        menuItem = new JMenuItem(OVERVIEW_STRING);
        menuItem.addActionListener(this);
        popupMenu.add(menuItem);

    }

    // Handler class for window events
    class WindowHandler extends WindowAdapter {
	// Handler for window closing event
	public void windowClosing(WindowEvent e) {
	    setVisible(false);
	}
    } // class WindowHandler

    // parseAction is a convenience method to compute menu item ids
    private int parseAction(String menuString) {
       if (menuString.startsWith(WIN_LEVEL_STRING)) return MENU_ITEM_WIN_LEVEL;
       if (menuString.startsWith(OVERVIEW_STRING)) return MENU_ITEM_HIDE_OVERVIEW;
       return MENU_ITEM_INVALID;
    } // parseAction

    /** menu callback event handler. 
     *
     * @param e menu event
     */


    public void actionPerformed(ActionEvent e) {
        JMenuItem source = (JMenuItem)(e.getSource());
	switch (parseAction(source.getText())) {

            case MENU_ITEM_WIN_LEVEL:

                // Display a win-level widget for current overview display
                this.overviewDisplayPane.mtw.setVisible(true);
                break;

            case MENU_ITEM_HIDE_OVERVIEW:

                // Hide the overview
                this.setVisible(false);
                break;

        }
    } // actionPerformed

    /** Inner class to include a popup menu */


    class PopupListener extends MouseAdapter {

        public void mousePressed(MouseEvent e) {
            maybeShowPopup(e);
        }

        public void mouseReleased(MouseEvent e) {
            maybeShowPopup(e);
        }

        private void maybeShowPopup(MouseEvent e) {
            if (e.isPopupTrigger())
                popupMenu.show(e.getComponent(), e.getX(), e.getY());
        }
    } // class PopupListener
}

