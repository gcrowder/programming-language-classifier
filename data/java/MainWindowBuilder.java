//
//  MainWindowBuilder.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.mainwindow;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.*;

import com.example.app.*;

/**
 * Creates a primary window to show the current contact and a button
 * to open the AddressBook palette. Does not need to subclass JFrame,
 * or any other JComponents because it only constructs the UI, and 
 * does not need to override any default Swing behaviors.
 * 
 * @author ___USERNAME___
 */
public class MainWindowBuilder {
	public static JFrame createMainWindow(final MainWindowController controller) {
		final JFrame frame = new JFrame();
		frame.setTitle(controller.app.getLocalizer().getText("mainwindow.title", "Example Java App"));
		
		// need to request to close from the controller
		frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(final WindowEvent e) {
				controller.closeWindowAction.actionPerformed(new ActionEvent(e.getSource(), ActionEvent.ACTION_PERFORMED, null));
			}
		});
		
		// add the contents to the frame
		frame.setJMenuBar(createMenuBar(controller));
		frame.add(createToolbar(controller));
		frame.add(createContentPanel(controller));
		
		return frame;
	}
	
	static JMenuBar createMenuBar(final MainWindowController controller) {
		return MenuBarBuilder.createMenuBar(controller.app, controller.closeWindowAction);
	}
	
	static JToolBar createToolbar(final MainWindowController controller) {
		final JToolBar toolBar = new JToolBar();
		
		// add tool bar items here
		
		return toolBar;
	}
	
	static JPanel createContentPanel(final MainWindowController controller) {
		final JPanel panel = new JPanel(new BorderLayout());
		panel.setBorder(new EmptyBorder(6, 6, 6, 6));
		
		panel.add(createAddressPanel(controller), BorderLayout.CENTER);
		panel.add(createAddressBrowserButton(controller), BorderLayout.SOUTH);
		return panel;
	}
	
	static JPanel createAddressPanel(final MainWindowController controller) {
		final JPanel panel = new JPanel(new BorderLayout());
		panel.setBorder(BorderFactory.createTitledBorder(controller.app.getLocalizer().getText("mainwindow.selectedperson.label", "Selected Person")));
		
		// make a JTable that looks like part of the window background
		// since it does not go into a JScrollPane, it doesn't get any headers
		final JTable personTable = new JTable(controller.selectedPersonTableModel);
		personTable.setIntercellSpacing(new Dimension(15, 1));
		personTable.setBackground(new Color(0, 0, 0, 0)); // clear
		personTable.setOpaque(false); // force full repaint
		personTable.setShowGrid(false);
		personTable.setEnabled(false); // disallow focusing into cells
		
		final TableColumnModel columnModel = personTable.getTableHeader().getColumnModel();
		
		// right align the label column by subclassing the default renderer
		columnModel.getColumn(0).setCellRenderer(new DefaultTableCellRenderer() {
			final Font boldFont = personTable.getFont().deriveFont(Font.BOLD);
			@Override
			public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
				setHorizontalAlignment(SwingConstants.TRAILING);
				final Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				c.setFont(boldFont); // have to set font after calling super, since that resets the font
				return c;
			}
		});
		
		// make the value column wider
		columnModel.getColumn(1).setMinWidth(210);
		
		panel.add(personTable, BorderLayout.CENTER);
		panel.add(createDecorationIcon(controller), BorderLayout.WEST);
		
		return panel;
	}
	
	static JLabel createDecorationIcon(final MainWindowController controller) {
		final Image image = controller.app.getLocalizer().getIcon("mainwindow.icon");
		
		final JLabel label = new JLabel(new ImageIcon(image));
		label.setBorder(new EmptyBorder(16, 16, 16, 16));
		label.setVerticalAlignment(SwingConstants.TOP);
		return label;
	}

	static JPanel createAddressBrowserButton(final MainWindowController controller) {
		final JPanel panel = new JPanel(new BorderLayout());
		
		// create the toggle button
		final Localizer loc = controller.app.getLocalizer();
		final JButton addressBookPaletteButton = new JButton(loc.new LocalizedAction("mainwindow.picker", "Show Contacts", null) {
			public void actionPerformed(final ActionEvent e) {
				controller.app.getAddressBookController().showAddressBookPalette();
			}
		});
		panel.add(addressBookPaletteButton, BorderLayout.EAST);
		
		// invoking later, because the new button isn't actually inside its
		// top level window until this function and its callers return
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				// makes button default "return" key responder, adds a pulsing effect in Aqua
				SwingUtilities.getRootPane(addressBookPaletteButton).setDefaultButton(addressBookPaletteButton);
			}
		});
		
		return panel;
	}
}
