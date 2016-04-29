//
//  AddressBookPaletteBuilder.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.addressbook;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.Comparator;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.DefaultTableCellRenderer;

import com.example.app.*;

/**
 * Creates the UI for the AddressBook palette window.
 * 
 * @author ___USERNAME___
 */
public class AddressBookPaletteBuilder {
	// create the top level window
	public static JFrame createPaletteWindow(final AddressBookController controller) {
		final Localizer loc = controller.app.getLocalizer();
		
		final JFrame palette = new JFrame();
		palette.setTitle(controller.app.getLocalizer().getText("address.palette.title", "Contacts"));
		palette.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		
		// give the window palette behavior on Mac OS X
		palette.getRootPane().putClientProperty("Window.style", "small");
		palette.setAlwaysOnTop(true);
		
		// add the content
		palette.setJMenuBar(createMenuBar(controller));
		palette.add(createToolbar(controller, loc), BorderLayout.NORTH);
		palette.add(createContentPanel(controller, loc), BorderLayout.CENTER);
		
		palette.setSize(360, 420);
		return palette;
	}
	
	private static JMenuBar createMenuBar(final AddressBookController controller) {
		final Action closeAction = controller.app.getLocalizer().new LocalizedAction("address.palette.menu.close", "Close", "W") {
			public void actionPerformed(final ActionEvent e) {
				controller.hideAddressBookWindow();
			}
		};
		
		return MenuBarBuilder.createMenuBar(controller.app, closeAction);
	}

	static JToolBar createToolbar(final AddressBookController controller, final Localizer loc) {
		final JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		
		toolBar.add(createSorters(controller, loc));
		toolBar.add(Box.createGlue());
		toolBar.add(createSearchField(controller));
		
		return toolBar;
	}
	
	// creates the first/last sort order button pair
	static Component createSorters(final AddressBookController controller, final Localizer loc) {
		final JPanel panel = new JPanel(new BorderLayout());
		final ButtonGroup group = new ButtonGroup();
		
		final JToggleButton sortByFirstButton = createSortButton(controller, Person.sortByFirst, "first");
		sortByFirstButton.setText(loc.getText("address.palette.sort.first", "First"));
		panel.add(sortByFirstButton, BorderLayout.WEST);
		group.add(sortByFirstButton);
		
		final JToggleButton sortByLastButton = createSortButton(controller, Person.sortByLast, "last");
		sortByLastButton.setText(loc.getText("address.palette.sort.last", "Last"));
		panel.add(sortByLastButton, BorderLayout.EAST);
		group.add(sortByLastButton);
		
		panel.setMaximumSize(panel.getPreferredSize());
		return panel;
	}
	
	// creates an individual sort button
	static JToggleButton createSortButton(final AddressBookController controller, final Comparator<Person> mySort, final String position) {
		final JToggleButton sortButton = new JToggleButton(new AbstractAction() {
			public void actionPerformed(final ActionEvent e) {
				controller.setSort(mySort);
			}
		});
		sortButton.setSelected(controller.getSort() == mySort);
		sortButton.setFocusable(false);
		
		// gives the buttons a paired appearance in Aqua
		sortButton.putClientProperty("JButton.buttonType", "segmented");
		sortButton.putClientProperty("JButton.segmentPosition", position);
		return sortButton;
	}

	static Component createSearchField(final AddressBookController controller) {
		final JTextField searchField = new JTextField(6);
		searchField.putClientProperty("JTextField.variant", "search"); // makes the textfield round in Aqua
		searchField.setMaximumSize(searchField.getPreferredSize()); // prevents the textfield from stretching
		
		// triggers the "live search" on every modification to the textfield, including
		// copy/paste, complex input events, and not just keystrokes
		searchField.getDocument().addDocumentListener(new DocumentListener() {
			public void changedUpdate(final DocumentEvent e) {
				controller.setFilter(searchField.getText());
			}

			public void insertUpdate(final DocumentEvent e) {
				controller.setFilter(searchField.getText());
			}

			public void removeUpdate(final DocumentEvent e) {
				controller.setFilter(searchField.getText());
			}
		});
		
		return searchField;
	}

	static Component createContentPanel(final AddressBookController controller, final Localizer loc) {
		final JTable addressTable = createAddressTable(controller);
		final JScrollPane scrollPane = new JScrollPane(addressTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scrollPane.setBorder(BorderFactory.createEmptyBorder()); // fits the content right up to the edge of the window
		return scrollPane;
	}
	
	static JTable createAddressTable(final AddressBookController controller) {
		final JTable addressTable = new JTable(controller.model); // hook up the controller's model to this table
		
		// only allow a single row to be selected, and tell all interested parties when the selected person changes 
		addressTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		addressTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(final ListSelectionEvent e) {
				final int index = addressTable.getSelectedRow();
				final Person person = index < 0 ? null : controller.people.get(index);
				controller.personSelectionListeners.personSelected(addressTable, person);
			}
		});
		
		final Font currentFont = addressTable.getFont();
		addressTable.setFont(currentFont.deriveFont(currentFont.getSize2D() - 1.0f));
		addressTable.setShowGrid(false);
		addressTable.setIntercellSpacing(new Dimension(0, 1));
		
		// install a custom renderer indent slightly, and show alternating rows in Aqua
		addressTable.setDefaultRenderer(Object.class, new DefaultTableCellRenderer() {
			Border indent = new EmptyBorder(0, 3, 0, 0);
			Border even = UIManager.getBorder("List.evenRowBackgroundPainter");
			Border odd = UIManager.getBorder("List.oddRowBackgroundPainter");
			Border current;
			
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				current = row % 2 == 1 ? even : odd;
				setBorder(indent);
				return c;
			}
			
			@Override
			protected void paintComponent(Graphics g) {
				// paint the background painter first, then the rest of the component
				if (current != null) current.paintBorder(addressTable, g, 0, 0, getWidth(), getHeight());
				super.paintComponent(g);
			}
		});
		
		return addressTable;
	}
}
