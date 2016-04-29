//
//  MainWindowController.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.mainwindow;

import java.awt.event.*;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import com.example.app.*;
import com.example.app.addressbook.*;

/**
 * Controls the main window which shows the currently selected person.
 * 
 * Unlike most applications, once the main window is closed a quit
 * is requested, and all the controlers are disposed, and the app should
 * exit organically.
 * 
 * Swing will automatically terminate the process if there are no more
 * windows open, no background threads running, and no more events left
 * to process in the event queue.
 * 
 * @author ___USERNAME___
 */
public class MainWindowController {
	final ApplicationController app;
	
	SelectedPersonTableModel selectedPersonTableModel;
	JFrame mainWindow;
	Action closeWindowAction;
	
	public MainWindowController(final ApplicationController appController) {
		this.app = appController;
	}
	
	public void start() {
		selectedPersonTableModel = new SelectedPersonTableModel();
		app.getAddressBookController().addPersonSelectionListener(selectedPersonTableModel);
		
		closeWindowAction = app.getLocalizer().new LocalizedAction("mainwindow.close", "Close", "W") {
			public void actionPerformed(final ActionEvent e) {
				final boolean windowShouldClose = requestMainWindowClose();
				if (!windowShouldClose) return;
				closeWindow();
			}
		};
		
		mainWindow = MainWindowBuilder.createMainWindow(this);
		mainWindow.pack();
		mainWindow.addComponentListener(new ComponentAdapter() {
			public void componentHidden(final ComponentEvent e) {
				app.getActions().quit.actionPerformed(null);
			}
		});
		
		bringMainWindowToFront();
	}
	
	public void bringMainWindowToFront() {
		mainWindow.setVisible(true);
		mainWindow.toFront();
	}
	
	boolean requestMainWindowClose() {
		// should the main window close?

		return true;
	}

	void closeWindow() {
		mainWindow.setVisible(false);
	}
	
	public void dispose() {
		closeWindow();
		mainWindow.dispose();
		app.getAddressBookController().removePersonSelectionListener(selectedPersonTableModel);
	}
	
	// JTable model which listens for the "currently selected person"
	class SelectedPersonTableModel extends DefaultTableModel implements PersonSelectionListener {
		Person selectedPerson;
		
		public void personSelected(final Object source, final Person person) {
			this.selectedPerson = person;
			fireTableDataChanged();
		}
		
		@Override
		public Object getValueAt(final int row, final int column) {
			if (column != 1) {
				final Localizer loc = app.getLocalizer();
				switch (row) {
					case 0: return loc.getText("person.name", "Name");
					case 1: return loc.getText("person.email", "Email");
					case 2: return loc.getText("person.number", "Phone");
					case 3: return loc.getText("person.chat", "Chat");
	//				case 4: return "UID";
				}
			} else if (selectedPerson != null) {
				switch (row) {
					case 0: return selectedPerson.getFullName();
					case 1: return selectedPerson.getEmail();
					case 2: return selectedPerson.getPhone();
					case 3: return selectedPerson.getChat();
	//				case 4: return selectedPerson.getUID();
				}
			}
			return "";
		}
		
		@Override
		public int getColumnCount() {
			return 2;
		}
		
		@Override
		public int getRowCount() {
			return 4;
		}
		
		@Override
		public boolean isCellEditable(final int row, final int column) {
			return false;
		}
	}
}
