//
//  ApplicationController.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app;

import java.io.File;

import javax.swing.UIManager;
import javax.swing.border.Border;

import com.example.app.addressbook.AddressBookController;
import com.example.app.mainwindow.MainWindowController;

/**
 * Central controller of the application, holds shared resources, and is
 * the arbiter of what the app can and cannot do at any moment.
 * 
 * This example delegates to two controllers: one for the main window,
 * and the other for the address book palette window. These two windows
 * share localized content, common actions, and similar menu bars.
 * 
 * Larger applications should delegate to sub-controllers which manage
 * more specific resources and its related UI. Document-based apps
 * should create a new sub-controller for each open document that manages
 * the document life cycle (unsaved changes, data model, toolbars, etc).
 * 
 * @author ___USERNAME___
 */
public class ApplicationController {
	Localizer localization;
	Actions actions;
	
	MainWindowController mainWindowController;
	AddressBookController addressBookController;
	
	// called on the main() thread
	public void init() {
		localization = new Localizer(this);
		actions = new Actions(this);
		addressBookController = new AddressBookController(this);
		mainWindowController = new MainWindowController(this);
	}
	
	// called on the Swing Event Dispatch Thread (EDT)
	public void start(final String[] args) {
		initUIDefaults();
		
		actions.start();
		addressBookController.start();
		mainWindowController.start();
	}
	
	void initUIDefaults() {
		// install Mac-specific menu and event handlers
		OSXAppAdapter.installAdapterForControler(this);
		
		// make all new titled borders Aqua-style
		final Border aquaBorder = UIManager.getBorder("TitledBorder.aquaVariant");
		if (aquaBorder != null) UIManager.put("TitledBorder.border", aquaBorder);
	}

	public Localizer getLocalizer() {
		return localization;
	}
	
	public Actions getActions() {
		return actions;
	}
	
	public AddressBookController getAddressBookController() {
		return addressBookController;
	}
	
	void openFile(final File file) {
		// open file here, for a document based application
	}

	void showAboutBox() {
		// show an about window
	}

	void showPreferences() {
		// show a preferences window
	}
	
	void showHelp() {
		// show a help window
	}
	
	boolean requestQuit() {
		// determine if now is a good time to quit
		
		// Document-based apps should poll the controllers for all
		// open documents, to see if they have unsaved changed, and put
		// up the appropriate prompts.
		
		return true;
	}

	void doQuit() {
		// closing and disposing the last frame causes the app to exit organically
		addressBookController.dispose();
		mainWindowController.dispose();
		actions.dispose();
	}
}
