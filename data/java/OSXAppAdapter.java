//
//  OSXAppAdapter.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app;

import java.awt.event.ActionEvent;
import java.beans.*;
import java.io.File;

import com.apple.eawt.*;

/**
 * Helper class for Mac OS X, which integrates the ApplicationController
 * functions with the native About, Preferences, and Quit menus in the
 * Mac OS X application menu. Also hooks up the open-file action.
 * 
 * @author ___USERNAME___
 */
public class OSXAppAdapter {
	static void installAdapterForControler(final ApplicationController app) {
		// if not on a Mac, don't load the EAWT application
		final String os = System.getProperty("os.name");
		if (!os.startsWith("Mac OS X")) return;
		
		final Application macApp = Application.getApplication();
		final Actions actions = app.getActions();
		
		// link the About action enabled state to the EAWT About menu
		macApp.setEnabledAboutMenu(actions.about.isEnabled());
		actions.about.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(final PropertyChangeEvent evt) {
				macApp.setEnabledAboutMenu(actions.about.isEnabled());
			}
		});
		
		// link the Preferences action enabled state to the EAWT Preferences menu
		macApp.setEnabledPreferencesMenu(actions.preferences.isEnabled());
		actions.preferences.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(final PropertyChangeEvent evt) {
				macApp.setEnabledPreferencesMenu(actions.preferences.isEnabled());
			}
		});
		
		// link the EAWT actions to Swing actions
		macApp.addApplicationListener(new ApplicationAdapter() {
			public void handleOpenFile(final ApplicationEvent e) {
				if (!actions.openFile.isEnabled()) return;
				final File file = new File(e.getFilename());
				app.openFile(file);
			}
			
			public void handleQuit(final ApplicationEvent e) {
				e.setHandled(false); // allow the app to quit under its own power
				app.getActions().quit.actionPerformed(createActionEventFrom(e));
			}
			
			public void handleAbout(final ApplicationEvent e) {
				app.getActions().about.actionPerformed(createActionEventFrom(e));
			}
			
			public void handlePreferences(final ApplicationEvent e) {
				app.getActions().preferences.actionPerformed(createActionEventFrom(e));
			}
			
			ActionEvent createActionEventFrom(final ApplicationEvent e) {
				return new ActionEvent(e.getSource(), ActionEvent.ACTION_PERFORMED, null);
			}
		});
	}
}
