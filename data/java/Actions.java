//
//  Actions.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.beans.*;
import java.io.File;

import javax.swing.*;
import javax.swing.text.*;

/**
 * Actions can be placed inside of JButtons, JMenus, and also invoked programmatically.
 * Any action which is enabled or disabled will update all of its dependent JComponents.
 * 
 * These Cut, Copy, Paste and Select All actions have special machinery that keeps their enabled
 * state in sync with the current focus owner. This implementation only supports JTextComponents.
 * 
 * Larger applications should likely have Actions that are specific to each
 * of their controllers.
 * 
 * @author ___USERNAME___
 */
public class Actions implements PropertyChangeListener {
	public final Action openFile;
	public final Action about;
	public final Action preferences;
	public final Action quit;
	public final Action help;
	
	public final LocalizedTextAction cut;
	public final LocalizedTextAction copy;
	public final LocalizedTextAction paste;
	public final LocalizedTextAction selectAll;
	
	LocalizedTextAction[] textFieldActions;
	JTextComponent currentTextComponent;
	
	public Actions(final ApplicationController app) {
		final Localizer loc = app.getLocalizer();
		
		openFile = loc.new LocalizedAction("app.open", "Open...", "O") {
			public void actionPerformed(final ActionEvent e) {
				final Frame[] frames = Frame.getFrames();
				if (frames == null || frames.length == 0) return;
				final FileDialog openFileDialog = new FileDialog(frames[0]);
				openFileDialog.setVisible(true);
				final String file = openFileDialog.getFile();
				if (file == null) return;
				app.openFile(new File(file));
			}
		};
		
		// used on non-Mac OS X platforms
		about = loc.new LocalizedAction("app.about", "About...", null) {
			public void actionPerformed(final ActionEvent e) {
				app.showAboutBox();
			}
		};
		
		preferences = loc.new LocalizedAction("app.preferences", "Preferences...", null) {
			public void actionPerformed(final ActionEvent e) {
				app.showPreferences();
			}
		};
		
		quit = loc.new LocalizedAction("app.exit", "Exit", null) {
			public void actionPerformed(final ActionEvent e) {
				final boolean shouldQuit = app.requestQuit();
				if (shouldQuit) app.doQuit();
			}
		};
		
		help = loc.new LocalizedAction("app.help", "Help", "/") {
			public void actionPerformed(final ActionEvent e) {
				app.showHelp();
			}
		};
		
		cut = new LocalizedTextAction("app.cut", "Cut", "X", DefaultEditorKit.cutAction, loc);
		copy = new LocalizedTextAction("app.copy", "Copy", "C", DefaultEditorKit.copyAction, loc);
		paste = new LocalizedTextAction("app.paste", "Paste", "V", DefaultEditorKit.pasteAction, loc);
		selectAll = new LocalizedTextAction("app.selectAll", "Select All", "A", DefaultEditorKit.selectAllAction, loc);
		
		textFieldActions = new LocalizedTextAction[] {
			cut, copy, paste, selectAll
		};
	}
	
	void start() {
		KeyboardFocusManager.getCurrentKeyboardFocusManager().addPropertyChangeListener(this);
	}
	
	void dispose() {
		KeyboardFocusManager.getCurrentKeyboardFocusManager().removePropertyChangeListener(this);
	}

	public void propertyChange(final PropertyChangeEvent evt) {
		// check if this is a focus owner event
		final String changedPropertyName = evt.getPropertyName();
		if (!"permanentFocusOwner".equals(changedPropertyName)) return;
		
		// check if this is a text component
		final Object newFocusOwner = evt.getNewValue();
		if (newFocusOwner instanceof JTextComponent) {
			currentTextComponent = (JTextComponent)newFocusOwner;
			syncTextFieldActionStateToCurrentComponent();
			return;
		}
		
		// if not, disable our actions
		for (final Action textFieldAction : textFieldActions) {
			textFieldAction.setEnabled(false);
		}
		currentTextComponent = null;
	}
	
	// cycle though our text actions, and set them to the enabled state of the actions installed on the current text component
	private void syncTextFieldActionStateToCurrentComponent() {
		final ActionMap actionMap = currentTextComponent.getActionMap();
		for (final LocalizedTextAction textFieldAction : textFieldActions) {
			final Action installedAction = actionMap.get(textFieldAction.delegatedActionID);
			textFieldAction.setEnabled(installedAction == null ? false : installedAction.isEnabled());
		}
	}
	
	// sends the fired ActionEvent to a specific action of the current text component
	void performTextAction(final ActionEvent e, final String actionID) {
		if (currentTextComponent == null) return;
		
		final Action targetAction = currentTextComponent.getActionMap().get(actionID);
		if (targetAction == null) return;
		
		targetAction.actionPerformed(e);
	}
	
	// helper class that relays events to JTextComponents
	class LocalizedTextAction extends Localizer.LocalizedAction {
		String delegatedActionID;
		
		LocalizedTextAction(final String key, final String text, final String shortcut, final String delegatedActionID, final Localizer loc) {
			loc.super(key, text, shortcut);
			this.delegatedActionID = delegatedActionID;
		}
		
		public void actionPerformed(final ActionEvent e) {
			performTextAction(e, delegatedActionID);
		}
	}
}
