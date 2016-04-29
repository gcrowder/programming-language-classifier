//
//  Localizer.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app;

import java.awt.*;
import java.awt.image.BufferedImage;

import javax.swing.*;

/**
 * Placeholder class for localization support. Also useful for changing keyboard
 * shortcuts or menu names based on platform as well as language (Meta-W vs. Alt-F4
 * for close, Meta-R vs. F5 for refresh, etc). This class should be expanded to load
 * localized resources via ResourceBundles or other mechanisms of your choosing.
 * 
 * @author ___USERNAME___
 */
public class Localizer {
	final ApplicationController controller;
	final boolean isMac;
	
	public Localizer(final ApplicationController controller) {
		this.controller = controller;
		
		final String os = System.getProperty("os.name");
		isMac = os.startsWith("Mac OS X");
	}
	
	public String getText(final String key, final String fallback) {
		// get regionalized strings here, returning fallbacks for this example
		return fallback;
	}
	
	public KeyStroke getKeyStroke(final String key, final String fallback) {
		// get platform and region-specific keystrokes here, returning platform-split fallbacks for this example
		if (fallback == null) return null;
		final String prefix = isMac ? "meta" : "control";
		return KeyStroke.getKeyStroke(prefix + " " + fallback);
	}

	public Image getIcon(final String key) {
		// for this example, only returning an icon for the main window, and a blank icon on Windows
		if (!"mainwindow.icon".equals(key)) return null;
		if (!isMac) return new BufferedImage(32, 32, BufferedImage.TYPE_INT_ARGB);
		
		// using NSImage:// syntax to access AppKit named images
		// see the named images section for more constants at
		// http://developer.apple.com/documentation/Cocoa/Reference/ApplicationKit/Classes/nsimage_Class/Reference/Reference.html
		return Toolkit.getDefaultToolkit().getImage("NSImage://NSEveryone");
	}
	
	/*
	 * helper action class to make creating fully localized actions easier
	 * see usage in Actions.java
	 */
	public abstract class LocalizedAction extends AbstractAction {
		public LocalizedAction(final String key, final String text, final String shortcut) {
			putValue(Action.NAME, getText(key + ".text", text));
			if (shortcut != null) {
				putValue(Action.ACCELERATOR_KEY, getKeyStroke(key + ".keystroke", shortcut));
			}
			putValue(Action.SMALL_ICON, getIcon(key + ".icon"));
		}
	}
}
