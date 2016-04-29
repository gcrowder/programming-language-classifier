//
//  PersonSelectionListener.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.addressbook;

import java.util.*;

/**
 * Parties interested in knowing when people are selected from the
 * AddressBookControler should implement this interface, and add themselves
 * to the the controller.
 * 
 * @author ___USERNAME___
 */
public interface PersonSelectionListener {
	public void personSelected(final Object source, final Person person);
}

// helper class to multi-cast "person selected" events
class PersonSelectionListenerList implements PersonSelectionListener {
	final List<PersonSelectionListener> listeners = new ArrayList<PersonSelectionListener>();
	Object originalSource;
	Person selectedPerson;
	
	public void addPersonSelectionListener(final PersonSelectionListener listener) {
		listeners.add(listener);
		listener.personSelected(originalSource, selectedPerson);
	}
	
	public void removePersonSelectionListener(final PersonSelectionListener listener) {
		listeners.remove(listener);
	}

	public void personSelected(final Object source, final Person person) {
		this.originalSource = source;
		this.selectedPerson = person;
		
		for (final PersonSelectionListener listener : listeners) {
			listener.personSelected(source, person);
		}
	}
}
