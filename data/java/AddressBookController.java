//
//  AddressBookController.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.addressbook;

import java.awt.Window;
import java.util.*;

import javax.swing.table.DefaultTableModel;

import com.example.app.ApplicationController;

/**
 * Controls the "currently selected person" and the palette window which
 * the user changes it with. Creates the native address book, fetches people
 * from it, sorts them, filters them, and presents them.
 * 
 * @author ___USERNAME___
 */
public class AddressBookController {
	final ApplicationController app;
	final PersonSelectionListenerList personSelectionListeners = new PersonSelectionListenerList();
	final List<Person> people = new ArrayList<Person>();
	
	NativeAddressBook addressBook;
	Window palette;
	
	PeopleTableModel model;
	Comparator<Person> sort = Person.sortByFirst;
	String filter;

	public AddressBookController(final ApplicationController appController) {
		this.app = appController;
	}
	
	public void start() {
		addressBook = new NativeAddressBook();
		model = new PeopleTableModel();
		
		final Person me = addressBook.getMe();
		personSelectionListeners.personSelected(this, me);
	}

	public void dispose() {
		if (palette == null) return;
		palette.setVisible(false);
		palette.dispose();
	}
	
	public void showAddressBookPalette() {
		if (palette == null) {
			palette = AddressBookPaletteBuilder.createPaletteWindow(this);
			palette.setLocationRelativeTo(null); // center palette
			
			updatePeopleList();
		}
		
		palette.setVisible(true);
		palette.toFront();
	}
	
	void hideAddressBookWindow() {
		if (palette == null) return;
		palette.setVisible(false);
	}

	public void addPersonSelectionListener(final PersonSelectionListener listener) {
		personSelectionListeners.addPersonSelectionListener(listener);
	}

	public void removePersonSelectionListener(final PersonSelectionListener listener) {
		personSelectionListeners.removePersonSelectionListener(listener);
	}

	Comparator<Person> getSort() {
		return sort;
	}
	
	void setSort(final Comparator<Person> sort) {
		this.sort = sort;
		updatePeopleList();
	}
	
	void setFilter(final String text) {
		filter = text.toLowerCase(); // filtering all lower case
		updatePeopleList();
	}
	
	void updatePeopleList() {
		people.clear();
		final List<Person> everyone = addressBook.getEveryone();
		
		if (filter == null) {
			people.addAll(everyone);
		} else {
			for (final Person person : everyone) {
				final String text = person.getFullSearchText();
				if (text.indexOf(filter) != -1) {
					people.add(person);
				}
			}
		}
		
		Collections.sort(people, sort);
		model.fireTableDataChanged();
	}
	
	class PeopleTableModel extends DefaultTableModel {
		@Override
		public int getColumnCount() {
			return 3;
		}
		
		@Override
		public int getRowCount() {
			return people.size();
		}
		
		@Override
		public String getColumnName(final int col) {
			switch (col) {
				case 0: return app.getLocalizer().getText("address.palette.column.name", "Name");
				case 1: return app.getLocalizer().getText("address.palette.column.email", "Email");
				case 2: return app.getLocalizer().getText("address.palette.column.chat", "Chat");
			}
			return "";
		}
		
		@Override
		public Object getValueAt(final int row, final int col) {
			final Person person = people.get(row);
			switch (col) {
				case 0: return person.getFullName();
				case 1: return person.getEmail();
				case 2: return person.getChat();
			}
			return "";
		}
		
		@Override
		public boolean isCellEditable(final int row, final int column) {
			return false;
		}
	}
}
