//
//  NativeAddressBook.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.addressbook;

import java.util.*;

/**
 * Abstraction for the Mac OS X AddressBook API
 * 
 * Creates a copy of contacts from the address book, and vends Person
 * objects. Uses JNI to obtain simple Lists, Maps, Strings, and Number
 * representations of
 * 
 * @author ___USERNAME___
 */
public class NativeAddressBook {
	static {
		// ensure native JNI library is loaded
		System.loadLibrary("AddressBook");
	}
	
	// JNI method which obtains the UID of the "me" card in Address Book.app
	private static native String getMyUID();
	
	// JNI method that fetches the list of everyone as a list of maps, with String keys and either 
	private static native List<Map<String,?>> getNativeAddressBookContacts();
	
	List<Person> everyone;
	
	public List<Person> getEveryone() {
		if (everyone != null) return everyone;
		
		final List<Map<String, ?>> rawContacts = getNativeAddressBookContacts();
		final List<Person> people = new ArrayList<Person>();
		for (final Map<String, ?> rawContact : rawContacts) {
			final Person person = new Person(rawContact);
			if ("".equals(person.getFullName())) continue; // strip out the nameless
			people.add(person);
		}
		
		return everyone = Collections.unmodifiableList(people);
	}
	
	public Person getMe() {
		final String myUID = getMyUID();
		if (myUID == null) return null;
		
		final List<Person> people = getEveryone();
		for (final Person person : people) {
			if (myUID.equals(person.getUID())) return person;
		}
		
		return null;
	}
}
