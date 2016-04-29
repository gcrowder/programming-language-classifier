//
//  Person.java
//  ___PROJECTNAME___
//
//  Created by ___FULLUSERNAME___ on ___DATE___.
//  Copyright (c) ___YEAR___ ___ORGANIZATIONNAME___. All rights reserved.
//

package com.example.app.addressbook;

import java.util.*;

/**
 * Data model class for everyone records vended from the Mac OS X address book.
 * 
 * Decodes the data structures returned from JNI, by simply selecting the
 * first string value it finds. More sophisticated models that return lists
 * of email, chat, and phone numbers are an exercise left to the reader.
 * 
 * This model stores its values in a HashMap cache instead of instance variables
 * for sake of clarity and terseness. It is not thread-safe, and is currently only
 * used from the Swing event dispatch thread (EDT).
 * 
 * @author ___USERNAME___
 */
public class Person {
	static final String
		FIRST = "First", MIDDLE = "Middle", LAST = "Last",
		EMAIL = "Email", PHONE = "Phone", CHAT = "AIMInstant",
		UID = "UID", FULL_NAME = "@FullName@", SEARCH_TEXT = "@SearchText@";
	
	static final Comparator<Person> sortByFirst = new FirstNameSorter();
	static final Comparator<Person> sortByLast = new LastNameSorter();
	
	final Map<String, ?> rawAttributes;
	final Map<String, String> cache = new HashMap<String, String>();
	
	public Person(final Map<String, ?> rawAttributes) {
		this.rawAttributes = rawAttributes;
	}
	
	// direct objects from the addressbook
	public String getFirstName()	{ return getCached(FIRST); }
	public String getMiddleName()	{ return getCached(MIDDLE); }
	public String getLastName()		{ return getCached(LAST); }
	public String getEmail()		{ return getCached(EMAIL); }
	public String getPhone()		{ return getCached(PHONE); }
	public String getChat()			{ return getCached(CHAT); }
	public String getUID()			{ return getCached(UID); }
	
	private String getCached(final String key) {
		final String cachedValue = cache.get(key);
		if (cachedValue != null) return cachedValue;
		
		final String value = getFirstStringOf(rawAttributes.get(key));
		cache.put(key, value);
		return value;
	}
	
	// digs into collections and pulls out the first String it finds
	private static String getFirstStringOf(final Object object) {
		if (object instanceof String) return (String)object;
		if (object instanceof List<?>) {
			return getFirstStringOf(((List<?>)object).get(0));
		}
		if (object instanceof Map<?,?>) {
			return getFirstStringOf(((Map<?,?>)object).values().iterator().next());
		}
		return "";
	}
	
	// synthesized property
	public String getFullName() {
		final String name = cache.get(FULL_NAME);
		if (name != null) return name;
		
		final StringBuilder fullName = new StringBuilder();
		append(fullName, getFirstName(), false);
		append(fullName, getMiddleName(), true);
		append(fullName, getLastName(), true);
		
		final String newName = fullName.toString();
		cache.put(FULL_NAME, newName);
		return newName;
	}
	
	// synthesized property, returns lowercase text
	public String getFullSearchText() {
		final String fullSearchText = cache.get(SEARCH_TEXT);
		if (fullSearchText != null) return fullSearchText;
		
		final StringBuilder fullText = new StringBuilder(getFullName());
		append(fullText, getEmail(), true);
		append(fullText, getChat(), true);
		
		final String text = fullText.toString().toLowerCase();
		cache.put(SEARCH_TEXT, text);
		return text;
	}
	
	// string building helper
	private static void append(final StringBuilder builder, final String value, final boolean space) {
		if (value == null || "".equals(value)) return;
		if (space) builder.append(' ');
		builder.append(value);
	}
	
	static class FirstNameSorter implements Comparator<Person> {
		public int compare(final Person o1, final Person o2) {
			return o1.getFirstName().compareTo(o2.getFirstName());
		}
	}
	
	static class LastNameSorter implements Comparator<Person> {
		public int compare(final Person o1, final Person o2) {
			return o1.getLastName().compareTo(o2.getLastName());
		}
	}
}
