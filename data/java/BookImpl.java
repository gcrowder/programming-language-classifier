/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 *******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test.model.impl;

import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecp.view.validation.test.model.Book;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.eclipse.emf.ecp.view.validation.test.model.util.TestValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Book</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.BookImpl#getTitle <em>Title</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.BookImpl#getPages <em>Pages</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.BookImpl#getWriters <em>Writers</em>}</li>
 * </ul>
 *
 * @generated
 */
public class BookImpl extends EObjectImpl implements Book {
	/**
	 * The default value of the '{@link #getTitle() <em>Title</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getTitle()
	 * @generated
	 * @ordered
	 */
	protected static final String TITLE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTitle() <em>Title</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getTitle()
	 * @generated
	 * @ordered
	 */
	protected String title = TITLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getPages() <em>Pages</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPages()
	 * @generated
	 * @ordered
	 */
	protected static final int PAGES_EDEFAULT = 100;

	/**
	 * The cached value of the '{@link #getPages() <em>Pages</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPages()
	 * @generated
	 * @ordered
	 */
	protected int pages = PAGES_EDEFAULT;

	/**
	 * The cached value of the '{@link #getWriters() <em>Writers</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getWriters()
	 * @generated
	 * @ordered
	 */
	protected Writer writers;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected BookImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestPackage.Literals.BOOK;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setTitle(String newTitle) {
		final String oldTitle = title;
		title = newTitle;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.BOOK__TITLE, oldTitle, title));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public int getPages() {
		return pages;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setPages(int newPages) {
		final int oldPages = pages;
		pages = newPages;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.BOOK__PAGES, oldPages, pages));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Writer getWriters() {
		if (writers != null && writers.eIsProxy()) {
			final InternalEObject oldWriters = (InternalEObject) writers;
			writers = (Writer) eResolveProxy(oldWriters);
			if (writers != oldWriters) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestPackage.BOOK__WRITERS, oldWriters,
						writers));
				}
			}
		}
		return writers;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Writer basicGetWriters() {
		return writers;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetWriters(Writer newWriters, NotificationChain msgs) {
		final Writer oldWriters = writers;
		writers = newWriters;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.BOOK__WRITERS,
				oldWriters, newWriters);
			if (msgs == null) {
				msgs = notification;
			} else {
				msgs.add(notification);
			}
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setWriters(Writer newWriters) {
		if (newWriters != writers) {
			NotificationChain msgs = null;
			if (writers != null) {
				msgs = ((InternalEObject) writers).eInverseRemove(this, TestPackage.WRITER__BOOKS, Writer.class, msgs);
			}
			if (newWriters != null) {
				msgs = ((InternalEObject) newWriters).eInverseAdd(this, TestPackage.WRITER__BOOKS, Writer.class, msgs);
			}
			msgs = basicSetWriters(newWriters, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.BOOK__WRITERS, newWriters, newWriters));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	// BEGIN COMPLEX CODE
	@Override
	public boolean validate(DiagnosticChain chain, Map<Object, Object> context) {

		boolean wasSuccesful = true;

		// cancel if name is offensive
		if (getTitle() != null && getTitle().equals("Offensive")) {
			if (chain != null) {
				chain.add(new BasicDiagnostic(Diagnostic.CANCEL,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.BOOK__VALIDATE,
					"Title is too offensive to validate",
					new Object[] { this, TestPackage.eINSTANCE.getBook_Title() }));
			}
			wasSuccesful = false;
		}

		// error when no title
		if (getTitle() == null || getTitle().equals("")) {
			if (chain != null) {
				chain.add(new BasicDiagnostic(Diagnostic.ERROR,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.BOOK__VALIDATE,
					"Books need to have a title",
					new Object[] { this, TestPackage.eINSTANCE.getBook_Title() }));
			}
			wasSuccesful = false;
		}

		// warning title equals warning
		if (getTitle() != null && getTitle().equals("Warning")) {
			if (chain != null) {
				chain.add(new BasicDiagnostic(Diagnostic.WARNING,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.BOOK__VALIDATE,
					"Title says warning",
					new Object[] { this, TestPackage.eINSTANCE.getBook_Title() }));
			}
			wasSuccesful = false;
		}

		// warning when no writer
		if (getWriters() == null) {
			if (chain != null) {
				chain.add(new BasicDiagnostic(Diagnostic.WARNING,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.BOOK__VALIDATE,
					"Warning: A book without a writer?",
					new Object[] { this, TestPackage.eINSTANCE.getBook_Writers() }));
			}
			wasSuccesful = false;
		}

		// info when title short
		if (getTitle() != null && getTitle().length() == 1) {
			if (chain != null) {
				chain.add(new BasicDiagnostic(Diagnostic.INFO,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.BOOK__VALIDATE,
					"Very short title.",
					new Object[] { this, TestPackage.eINSTANCE.getBook_Title() }));
			}
			wasSuccesful = false;
		}

		return wasSuccesful;
	}

	// END COMPLEX CODE

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.BOOK__WRITERS:
			if (writers != null) {
				msgs = ((InternalEObject) writers).eInverseRemove(this, TestPackage.WRITER__BOOKS, Writer.class, msgs);
			}
			return basicSetWriters((Writer) otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.BOOK__WRITERS:
			return basicSetWriters(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case TestPackage.BOOK__TITLE:
			return getTitle();
		case TestPackage.BOOK__PAGES:
			return getPages();
		case TestPackage.BOOK__WRITERS:
			if (resolve) {
				return getWriters();
			}
			return basicGetWriters();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case TestPackage.BOOK__TITLE:
			setTitle((String) newValue);
			return;
		case TestPackage.BOOK__PAGES:
			setPages((Integer) newValue);
			return;
		case TestPackage.BOOK__WRITERS:
			setWriters((Writer) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case TestPackage.BOOK__TITLE:
			setTitle(TITLE_EDEFAULT);
			return;
		case TestPackage.BOOK__PAGES:
			setPages(PAGES_EDEFAULT);
			return;
		case TestPackage.BOOK__WRITERS:
			setWriters((Writer) null);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case TestPackage.BOOK__TITLE:
			return TITLE_EDEFAULT == null ? title != null : !TITLE_EDEFAULT.equals(title);
		case TestPackage.BOOK__PAGES:
			return pages != PAGES_EDEFAULT;
		case TestPackage.BOOK__WRITERS:
			return writers != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) {
			return super.toString();
		}

		final StringBuffer result = new StringBuffer(super.toString());
		result.append(" (title: ");
		result.append(title);
		result.append(", pages: ");
		result.append(pages);
		result.append(')');
		return result.toString();
	}

} // BookImpl
