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

import java.util.Collection;
import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.validation.test.model.Book;
import org.eclipse.emf.ecp.view.validation.test.model.Librarian;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.eclipse.emf.ecp.view.validation.test.model.util.TestValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Library</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.LibraryImpl#getName <em>Name</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.LibraryImpl#getWriters <em>Writers</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.LibraryImpl#getBooks <em>Books</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.LibraryImpl#getLibrarian <em>Librarian</em>}</li>
 * </ul>
 *
 * @generated
 */
public class LibraryImpl extends EObjectImpl implements Library {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getWriters() <em>Writers</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getWriters()
	 * @generated
	 * @ordered
	 */
	protected EList<Writer> writers;

	/**
	 * The cached value of the '{@link #getBooks() <em>Books</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getBooks()
	 * @generated
	 * @ordered
	 */
	protected EList<Book> books;

	/**
	 * The cached value of the '{@link #getLibrarian() <em>Librarian</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getLibrarian()
	 * @generated
	 * @ordered
	 */
	protected Librarian librarian;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected LibraryImpl() {
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
		return TestPackage.Literals.LIBRARY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.LIBRARY__NAME, oldName, name));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<Writer> getWriters() {
		if (writers == null) {
			writers = new EObjectContainmentWithInverseEList<Writer>(Writer.class, this, TestPackage.LIBRARY__WRITERS,
				TestPackage.WRITER__LIBRARY);
		}
		return writers;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<Book> getBooks() {
		if (books == null) {
			books = new EObjectContainmentEList<Book>(Book.class, this, TestPackage.LIBRARY__BOOKS);
		}
		return books;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Librarian getLibrarian() {
		return librarian;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetLibrarian(Librarian newLibrarian, NotificationChain msgs) {
		final Librarian oldLibrarian = librarian;
		librarian = newLibrarian;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.LIBRARY__LIBRARIAN, oldLibrarian, newLibrarian);
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
	public void setLibrarian(Librarian newLibrarian) {
		if (newLibrarian != librarian) {
			NotificationChain msgs = null;
			if (librarian != null) {
				msgs = ((InternalEObject) librarian).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.LIBRARY__LIBRARIAN, null, msgs);
			}
			if (newLibrarian != null) {
				msgs = ((InternalEObject) newLibrarian).eInverseAdd(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.LIBRARY__LIBRARIAN, null, msgs);
			}
			msgs = basicSetLibrarian(newLibrarian, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.LIBRARY__LIBRARIAN, newLibrarian,
				newLibrarian));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	@Override
	public boolean validate(DiagnosticChain diagnostic, Map<Object, Object> context) {
		if (getName() != null && getName().equals("warning")) {
			if (diagnostic != null) {
				diagnostic.add(new BasicDiagnostic(Diagnostic.WARNING,
					TestValidator.DIAGNOSTIC_SOURCE,
					TestValidator.LIBRARY__VALIDATE,
					"Name is warning.",
					new Object[] { this, TestPackage.eINSTANCE.getLibrary_Name()
					}));
			}
			return false;
		}
		return true;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.LIBRARY__WRITERS:
			return ((InternalEList<InternalEObject>) (InternalEList<?>) getWriters()).basicAdd(otherEnd, msgs);
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
		case TestPackage.LIBRARY__WRITERS:
			return ((InternalEList<?>) getWriters()).basicRemove(otherEnd, msgs);
		case TestPackage.LIBRARY__BOOKS:
			return ((InternalEList<?>) getBooks()).basicRemove(otherEnd, msgs);
		case TestPackage.LIBRARY__LIBRARIAN:
			return basicSetLibrarian(null, msgs);
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
		case TestPackage.LIBRARY__NAME:
			return getName();
		case TestPackage.LIBRARY__WRITERS:
			return getWriters();
		case TestPackage.LIBRARY__BOOKS:
			return getBooks();
		case TestPackage.LIBRARY__LIBRARIAN:
			return getLibrarian();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case TestPackage.LIBRARY__NAME:
			setName((String) newValue);
			return;
		case TestPackage.LIBRARY__WRITERS:
			getWriters().clear();
			getWriters().addAll((Collection<? extends Writer>) newValue);
			return;
		case TestPackage.LIBRARY__BOOKS:
			getBooks().clear();
			getBooks().addAll((Collection<? extends Book>) newValue);
			return;
		case TestPackage.LIBRARY__LIBRARIAN:
			setLibrarian((Librarian) newValue);
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
		case TestPackage.LIBRARY__NAME:
			setName(NAME_EDEFAULT);
			return;
		case TestPackage.LIBRARY__WRITERS:
			getWriters().clear();
			return;
		case TestPackage.LIBRARY__BOOKS:
			getBooks().clear();
			return;
		case TestPackage.LIBRARY__LIBRARIAN:
			setLibrarian((Librarian) null);
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
		case TestPackage.LIBRARY__NAME:
			return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
		case TestPackage.LIBRARY__WRITERS:
			return writers != null && !writers.isEmpty();
		case TestPackage.LIBRARY__BOOKS:
			return books != null && !books.isEmpty();
		case TestPackage.LIBRARY__LIBRARIAN:
			return librarian != null;
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
		result.append(" (name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} // LibraryImpl
