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
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.validation.test.test;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Book</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Book#getTitle <em>Title</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Book#getPages <em>Pages</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Book#getWriters <em>Writers</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getBook()
 * @model
 * @generated
 */
public interface Book extends EObject {
	/**
	 * Returns the value of the '<em><b>Title</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Title</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Title</em>' attribute.
	 * @see #setTitle(String)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getBook_Title()
	 * @model
	 * @generated
	 */
	String getTitle();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Book#getTitle <em>Title</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Title</em>' attribute.
	 * @see #getTitle()
	 * @generated
	 */
	void setTitle(String value);

	/**
	 * Returns the value of the '<em><b>Pages</b></em>' attribute.
	 * The default value is <code>"100"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Pages</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Pages</em>' attribute.
	 * @see #setPages(int)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getBook_Pages()
	 * @model default="100"
	 * @generated
	 */
	int getPages();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Book#getPages <em>Pages</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Pages</em>' attribute.
	 * @see #getPages()
	 * @generated
	 */
	void setPages(int value);

	/**
	 * Returns the value of the '<em><b>Writers</b></em>' reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBooks
	 * <em>Books</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Writers</em>' reference isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Writers</em>' reference.
	 * @see #setWriters(Writer)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getBook_Writers()
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getBooks
	 * @model opposite="books"
	 * @generated
	 */
	Writer getWriters();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Book#getWriters <em>Writers</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Writers</em>' reference.
	 * @see #getWriters()
	 * @generated
	 */
	void setWriters(Writer value);

} // Book
