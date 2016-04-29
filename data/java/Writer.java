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

import java.util.Date;
import java.util.Map;

import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Writer</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getFirstName <em>First Name</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getLastName <em>Last Name</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getEMail <em>EMail</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBirthDate <em>Birth Date</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBooks <em>Books</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#isPseudonym <em>Pseudonym</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.validation.test.test.Writer#getLibrary <em>Library</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter()
 * @model
 * @generated
 */
public interface Writer extends EObject {
	/**
	 * Returns the value of the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>First Name</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>First Name</em>' attribute.
	 * @see #setFirstName(String)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_FirstName()
	 * @model
	 * @generated
	 */
	String getFirstName();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getFirstName <em>First Name</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>First Name</em>' attribute.
	 * @see #getFirstName()
	 * @generated
	 */
	void setFirstName(String value);

	/**
	 * Returns the value of the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Last Name</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Last Name</em>' attribute.
	 * @see #setLastName(String)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_LastName()
	 * @model
	 * @generated
	 */
	String getLastName();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getLastName <em>Last Name</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Last Name</em>' attribute.
	 * @see #getLastName()
	 * @generated
	 */
	void setLastName(String value);

	/**
	 * Returns the value of the '<em><b>EMail</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>EMail</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>EMail</em>' attribute.
	 * @see #setEMail(String)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_EMail()
	 * @model
	 * @generated
	 */
	String getEMail();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getEMail <em>EMail</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>EMail</em>' attribute.
	 * @see #getEMail()
	 * @generated
	 */
	void setEMail(String value);

	/**
	 * Returns the value of the '<em><b>Birth Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Birth Date</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Birth Date</em>' attribute.
	 * @see #setBirthDate(Date)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_BirthDate()
	 * @model
	 * @generated
	 */
	Date getBirthDate();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBirthDate <em>Birth Date</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Birth Date</em>' attribute.
	 * @see #getBirthDate()
	 * @generated
	 */
	void setBirthDate(Date value);

	/**
	 * Returns the value of the '<em><b>Books</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.validation.test.test.Book}.
	 * It is bidirectional and its opposite is '{@link org.eclipse.emf.ecp.validation.test.test.Book#getWriters
	 * <em>Writers</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Books</em>' reference list isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Books</em>' reference list.
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_Books()
	 * @see org.eclipse.emf.ecp.validation.test.test.Book#getWriters
	 * @model opposite="writers"
	 * @generated
	 */
	EList<Book> getBooks();

	/**
	 * Returns the value of the '<em><b>Pseudonym</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Pseudonym</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Pseudonym</em>' attribute.
	 * @see #setPseudonym(boolean)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_Pseudonym()
	 * @model default="false"
	 * @generated
	 */
	boolean isPseudonym();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#isPseudonym <em>Pseudonym</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Pseudonym</em>' attribute.
	 * @see #isPseudonym()
	 * @generated
	 */
	void setPseudonym(boolean value);

	/**
	 * Returns the value of the '<em><b>Library</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link org.eclipse.emf.ecp.validation.test.test.Library#getWriters
	 * <em>Writers</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Library</em>' container reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Library</em>' container reference.
	 * @see #setLibrary(Library)
	 * @see org.eclipse.emf.ecp.validation.test.test.TestPackage#getWriter_Library()
	 * @see org.eclipse.emf.ecp.validation.test.test.Library#getWriters
	 * @model opposite="writers" transient="false"
	 * @generated
	 */
	Library getLibrary();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getLibrary <em>Library</em>}'
	 * container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Library</em>' container reference.
	 * @see #getLibrary()
	 * @generated
	 */
	void setLibrary(Library value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @model
	 * @generated
	 */
	boolean validate(DiagnosticChain diagnostic, Map<Object, Object> context);

} // Writer
