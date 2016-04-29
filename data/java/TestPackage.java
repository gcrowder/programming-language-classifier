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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.validation.test.test.TestFactory
 * @model kind="package"
 * @generated
 */
public interface TestPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNAME = "test";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_URI = "hhtp://www.eclipse.org/emf/ecp/validation/test/model";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_PREFIX = "org.eclipse.emf.ecp.validation.test.model";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	TestPackage eINSTANCE = org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.LibraryImpl <em>Library</em>}'
	 * class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.LibraryImpl
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getLibrary()
	 * @generated
	 */
	int LIBRARY = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int LIBRARY__NAME = 0;

	/**
	 * The feature id for the '<em><b>Writers</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int LIBRARY__WRITERS = 1;

	/**
	 * The feature id for the '<em><b>Books</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int LIBRARY__BOOKS = 2;

	/**
	 * The number of structural features of the '<em>Library</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int LIBRARY_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.WriterImpl <em>Writer</em>}'
	 * class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.WriterImpl
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getWriter()
	 * @generated
	 */
	int WRITER = 1;

	/**
	 * The feature id for the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__FIRST_NAME = 0;

	/**
	 * The feature id for the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__LAST_NAME = 1;

	/**
	 * The feature id for the '<em><b>EMail</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__EMAIL = 2;

	/**
	 * The feature id for the '<em><b>Birth Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__BIRTH_DATE = 3;

	/**
	 * The feature id for the '<em><b>Books</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__BOOKS = 4;

	/**
	 * The feature id for the '<em><b>Pseudonym</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__PSEUDONYM = 5;

	/**
	 * The feature id for the '<em><b>Library</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER__LIBRARY = 6;

	/**
	 * The number of structural features of the '<em>Writer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int WRITER_FEATURE_COUNT = 7;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.BookImpl <em>Book</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.BookImpl
	 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getBook()
	 * @generated
	 */
	int BOOK = 2;

	/**
	 * The feature id for the '<em><b>Title</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int BOOK__TITLE = 0;

	/**
	 * The feature id for the '<em><b>Pages</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int BOOK__PAGES = 1;

	/**
	 * The feature id for the '<em><b>Writers</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int BOOK__WRITERS = 2;

	/**
	 * The number of structural features of the '<em>Book</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int BOOK_FEATURE_COUNT = 3;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.validation.test.test.Library <em>Library</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Library</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Library
	 * @generated
	 */
	EClass getLibrary();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Library#getName
	 * <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Library#getName()
	 * @see #getLibrary()
	 * @generated
	 */
	EAttribute getLibrary_Name();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.validation.test.test.Library#getWriters <em>Writers</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Writers</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Library#getWriters()
	 * @see #getLibrary()
	 * @generated
	 */
	EReference getLibrary_Writers();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.validation.test.test.Library#getBooks <em>Books</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Books</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Library#getBooks()
	 * @see #getLibrary()
	 * @generated
	 */
	EReference getLibrary_Books();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.validation.test.test.Writer <em>Writer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Writer</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer
	 * @generated
	 */
	EClass getWriter();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getFirstName
	 * <em>First Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>First Name</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getFirstName()
	 * @see #getWriter()
	 * @generated
	 */
	EAttribute getWriter_FirstName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getLastName
	 * <em>Last Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Last Name</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getLastName()
	 * @see #getWriter()
	 * @generated
	 */
	EAttribute getWriter_LastName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getEMail
	 * <em>EMail</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>EMail</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getEMail()
	 * @see #getWriter()
	 * @generated
	 */
	EAttribute getWriter_EMail();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBirthDate
	 * <em>Birth Date</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Birth Date</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getBirthDate()
	 * @see #getWriter()
	 * @generated
	 */
	EAttribute getWriter_BirthDate();

	/**
	 * Returns the meta object for the reference list '{@link org.eclipse.emf.ecp.validation.test.test.Writer#getBooks
	 * <em>Books</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the reference list '<em>Books</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getBooks()
	 * @see #getWriter()
	 * @generated
	 */
	EReference getWriter_Books();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Writer#isPseudonym
	 * <em>Pseudonym</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Pseudonym</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#isPseudonym()
	 * @see #getWriter()
	 * @generated
	 */
	EAttribute getWriter_Pseudonym();

	/**
	 * Returns the meta object for the container reference '
	 * {@link org.eclipse.emf.ecp.validation.test.test.Writer#getLibrary <em>Library</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the container reference '<em>Library</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Writer#getLibrary()
	 * @see #getWriter()
	 * @generated
	 */
	EReference getWriter_Library();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.validation.test.test.Book <em>Book</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Book</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Book
	 * @generated
	 */
	EClass getBook();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Book#getTitle
	 * <em>Title</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Title</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Book#getTitle()
	 * @see #getBook()
	 * @generated
	 */
	EAttribute getBook_Title();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.validation.test.test.Book#getPages
	 * <em>Pages</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Pages</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Book#getPages()
	 * @see #getBook()
	 * @generated
	 */
	EAttribute getBook_Pages();

	/**
	 * Returns the meta object for the reference '{@link org.eclipse.emf.ecp.validation.test.test.Book#getWriters
	 * <em>Writers</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the reference '<em>Writers</em>'.
	 * @see org.eclipse.emf.ecp.validation.test.test.Book#getWriters()
	 * @see #getBook()
	 * @generated
	 */
	EReference getBook_Writers();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	TestFactory getTestFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.LibraryImpl
		 * <em>Library</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.LibraryImpl
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getLibrary()
		 * @generated
		 */
		EClass LIBRARY = eINSTANCE.getLibrary();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute LIBRARY__NAME = eINSTANCE.getLibrary_Name();

		/**
		 * The meta object literal for the '<em><b>Writers</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference LIBRARY__WRITERS = eINSTANCE.getLibrary_Writers();

		/**
		 * The meta object literal for the '<em><b>Books</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference LIBRARY__BOOKS = eINSTANCE.getLibrary_Books();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.WriterImpl
		 * <em>Writer</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.WriterImpl
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getWriter()
		 * @generated
		 */
		EClass WRITER = eINSTANCE.getWriter();

		/**
		 * The meta object literal for the '<em><b>First Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute WRITER__FIRST_NAME = eINSTANCE.getWriter_FirstName();

		/**
		 * The meta object literal for the '<em><b>Last Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute WRITER__LAST_NAME = eINSTANCE.getWriter_LastName();

		/**
		 * The meta object literal for the '<em><b>EMail</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute WRITER__EMAIL = eINSTANCE.getWriter_EMail();

		/**
		 * The meta object literal for the '<em><b>Birth Date</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute WRITER__BIRTH_DATE = eINSTANCE.getWriter_BirthDate();

		/**
		 * The meta object literal for the '<em><b>Books</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference WRITER__BOOKS = eINSTANCE.getWriter_Books();

		/**
		 * The meta object literal for the '<em><b>Pseudonym</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute WRITER__PSEUDONYM = eINSTANCE.getWriter_Pseudonym();

		/**
		 * The meta object literal for the '<em><b>Library</b></em>' container reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference WRITER__LIBRARY = eINSTANCE.getWriter_Library();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.validation.test.test.impl.BookImpl <em>Book</em>}
		 * ' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.BookImpl
		 * @see org.eclipse.emf.ecp.validation.test.test.impl.TestPackageImpl#getBook()
		 * @generated
		 */
		EClass BOOK = eINSTANCE.getBook();

		/**
		 * The meta object literal for the '<em><b>Title</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute BOOK__TITLE = eINSTANCE.getBook_Title();

		/**
		 * The meta object literal for the '<em><b>Pages</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute BOOK__PAGES = eINSTANCE.getBook_Pages();

		/**
		 * The meta object literal for the '<em><b>Writers</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference BOOK__WRITERS = eINSTANCE.getBook_Writers();

	}

} // TestPackage
