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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecp.view.validation.test.model.Book;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.Content;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent;
import org.eclipse.emf.ecp.view.validation.test.model.Librarian;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.Mainboard;
import org.eclipse.emf.ecp.view.validation.test.model.PowerBlock;
import org.eclipse.emf.ecp.view.validation.test.model.Referencer;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild2;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithoutValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithUnique;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicityConcrete;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 *
 * @generated
 */
public class TestFactoryImpl extends EFactoryImpl implements TestFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static TestFactory init() {
		try {
			final TestFactory theTestFactory = (TestFactory) EPackage.Registry.INSTANCE
				.getEFactory(TestPackage.eNS_URI);
			if (theTestFactory != null) {
				return theTestFactory;
			}
		} catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new TestFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public TestFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
		case TestPackage.LIBRARY:
			return createLibrary();
		case TestPackage.WRITER:
			return createWriter();
		case TestPackage.BOOK:
			return createBook();
		case TestPackage.LIBRARIAN:
			return createLibrarian();
		case TestPackage.COMPUTER:
			return createComputer();
		case TestPackage.MAINBOARD:
			return createMainboard();
		case TestPackage.POWER_BLOCK:
			return createPowerBlock();
		case TestPackage.CONTAINER:
			return createContainer();
		case TestPackage.CONTENT:
			return createContent();
		case TestPackage.TABLE_WITH_MULTIPLICITY:
			return createTableWithMultiplicity();
		case TestPackage.TABLE_CONTENT_WITHOUT_VALIDATION:
			return createTableContentWithoutValidation();
		case TestPackage.TABLE_CONTENT_WITH_VALIDATION:
			return createTableContentWithValidation();
		case TestPackage.TABLE_WITHOUT_MULTIPLICITY:
			return createTableWithoutMultiplicity();
		case TestPackage.TABLE_WITH_UNIQUE:
			return createTableWithUnique();
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD2:
			return createTableContentWithInnerChild2();
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD:
			return createTableContentWithInnerChild();
		case TestPackage.TABLE_WITHOUT_MULTIPLICITY_CONCRETE:
			return createTableWithoutMultiplicityConcrete();
		case TestPackage.REFERENCER:
			return createReferencer();
		case TestPackage.CROSS_REFERENCE_CONTAINER:
			return createCrossReferenceContainer();
		case TestPackage.CROSS_REFERENCE_CONTENT:
			return createCrossReferenceContent();
		default:
			throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Library createLibrary() {
		final LibraryImpl library = new LibraryImpl();
		return library;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Writer createWriter() {
		final WriterImpl writer = new WriterImpl();
		return writer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Book createBook() {
		final BookImpl book = new BookImpl();
		return book;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Librarian createLibrarian() {
		final LibrarianImpl librarian = new LibrarianImpl();
		return librarian;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Computer createComputer() {
		final ComputerImpl computer = new ComputerImpl();
		return computer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Mainboard createMainboard() {
		final MainboardImpl mainboard = new MainboardImpl();
		return mainboard;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public PowerBlock createPowerBlock() {
		final PowerBlockImpl powerBlock = new PowerBlockImpl();
		return powerBlock;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public org.eclipse.emf.ecp.view.validation.test.model.Container createContainer() {
		final ContainerImpl container = new ContainerImpl();
		return container;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Content createContent() {
		final ContentImpl content = new ContentImpl();
		return content;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableWithMultiplicity createTableWithMultiplicity() {
		final TableWithMultiplicityImpl tableWithMultiplicity = new TableWithMultiplicityImpl();
		return tableWithMultiplicity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableContentWithoutValidation createTableContentWithoutValidation() {
		final TableContentWithoutValidationImpl tableContentWithoutValidation = new TableContentWithoutValidationImpl();
		return tableContentWithoutValidation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableContentWithValidation createTableContentWithValidation() {
		final TableContentWithValidationImpl tableContentWithValidation = new TableContentWithValidationImpl();
		return tableContentWithValidation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableWithoutMultiplicity createTableWithoutMultiplicity() {
		final TableWithoutMultiplicityImpl tableWithoutMultiplicity = new TableWithoutMultiplicityImpl();
		return tableWithoutMultiplicity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableWithUnique createTableWithUnique() {
		final TableWithUniqueImpl tableWithUnique = new TableWithUniqueImpl();
		return tableWithUnique;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableContentWithInnerChild2 createTableContentWithInnerChild2() {
		final TableContentWithInnerChild2Impl tableContentWithInnerChild2 = new TableContentWithInnerChild2Impl();
		return tableContentWithInnerChild2;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableContentWithInnerChild createTableContentWithInnerChild() {
		final TableContentWithInnerChildImpl tableContentWithInnerChild = new TableContentWithInnerChildImpl();
		return tableContentWithInnerChild;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableWithoutMultiplicityConcrete createTableWithoutMultiplicityConcrete() {
		final TableWithoutMultiplicityConcreteImpl tableWithoutMultiplicityConcrete = new TableWithoutMultiplicityConcreteImpl();
		return tableWithoutMultiplicityConcrete;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Referencer createReferencer() {
		final ReferencerImpl referencer = new ReferencerImpl();
		return referencer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public CrossReferenceContainer createCrossReferenceContainer() {
		final CrossReferenceContainerImpl crossReferenceContainer = new CrossReferenceContainerImpl();
		return crossReferenceContainer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public CrossReferenceContent createCrossReferenceContent() {
		final CrossReferenceContentImpl crossReferenceContent = new CrossReferenceContentImpl();
		return crossReferenceContent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TestPackage getTestPackage() {
		return (TestPackage) getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static TestPackage getPackage() {
		return TestPackage.eINSTANCE;
	}

} // TestFactoryImpl
