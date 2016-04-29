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
package org.eclipse.emf.ecp.view.validation.test.model.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.ResourceLocator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EObjectValidator;
import org.eclipse.emf.ecp.view.internal.validation.ValidationNotification;
import org.eclipse.emf.ecp.view.validation.test.model.Book;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.Container;
import org.eclipse.emf.ecp.view.validation.test.model.Content;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent;
import org.eclipse.emf.ecp.view.validation.test.model.Librarian;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.Mainboard;
import org.eclipse.emf.ecp.view.validation.test.model.PowerBlock;
import org.eclipse.emf.ecp.view.validation.test.model.Referencer;
import org.eclipse.emf.ecp.view.validation.test.model.TableContent;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild2;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithoutValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithUnique;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicityConcrete;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;

/**
 * <!-- begin-user-doc -->
 * The <b>Validator</b> for the model.
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage
 * @generated
 */
public class TestValidator extends EObjectValidator {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final TestValidator INSTANCE = new TestValidator();

	/**
	 * A constant for the {@link org.eclipse.emf.common.util.Diagnostic#getSource() source} of diagnostic
	 * {@link org.eclipse.emf.common.util.Diagnostic#getCode() codes} from this package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.common.util.Diagnostic#getSource()
	 * @see org.eclipse.emf.common.util.Diagnostic#getCode()
	 * @generated
	 */
	public static final String DIAGNOSTIC_SOURCE = "org.eclipse.emf.ecp.view.validation.test.model";

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Validate' of 'Library'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final int LIBRARY__VALIDATE = 1;

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Validate' of 'Writer'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final int WRITER__VALIDATE = 2;

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Validate' of 'Book'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final int BOOK__VALIDATE = 3;

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Validate' of 'Librarian'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final int LIBRARIAN__VALIDATE = 4;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private static final int GENERATED_DIAGNOSTIC_CODE_COUNT = 4;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants in a
	 * derived class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected static final int DIAGNOSTIC_CODE_COUNT = GENERATED_DIAGNOSTIC_CODE_COUNT;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public TestValidator() {
		super();
	}

	/**
	 * Returns the package of this validator switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EPackage getEPackage() {
		return TestPackage.eINSTANCE;
	}

	/**
	 * Calls <code>validateXXX</code> for the corresponding classifier of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected boolean validate(int classifierID, Object value, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		switch (classifierID) {
		case TestPackage.LIBRARY:
			return validateLibrary((Library) value, diagnostics, context);
		case TestPackage.WRITER:
			return validateWriter((Writer) value, diagnostics, context);
		case TestPackage.BOOK:
			return validateBook((Book) value, diagnostics, context);
		case TestPackage.LIBRARIAN:
			return validateLibrarian((Librarian) value, diagnostics, context);
		case TestPackage.COMPUTER:
			return validateComputer((Computer) value, diagnostics, context);
		case TestPackage.MAINBOARD:
			return validateMainboard((Mainboard) value, diagnostics, context);
		case TestPackage.POWER_BLOCK:
			return validatePowerBlock((PowerBlock) value, diagnostics, context);
		case TestPackage.CONTAINER:
			return validateContainer((Container) value, diagnostics, context);
		case TestPackage.CONTENT:
			return validateContent((Content) value, diagnostics, context);
		case TestPackage.TABLE_WITH_MULTIPLICITY:
			return validateTableWithMultiplicity((TableWithMultiplicity) value, diagnostics, context);
		case TestPackage.TABLE_CONTENT:
			return validateTableContent((TableContent) value, diagnostics, context);
		case TestPackage.TABLE_CONTENT_WITHOUT_VALIDATION:
			return validateTableContentWithoutValidation((TableContentWithoutValidation) value, diagnostics, context);
		case TestPackage.TABLE_CONTENT_WITH_VALIDATION:
			return validateTableContentWithValidation((TableContentWithValidation) value, diagnostics, context);
		case TestPackage.TABLE_WITHOUT_MULTIPLICITY:
			return validateTableWithoutMultiplicity((TableWithoutMultiplicity) value, diagnostics, context);
		case TestPackage.TABLE_WITH_UNIQUE:
			return validateTableWithUnique((TableWithUnique) value, diagnostics, context);
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD2:
			return validateTableContentWithInnerChild2((TableContentWithInnerChild2) value, diagnostics, context);
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD:
			return validateTableContentWithInnerChild((TableContentWithInnerChild) value, diagnostics, context);
		case TestPackage.TABLE_WITHOUT_MULTIPLICITY_CONCRETE:
			return validateTableWithoutMultiplicityConcrete((TableWithoutMultiplicityConcrete) value, diagnostics,
				context);
		case TestPackage.REFERENCER:
			return validateReferencer((Referencer) value, diagnostics, context);
		case TestPackage.CROSS_REFERENCE_CONTAINER:
			return validateCrossReferenceContainer((CrossReferenceContainer) value, diagnostics, context);
		case TestPackage.CROSS_REFERENCE_CONTENT:
			return validateCrossReferenceContent((CrossReferenceContent) value, diagnostics, context);
		default:
			return true;
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateLibrary(Library library, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(library, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(library, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(library, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateLibrary_validate(library, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the validate constraint of '<em>Library</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateLibrary_validate(Library library, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return library.validate(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateWriter(Writer writer, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(writer, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(writer, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(writer, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateWriter_validate(writer, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the validate constraint of '<em>Writer</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateWriter_validate(Writer writer, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return writer.validate(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateBook(Book book, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(book, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(book, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(book, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateBook_validate(book, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the validate constraint of '<em>Book</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateBook_validate(Book book, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return book.validate(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateLibrarian(Librarian librarian, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(librarian, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(librarian, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(librarian, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateLibrarian_validate(librarian, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the validate constraint of '<em>Librarian</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateLibrarian_validate(Librarian librarian, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return librarian.validate(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateComputer(Computer computer, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(computer, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateMainboard(Mainboard mainboard, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(mainboard, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validatePowerBlock(PowerBlock powerBlock, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(powerBlock, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	public boolean validateContainer(Container container, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(container, diagnostics, context)
			&& validateContainerUniqueAttribuite(container, diagnostics, context);
	}

	/**
	 *
	 * @generated NOT
	 */
	public boolean validateContainerUniqueAttribuite(Container container,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		// TODO implement the constraint
		// -> specify the condition that violates the constraint
		// -> verify the diagnostic details, including severity, code, and message
		// Ensure that you remove @generated or mark it @generated NOT
		final EList<Content> contents = container.getContents();
		final Map<String, Set<Content>> uniqueAttToContentMap = new LinkedHashMap<String, Set<Content>>();

		for (final Content content : contents) {
			final String uniquiAtt = content.getUniqueAttribute();
			if (!uniqueAttToContentMap.containsKey(uniquiAtt)) {
				uniqueAttToContentMap.put(uniquiAtt, new LinkedHashSet<Content>());
			}
			uniqueAttToContentMap.get(uniquiAtt).add(content);
		}
		final List<Content> duplicates = new ArrayList<Content>();
		for (final String language : uniqueAttToContentMap.keySet()) {
			if (uniqueAttToContentMap.get(language).size() > 1) {
				duplicates.addAll(uniqueAttToContentMap.get(language));
			}
		}
		if (!duplicates.isEmpty()) {
			createUniqueDiagnostic(duplicates, diagnostics, context,
				"Same unique Attribute not allowed.", TestPackage.eINSTANCE.getContent_UniqueAttribute(),
				Diagnostic.ERROR);
			return false;
		}
		// create ok results
		for (final Content content : contents) {
			content.eNotify(new ValidationNotification(content));
		}

		final boolean result = true;
		return result;
	}

	/**
	 * @generated NOT
	 */
	private void createUniqueDiagnostic(List<? extends EObject> duplicates, DiagnosticChain diagnostics,
		Map<Object, Object> context, String message, EStructuralFeature feature, int severity) {
		if (diagnostics != null) {
			for (final EObject duplicate : duplicates) {
				diagnostics.add(createDiagnostic(severity, DIAGNOSTIC_SOURCE, 0,
					"_UI_GenericConstraint_diagnostic",
					new Object[] { message, getObjectLabel(duplicate, context) }, new Object[] { duplicate,
						feature },
					context));
			}
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	public boolean validateContent(Content content, DiagnosticChain diagnostics, Map<Object, Object> context) {
		content.eNotify(new ValidationNotification(content.eContainer()));
		return validate_EveryDefaultConstraint(content, diagnostics, context);
		// && validate(content.eContainer().eClass().getClassifierID(), content.eContainer(),
		// diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableWithMultiplicity(TableWithMultiplicity tableWithMultiplicity,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableWithMultiplicity, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableContent(TableContent tableContent, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableContent, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	public boolean validateTableContentWithoutValidation(TableContentWithoutValidation tableContentWithoutValidation,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		tableContentWithoutValidation.eNotify(new ValidationNotification(tableContentWithoutValidation.eContainer()));
		return validate_EveryDefaultConstraint(tableContentWithoutValidation, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	public boolean validateTableContentWithValidation(TableContentWithValidation tableContentWithValidation,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		tableContentWithValidation.eNotify(new ValidationNotification(tableContentWithValidation.eContainer()));
		return validate_EveryDefaultConstraint(tableContentWithValidation, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableWithoutMultiplicity(TableWithoutMultiplicity tableWithoutMultiplicity,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableWithoutMultiplicity, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	public boolean validateTableWithUnique(TableWithUnique tableWithUnique, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableWithUnique, diagnostics, context)
			& validateUniqueness(tableWithUnique, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableContentWithInnerChild2(TableContentWithInnerChild2 tableContentWithInnerChild2,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableContentWithInnerChild2, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableContentWithInnerChild(TableContentWithInnerChild tableContentWithInnerChild,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableContentWithInnerChild, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTableWithoutMultiplicityConcrete(
		TableWithoutMultiplicityConcrete tableWithoutMultiplicityConcrete, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(tableWithoutMultiplicityConcrete, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateReferencer(Referencer referencer, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(referencer, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateCrossReferenceContainer(CrossReferenceContainer crossReferenceContainer,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(crossReferenceContainer, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateCrossReferenceContent(CrossReferenceContent crossReferenceContent,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(crossReferenceContent, diagnostics, context);
	}

	private boolean validateUniqueness(TableWithUnique tableWithUnique, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		final EList<TableContent> contents = tableWithUnique.getContent();
		final Map<String, Set<TableContent>> uniqueAttToContentMap = new LinkedHashMap<String, Set<TableContent>>();

		EStructuralFeature nameFeature = null;

		for (final TableContent content : contents) {
			String uniquiAtt = null;
			if (TableContentWithoutValidation.class.isInstance(content)) {
				uniquiAtt = ((TableContentWithoutValidation) content).getName();
				nameFeature = TestPackage.eINSTANCE.getTableContentWithoutValidation_Name();
			}
			if (TableContentWithValidation.class.isInstance(content)) {
				uniquiAtt = ((TableContentWithValidation) content).getName();
				nameFeature = TestPackage.eINSTANCE.getTableContentWithValidation_Name();
			}
			if (!uniqueAttToContentMap.containsKey(uniquiAtt)) {
				uniqueAttToContentMap.put(uniquiAtt, new LinkedHashSet<TableContent>());
			}
			uniqueAttToContentMap.get(uniquiAtt).add(content);
		}
		final List<TableContent> duplicates = new ArrayList<TableContent>();
		for (final String language : uniqueAttToContentMap.keySet()) {
			if (uniqueAttToContentMap.get(language).size() > 1) {
				duplicates.addAll(uniqueAttToContentMap.get(language));
			}
		}

		boolean noDuplicates = true;

		if (!duplicates.isEmpty()) {
			createUniqueDiagnostic(duplicates, diagnostics, context,
				"Same unique name not allowed.", nameFeature, Diagnostic.WARNING);
			noDuplicates = false;
		}
		// create ok results
		for (final TableContent content : contents) {
			content.eNotify(new ValidationNotification(content));
		}

		return noDuplicates;
	}

	/**
	 * Returns the resource locator that will be used to fetch messages for this validator's diagnostics.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public ResourceLocator getResourceLocator() {
		// TODO
		// Specialize this to return a resource locator for messages specific to this validator.
		// Ensure that you remove @generated or mark it @generated NOT
		return super.getResourceLocator();
	}

} // TestValidator
