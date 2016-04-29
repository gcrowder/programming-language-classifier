/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 */
package org.eclipse.emf.ecp.view.dynamictree.model.util;

import java.util.Map;

import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.ResourceLocator;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.EObjectValidator;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainRoot;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElement;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer;

/**
 * <!-- begin-user-doc -->
 * The <b>Validator</b> for the model.
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage
 * @generated
 */
public class ModelValidator extends EObjectValidator {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final ModelValidator INSTANCE = new ModelValidator();

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
	public static final String DIAGNOSTIC_SOURCE = "org.eclipse.emf.ecp.view.dynamictree.model";

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Validate' of 'Test Element'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final int TEST_ELEMENT__VALIDATE = 1;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private static final int GENERATED_DIAGNOSTIC_CODE_COUNT = 1;

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
	public ModelValidator() {
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
		return ModelPackage.eINSTANCE;
	}

	/**
	 * Calls <code>validateXXX</code> for the corresponding classifier of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected boolean validate(int classifierID, Object value, DiagnosticChain diagnostics, Map<Object, Object> context) {
		switch (classifierID) {
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE:
			return validateDynamicContainmentTree((DynamicContainmentTree) value, diagnostics, context);
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM:
			return validateDynamicContainmentItem((DynamicContainmentItem) value, diagnostics, context);
		case ModelPackage.TEST_ELEMENT:
			return validateTestElement((TestElement) value, diagnostics, context);
		case ModelPackage.DOMAIN_ROOT:
			return validateDomainRoot((DomainRoot) value, diagnostics, context);
		case ModelPackage.DOMAIN_INTERMEDIATE:
			return validateDomainIntermediate((DomainIntermediate) value, diagnostics, context);
		case ModelPackage.TEST_ELEMENT_CONTAINER:
			return validateTestElementContainer((TestElementContainer) value, diagnostics, context);
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE:
			return validateDynamicContainmentTreeDomainModelReference(
				(DynamicContainmentTreeDomainModelReference) value, diagnostics, context);
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
	public boolean validateDynamicContainmentTree(DynamicContainmentTree dynamicContainmentTree,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(dynamicContainmentTree, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateDynamicContainmentItem(DynamicContainmentItem dynamicContainmentItem,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(dynamicContainmentItem, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTestElement(TestElement testElement, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(testElement, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(testElement, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(testElement, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateTestElement_validate(testElement, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the validate constraint of '<em>Test Element</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTestElement_validate(TestElement testElement, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return testElement.validate(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateDomainRoot(DomainRoot domainRoot, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(domainRoot, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateDomainIntermediate(DomainIntermediate domainIntermediate, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(domainIntermediate, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateTestElementContainer(TestElementContainer testElementContainer, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(testElementContainer, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateDynamicContainmentTreeDomainModelReference(
		DynamicContainmentTreeDomainModelReference dynamicContainmentTreeDomainModelReference,
		DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(dynamicContainmentTreeDomainModelReference, diagnostics, context);
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

} // ModelValidator
