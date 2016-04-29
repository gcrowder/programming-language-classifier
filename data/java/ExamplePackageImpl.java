/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 */
package org.eclipse.emf.ecp.view.index.test.example.impl;

import static org.eclipse.emf.ecp.view.index.test.example.ExamplePackage.CONTAINER;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;
import org.eclipse.emf.ecp.view.index.test.example.Child;
import org.eclipse.emf.ecp.view.index.test.example.ExampleFactory;
import org.eclipse.emf.ecp.view.index.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.index.test.example.Intermediate;
import org.eclipse.emf.ecp.view.index.test.example.IntermediateTarget;
import org.eclipse.emf.ecp.view.index.test.example.Root;
import org.eclipse.emf.ecp.view.index.test.example.Target;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 *
 * @generated
 */
public class ExamplePackageImpl extends EPackageImpl implements ExamplePackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass rootEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass intermediateEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass containerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass childEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass intermediateTargetEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass targetEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package
	 * package URI value.
	 * <p>
	 * Note: the correct way to create the package is via the static factory method {@link #init init()}, which also
	 * performs initialization of the package, or returns the registered package, if one already exists. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see org.eclipse.emf.ecp.view.index.test.example.ExamplePackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ExamplePackageImpl() {
		super(eNS_URI, ExampleFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>
	 * This method is used to initialize {@link ExamplePackage#eINSTANCE} when that field is accessed. Clients should
	 * not invoke it directly. Instead, they should simply access that field to obtain the package. <!-- begin-user-doc
	 * --> <!-- end-user-doc -->
	 *
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static ExamplePackage init() {
		if (isInited) {
			return (ExamplePackage) EPackage.Registry.INSTANCE.getEPackage(ExamplePackage.eNS_URI);
		}

		// Obtain or create and register package
		final ExamplePackageImpl theExamplePackage = (ExamplePackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ExamplePackageImpl ? EPackage.Registry.INSTANCE
			.get(eNS_URI)
			: new ExamplePackageImpl());

		isInited = true;

		// Create package meta-data objects
		theExamplePackage.createPackageContents();

		// Initialize created meta-data
		theExamplePackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theExamplePackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ExamplePackage.eNS_URI, theExamplePackage);
		return theExamplePackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getRoot() {
		return rootEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EReference getRoot_Intermediate() {
		return (EReference) rootEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getIntermediate() {
		return intermediateEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EReference getIntermediate_Container() {
		return (EReference) intermediateEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getContainer() {
		return containerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EReference getContainer_Children() {
		return (EReference) containerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getChild() {
		return childEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EReference getChild_IntermediateTarget() {
		return (EReference) childEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getIntermediateTarget() {
		return intermediateTargetEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EReference getIntermediateTarget_Target() {
		return (EReference) intermediateTargetEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getTarget() {
		return targetEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EAttribute getTarget_Name() {
		return (EAttribute) targetEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public ExampleFactory getExampleFactory() {
		return (ExampleFactory) getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package. This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) {
			return;
		}
		isCreated = true;

		// Create classes and their features
		rootEClass = createEClass(ROOT);
		createEReference(rootEClass, ROOT__INTERMEDIATE);

		intermediateEClass = createEClass(INTERMEDIATE);
		createEReference(intermediateEClass, INTERMEDIATE__CONTAINER);

		containerEClass = createEClass(CONTAINER);
		createEReference(containerEClass, CONTAINER__CHILDREN);

		childEClass = createEClass(CHILD);
		createEReference(childEClass, CHILD__INTERMEDIATE_TARGET);

		intermediateTargetEClass = createEClass(INTERMEDIATE_TARGET);
		createEReference(intermediateTargetEClass, INTERMEDIATE_TARGET__TARGET);

		targetEClass = createEClass(TARGET);
		createEAttribute(targetEClass, TARGET__NAME);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model. This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) {
			return;
		}
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes, features, and operations; add parameters
		initEClass(rootEClass, Root.class, "Root", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEReference(getRoot_Intermediate(), getIntermediate(), null, "intermediate", null, 0, 1, Root.class, //$NON-NLS-1$
			!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			!IS_DERIVED, IS_ORDERED);

		initEClass(intermediateEClass, Intermediate.class, "Intermediate", !IS_ABSTRACT, !IS_INTERFACE, //$NON-NLS-1$
			IS_GENERATED_INSTANCE_CLASS);
		initEReference(getIntermediate_Container(), getContainer(), null, "container", null, 0, 1, Intermediate.class, //$NON-NLS-1$
			!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			!IS_DERIVED, IS_ORDERED);

		initEClass(containerEClass, org.eclipse.emf.ecp.view.index.test.example.Container.class, "Container", //$NON-NLS-1$
			!IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getContainer_Children(), getChild(), null, "children", null, 0, -1, //$NON-NLS-1$
			org.eclipse.emf.ecp.view.index.test.example.Container.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE,
			IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(childEClass, Child.class, "Child", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEReference(getChild_IntermediateTarget(), getIntermediateTarget(), null, "intermediateTarget", null, 0, 1, //$NON-NLS-1$
			Child.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
			IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(intermediateTargetEClass, IntermediateTarget.class, "IntermediateTarget", !IS_ABSTRACT, //$NON-NLS-1$
			!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getIntermediateTarget_Target(), getTarget(), null, "target", null, 0, 1, //$NON-NLS-1$
			IntermediateTarget.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
			!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(targetEClass, Target.class, "Target", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEAttribute(getTarget_Name(), ecorePackage.getEString(), "name", null, 0, 1, Target.class, !IS_TRANSIENT, //$NON-NLS-1$
			!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

} // ExamplePackageImpl
