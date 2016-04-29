/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.TestTMDFactory;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.TestTMDPackage;

/**
 * <!-- begin-user-doc --> An implementation of the model <b>Factory</b>. <!--
 * end-user-doc -->
 *
 * @generated
 */
public class TestTMDFactoryImpl extends EFactoryImpl implements TestTMDFactory {
	/**
	 * Creates the default factory implementation. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 *
	 * @generated
	 */
	public static TestTMDFactory init() {
		try {
			final TestTMDFactory theTestTMDFactory = (TestTMDFactory) EPackage.Registry.INSTANCE
				.getEFactory(TestTMDPackage.eNS_URI);
			if (theTestTMDFactory != null) {
				return theTestTMDFactory;
			}
		} catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new TestTMDFactoryImpl();
	}

	/**
	 * Creates an instance of the factory. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 *
	 * @generated
	 */
	public TestTMDFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
		case TestTMDPackage.ROOT:
			return createRoot();
		case TestTMDPackage.CHILD_LEVEL1:
			return createChildLevel1();
		case TestTMDPackage.CHILD_LEVEL2:
			return createChildLevel2();
		default:
			throw new IllegalArgumentException(
				"The class '" + eClass.getName() + "' is not a valid classifier"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Root createRoot() {
		final RootImpl root = new RootImpl();
		return root;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public ChildLevel1 createChildLevel1() {
		final ChildLevel1Impl childLevel1 = new ChildLevel1Impl();
		return childLevel1;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public ChildLevel2 createChildLevel2() {
		final ChildLevel2Impl childLevel2 = new ChildLevel2Impl();
		return childLevel2;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TestTMDPackage getTestTMDPackage() {
		return (TestTMDPackage) getEPackage();
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static TestTMDPackage getPackage() {
		return TestTMDPackage.eINSTANCE;
	}

} // TestTMDFactoryImpl
