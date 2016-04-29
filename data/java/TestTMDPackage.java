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
package org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc --> The <b>Package</b> for the model. It contains
 * accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each operation of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.TestTMDFactory
 * @model kind="package"
 * @generated
 */
public interface TestTMDPackage extends EPackage {
	/**
	 * The package name. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNAME = "TestTMD"; //$NON-NLS-1$

	/**
	 * The package namespace URI. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_URI = "TestTMD"; //$NON-NLS-1$

	/**
	 * The package namespace name. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_PREFIX = "TestTMD"; //$NON-NLS-1$

	/**
	 * The singleton instance of the package. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 *
	 * @generated
	 */
	TestTMDPackage eINSTANCE = org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl
		.init();

	/**
	 * The meta object id for the '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl
	 * <em>Root</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getRoot()
	 * @generated
	 */
	int ROOT = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ROOT__NAME = 0;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference
	 * list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ROOT__CHILDREN = 1;

	/**
	 * The number of structural features of the '<em>Root</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ROOT_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Root</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ROOT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel1Impl
	 * <em>Child Level1</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc
	 * -->
	 *
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel1Impl
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getChildLevel1()
	 * @generated
	 */
	int CHILD_LEVEL1 = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL1__NAME = 0;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference
	 * list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL1__CHILDREN = 1;

	/**
	 * The number of structural features of the '<em>Child Level1</em>' class.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL1_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Child Level1</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL1_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel2Impl
	 * <em>Child Level2</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc
	 * -->
	 *
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel2Impl
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getChildLevel2()
	 * @generated
	 */
	int CHILD_LEVEL2 = 2;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL2__NAME = 0;

	/**
	 * The number of structural features of the '<em>Child Level2</em>' class.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL2_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Child Level2</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int CHILD_LEVEL2_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root
	 * <em>Root</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Root</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root
	 * @generated
	 */
	EClass getRoot();

	/**
	 * Returns the meta object for the attribute '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root#getName
	 * <em>Name</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root#getName()
	 * @see #getRoot()
	 * @generated
	 */
	EAttribute getRoot_Name();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root#getChildren
	 * <em>Children</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list ' <em>Children</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root#getChildren()
	 * @see #getRoot()
	 * @generated
	 */
	EReference getRoot_Children();

	/**
	 * Returns the meta object for class '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1
	 * <em>Child Level1</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Child Level1</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1
	 * @generated
	 */
	EClass getChildLevel1();

	/**
	 * Returns the meta object for the attribute '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1#getName
	 * <em>Name</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1#getName()
	 * @see #getChildLevel1()
	 * @generated
	 */
	EAttribute getChildLevel1_Name();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1#getChildren
	 * <em>Children</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list ' <em>Children</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1#getChildren()
	 * @see #getChildLevel1()
	 * @generated
	 */
	EReference getChildLevel1_Children();

	/**
	 * Returns the meta object for class '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2
	 * <em>Child Level2</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Child Level2</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2
	 * @generated
	 */
	EClass getChildLevel2();

	/**
	 * Returns the meta object for the attribute '
	 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2#getName
	 * <em>Name</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2#getName()
	 * @see #getChildLevel2()
	 * @generated
	 */
	EAttribute getChildLevel2_Name();

	/**
	 * Returns the factory that creates the instances of the model. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	TestTMDFactory getTestTMDFactory();

	/**
	 * <!-- begin-user-doc --> Defines literals for the meta objects that
	 * represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each operation of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '
		 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl
		 * <em>Root</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getRoot()
		 * @generated
		 */
		EClass ROOT = eINSTANCE.getRoot();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute
		 * feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute ROOT__NAME = eINSTANCE.getRoot_Name();

		/**
		 * The meta object literal for the '<em><b>Children</b></em>'
		 * containment reference list feature. <!-- begin-user-doc --> <!--
		 * end-user-doc -->
		 *
		 * @generated
		 */
		EReference ROOT__CHILDREN = eINSTANCE.getRoot_Children();

		/**
		 * The meta object literal for the '
		 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel1Impl
		 * <em>Child Level1</em>}' class. <!-- begin-user-doc --> <!--
		 * end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel1Impl
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getChildLevel1()
		 * @generated
		 */
		EClass CHILD_LEVEL1 = eINSTANCE.getChildLevel1();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute
		 * feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute CHILD_LEVEL1__NAME = eINSTANCE.getChildLevel1_Name();

		/**
		 * The meta object literal for the '<em><b>Children</b></em>'
		 * containment reference list feature. <!-- begin-user-doc --> <!--
		 * end-user-doc -->
		 *
		 * @generated
		 */
		EReference CHILD_LEVEL1__CHILDREN = eINSTANCE.getChildLevel1_Children();

		/**
		 * The meta object literal for the '
		 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel2Impl
		 * <em>Child Level2</em>}' class. <!-- begin-user-doc --> <!--
		 * end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.ChildLevel2Impl
		 * @see org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.TestTMDPackageImpl#getChildLevel2()
		 * @generated
		 */
		EClass CHILD_LEVEL2 = eINSTANCE.getChildLevel2();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute
		 * feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute CHILD_LEVEL2__NAME = eINSTANCE.getChildLevel2_Name();

	}

} // TestTMDPackage
