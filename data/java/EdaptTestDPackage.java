/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 */
package org.eclipse.emf.ecp.view.edapt.util.test.model.d;

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
 * <li>each operation of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * 
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestDFactory
 * @model kind="package"
 * @generated
 */
public interface EdaptTestDPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "d";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://example.org/d";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "d";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	EdaptTestDPackage eINSTANCE = org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDImpl
	 * <em>D</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDImpl
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDPackageImpl#getD()
	 * @generated
	 */
	int D = 0;

	/**
	 * The feature id for the '<em><b>B</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int D__B = 0;

	/**
	 * The number of structural features of the '<em>D</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int D_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>D</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int D_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD <em>D</em>}
	 * '.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>D</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD
	 * @generated
	 */
	EClass getD();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD#getB <em>B</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>B</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD#getB()
	 * @see #getD()
	 * @generated
	 */
	EReference getD_B();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EdaptTestDFactory getDFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
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
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDImpl
		 * <em>D</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDImpl
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDPackageImpl#getD()
		 * @generated
		 */
		EClass D = eINSTANCE.getD();

		/**
		 * The meta object literal for the '<em><b>B</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference D__B = eINSTANCE.getD_B();

	}

} // EdaptTestDPackage
