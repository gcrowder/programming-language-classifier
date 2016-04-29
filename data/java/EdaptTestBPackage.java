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
package org.eclipse.emf.ecp.view.edapt.util.test.model.b;

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
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestBFactory
 * @model kind="package"
 * @generated
 */
public interface EdaptTestBPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "b";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://example.org/b";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "b";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	EdaptTestBPackage eINSTANCE = org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl
	 * <em>B</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBPackageImpl#getB()
	 * @generated
	 */
	int B = 0;

	/**
	 * The feature id for the '<em><b>A</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int B__A = 0;

	/**
	 * The feature id for the '<em><b>C</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int B__C = 1;

	/**
	 * The number of structural features of the '<em>B</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int B_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>B</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int B_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB <em>B</em>}
	 * '.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>B</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB
	 * @generated
	 */
	EClass getB();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB#getA <em>A</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>A</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB#getA()
	 * @see #getB()
	 * @generated
	 */
	EReference getB_A();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB#getC <em>C</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>C</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB#getC()
	 * @see #getB()
	 * @generated
	 */
	EReference getB_C();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EdaptTestBFactory getBFactory();

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
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl
		 * <em>B</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBPackageImpl#getB()
		 * @generated
		 */
		EClass B = eINSTANCE.getB();

		/**
		 * The meta object literal for the '<em><b>A</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference B__A = eINSTANCE.getB_A();

		/**
		 * The meta object literal for the '<em><b>C</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference B__C = eINSTANCE.getB_C();

	}

} // EdaptTestBPackage
