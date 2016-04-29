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
package org.eclipse.emf.ecp.view.edapt.util.test.model.c;

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
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCFactory
 * @model kind="package"
 * @generated
 */
public interface EdaptTestCPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "c";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://example.org/c";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "c";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	EdaptTestCPackage eINSTANCE = org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCImpl
	 * <em>C</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCImpl
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCPackageImpl#getC()
	 * @generated
	 */
	int C = 0;

	/**
	 * The feature id for the '<em><b>D</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int C__D = 0;

	/**
	 * The number of structural features of the '<em>C</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int C_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>C</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int C_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC <em>C</em>}
	 * '.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>C</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC
	 * @generated
	 */
	EClass getC();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC#getD <em>D</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>D</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC#getD()
	 * @see #getC()
	 * @generated
	 */
	EReference getC_D();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EdaptTestCFactory getCFactory();

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
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCImpl
		 * <em>C</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCImpl
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCPackageImpl#getC()
		 * @generated
		 */
		EClass C = eINSTANCE.getC();

		/**
		 * The meta object literal for the '<em><b>D</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference C__D = eINSTANCE.getC_D();

	}

} // EdaptTestCPackage
