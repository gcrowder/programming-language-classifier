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
package org.eclipse.emf.ecp.view.edapt.util.test.model.w;

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
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestWFactory
 * @model kind="package"
 * @generated
 */
public interface EdaptTestWPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "w";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://example.org/w";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "w";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	EdaptTestWPackage eINSTANCE = org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWImpl
	 * <em>W</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWImpl
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWPackageImpl#getW()
	 * @generated
	 */
	int W = 0;

	/**
	 * The feature id for the '<em><b>X</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int W__X = 0;

	/**
	 * The number of structural features of the '<em>W</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int W_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>W</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int W_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW <em>W</em>}
	 * '.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>W</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW
	 * @generated
	 */
	EClass getW();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW#getX <em>X</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>X</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW#getX()
	 * @see #getW()
	 * @generated
	 */
	EReference getW_X();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EdaptTestWFactory getWFactory();

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
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWImpl
		 * <em>W</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWImpl
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.w.impl.EdaptTestWPackageImpl#getW()
		 * @generated
		 */
		EClass W = eINSTANCE.getW();

		/**
		 * The meta object literal for the '<em><b>X</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference W__X = eINSTANCE.getW_X();

	}

} // EdaptTestWPackage
