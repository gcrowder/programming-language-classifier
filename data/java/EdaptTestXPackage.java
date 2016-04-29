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
package org.eclipse.emf.ecp.view.edapt.util.test.model.x;

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
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestXFactory
 * @model kind="package"
 * @generated
 */
public interface EdaptTestXPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "x";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://example.org/x";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "x";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	EdaptTestXPackage eINSTANCE = org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXImpl
	 * <em>X</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXImpl
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXPackageImpl#getX()
	 * @generated
	 */
	int X = 0;

	/**
	 * The feature id for the '<em><b>W</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int X__W = 0;

	/**
	 * The number of structural features of the '<em>X</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int X_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>X</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int X_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX <em>X</em>}
	 * '.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>X</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX
	 * @generated
	 */
	EClass getX();

	/**
	 * Returns the meta object for the reference '
	 * {@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX#getW <em>W</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the meta object for the reference '<em>W</em>'.
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX#getW()
	 * @see #getX()
	 * @generated
	 */
	EReference getX_W();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EdaptTestXFactory getXFactory();

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
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXImpl
		 * <em>X</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXImpl
		 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXPackageImpl#getX()
		 * @generated
		 */
		EClass X = eINSTANCE.getX();

		/**
		 * The meta object literal for the '<em><b>W</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference X__W = eINSTANCE.getX_W();

	}

} // EdaptTestXPackage
