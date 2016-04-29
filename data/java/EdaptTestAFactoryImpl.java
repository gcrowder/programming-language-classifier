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
package org.eclipse.emf.ecp.view.edapt.util.test.model.a.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestA;
import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestAFactory;
import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestAPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * 
 * @generated
 */
public class EdaptTestAFactoryImpl extends EFactoryImpl implements EdaptTestAFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public static EdaptTestAFactory init() {
		try {
			final EdaptTestAFactory theAFactory = (EdaptTestAFactory) EPackage.Registry.INSTANCE
				.getEFactory(EdaptTestAPackage.eNS_URI);
			if (theAFactory != null) {
				return theAFactory;
			}
		} catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new EdaptTestAFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestAFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
		case EdaptTestAPackage.A:
			return createA();
		default:
			throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestA createA() {
		final EdaptTestAImpl a = new EdaptTestAImpl();
		return a;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestAPackage getAPackage() {
		return (EdaptTestAPackage) getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static EdaptTestAPackage getPackage() {
		return EdaptTestAPackage.eINSTANCE;
	}

} // EdaptTestAFactoryImpl
