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
package org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC;
import org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>C</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.impl.EdaptTestCImpl#getD <em>D</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestCImpl extends MinimalEObjectImpl.Container implements EdaptTestC {
	/**
	 * The cached value of the '{@link #getD() <em>D</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getD()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestD d;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestCImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return EdaptTestCPackage.Literals.C;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestD getD() {
		if (d != null && d.eIsProxy()) {
			final InternalEObject oldD = (InternalEObject) d;
			d = (EdaptTestD) eResolveProxy(oldD);
			if (d != oldD) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestCPackage.C__D, oldD, d));
				}
			}
		}
		return d;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestD basicGetD() {
		return d;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setD(EdaptTestD newD) {
		final EdaptTestD oldD = d;
		d = newD;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestCPackage.C__D, oldD, d));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case EdaptTestCPackage.C__D:
			if (resolve) {
				return getD();
			}
			return basicGetD();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case EdaptTestCPackage.C__D:
			setD((EdaptTestD) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case EdaptTestCPackage.C__D:
			setD((EdaptTestD) null);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case EdaptTestCPackage.C__D:
			return d != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestCImpl
