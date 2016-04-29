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
package org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestDPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>D</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.d.impl.EdaptTestDImpl#getB <em>B</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestDImpl extends MinimalEObjectImpl.Container implements EdaptTestD {
	/**
	 * The cached value of the '{@link #getB() <em>B</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getB()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestB b;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestDImpl() {
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
		return EdaptTestDPackage.Literals.D;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestB getB() {
		if (b != null && b.eIsProxy()) {
			final InternalEObject oldB = (InternalEObject) b;
			b = (EdaptTestB) eResolveProxy(oldB);
			if (b != oldB) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestDPackage.D__B, oldB, b));
				}
			}
		}
		return b;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestB basicGetB() {
		return b;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setB(EdaptTestB newB) {
		final EdaptTestB oldB = b;
		b = newB;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestDPackage.D__B, oldB, b));
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
		case EdaptTestDPackage.D__B:
			if (resolve) {
				return getB();
			}
			return basicGetB();
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
		case EdaptTestDPackage.D__B:
			setB((EdaptTestB) newValue);
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
		case EdaptTestDPackage.D__B:
			setB((EdaptTestB) null);
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
		case EdaptTestDPackage.D__B:
			return b != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestDImpl
