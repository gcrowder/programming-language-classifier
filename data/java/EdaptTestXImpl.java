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
package org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW;
import org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX;
import org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestXPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>X</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.impl.EdaptTestXImpl#getW <em>W</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestXImpl extends MinimalEObjectImpl.Container implements EdaptTestX {
	/**
	 * The cached value of the '{@link #getW() <em>W</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getW()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestW w;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestXImpl() {
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
		return EdaptTestXPackage.Literals.X;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestW getW() {
		if (w != null && w.eIsProxy()) {
			final InternalEObject oldW = (InternalEObject) w;
			w = (EdaptTestW) eResolveProxy(oldW);
			if (w != oldW) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestXPackage.X__W, oldW, w));
				}
			}
		}
		return w;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestW basicGetW() {
		return w;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setW(EdaptTestW newW) {
		final EdaptTestW oldW = w;
		w = newW;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestXPackage.X__W, oldW, w));
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
		case EdaptTestXPackage.X__W:
			if (resolve) {
				return getW();
			}
			return basicGetW();
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
		case EdaptTestXPackage.X__W:
			setW((EdaptTestW) newValue);
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
		case EdaptTestXPackage.X__W:
			setW((EdaptTestW) null);
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
		case EdaptTestXPackage.X__W:
			return w != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestXImpl
