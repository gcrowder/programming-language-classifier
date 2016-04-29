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
package org.eclipse.emf.ecp.view.edapt.util.test.model.y.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX;
import org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestY;
import org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestYPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.z.EdaptTestZ;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Y</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.impl.EdaptTestYImpl#getX <em>X</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.impl.EdaptTestYImpl#getZ <em>Z</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestYImpl extends MinimalEObjectImpl.Container implements EdaptTestY {
	/**
	 * The cached value of the '{@link #getX() <em>X</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getX()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestX x;

	/**
	 * The cached value of the '{@link #getZ() <em>Z</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getZ()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestZ z;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestYImpl() {
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
		return EdaptTestYPackage.Literals.Y;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestX getX() {
		if (x != null && x.eIsProxy()) {
			final InternalEObject oldX = (InternalEObject) x;
			x = (EdaptTestX) eResolveProxy(oldX);
			if (x != oldX) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestYPackage.Y__X, oldX, x));
				}
			}
		}
		return x;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestX basicGetX() {
		return x;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setX(EdaptTestX newX) {
		final EdaptTestX oldX = x;
		x = newX;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestYPackage.Y__X, oldX, x));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestZ getZ() {
		if (z != null && z.eIsProxy()) {
			final InternalEObject oldZ = (InternalEObject) z;
			z = (EdaptTestZ) eResolveProxy(oldZ);
			if (z != oldZ) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestYPackage.Y__Z, oldZ, z));
				}
			}
		}
		return z;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestZ basicGetZ() {
		return z;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setZ(EdaptTestZ newZ) {
		final EdaptTestZ oldZ = z;
		z = newZ;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestYPackage.Y__Z, oldZ, z));
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
		case EdaptTestYPackage.Y__X:
			if (resolve) {
				return getX();
			}
			return basicGetX();
		case EdaptTestYPackage.Y__Z:
			if (resolve) {
				return getZ();
			}
			return basicGetZ();
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
		case EdaptTestYPackage.Y__X:
			setX((EdaptTestX) newValue);
			return;
		case EdaptTestYPackage.Y__Z:
			setZ((EdaptTestZ) newValue);
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
		case EdaptTestYPackage.Y__X:
			setX((EdaptTestX) null);
			return;
		case EdaptTestYPackage.Y__Z:
			setZ((EdaptTestZ) null);
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
		case EdaptTestYPackage.Y__X:
			return x != null;
		case EdaptTestYPackage.Y__Z:
			return z != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestYImpl
