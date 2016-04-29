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
package org.eclipse.emf.ecp.view.edapt.util.test.model.e.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD;
import org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE;
import org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestF;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>E</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.impl.EdaptTestEImpl#getD <em>D</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.impl.EdaptTestEImpl#getF <em>F</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestEImpl extends MinimalEObjectImpl.Container implements EdaptTestE {
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
	 * The cached value of the '{@link #getF() <em>F</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getF()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestF f;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestEImpl() {
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
		return EdaptTestEPackage.Literals.E;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestEPackage.E__D, oldD, d));
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
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestEPackage.E__D, oldD, d));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestF getF() {
		if (f != null && f.eIsProxy()) {
			final InternalEObject oldF = (InternalEObject) f;
			f = (EdaptTestF) eResolveProxy(oldF);
			if (f != oldF) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestEPackage.E__F, oldF, f));
				}
			}
		}
		return f;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestF basicGetF() {
		return f;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setF(EdaptTestF newF) {
		final EdaptTestF oldF = f;
		f = newF;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestEPackage.E__F, oldF, f));
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
		case EdaptTestEPackage.E__D:
			if (resolve) {
				return getD();
			}
			return basicGetD();
		case EdaptTestEPackage.E__F:
			if (resolve) {
				return getF();
			}
			return basicGetF();
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
		case EdaptTestEPackage.E__D:
			setD((EdaptTestD) newValue);
			return;
		case EdaptTestEPackage.E__F:
			setF((EdaptTestF) newValue);
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
		case EdaptTestEPackage.E__D:
			setD((EdaptTestD) null);
			return;
		case EdaptTestEPackage.E__F:
			setF((EdaptTestF) null);
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
		case EdaptTestEPackage.E__D:
			return d != null;
		case EdaptTestEPackage.E__F:
			return f != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestEImpl
