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
package org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestA;
import org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestB;
import org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestBPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>B</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl#getA <em>A</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.b.impl.EdaptTestBImpl#getC <em>C</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EdaptTestBImpl extends MinimalEObjectImpl.Container implements EdaptTestB {
	/**
	 * The cached value of the '{@link #getA() <em>A</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getA()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestA a;

	/**
	 * The cached value of the '{@link #getC() <em>C</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @see #getC()
	 * @generated
	 * @ordered
	 */
	protected EdaptTestC c;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected EdaptTestBImpl() {
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
		return EdaptTestBPackage.Literals.B;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestA getA() {
		if (a != null && a.eIsProxy()) {
			final InternalEObject oldA = (InternalEObject) a;
			a = (EdaptTestA) eResolveProxy(oldA);
			if (a != oldA) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestBPackage.B__A, oldA, a));
				}
			}
		}
		return a;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestA basicGetA() {
		return a;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setA(EdaptTestA newA) {
		final EdaptTestA oldA = a;
		a = newA;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestBPackage.B__A, oldA, a));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EdaptTestC getC() {
		if (c != null && c.eIsProxy()) {
			final InternalEObject oldC = (InternalEObject) c;
			c = (EdaptTestC) eResolveProxy(oldC);
			if (c != oldC) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EdaptTestBPackage.B__C, oldC, c));
				}
			}
		}
		return c;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public EdaptTestC basicGetC() {
		return c;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setC(EdaptTestC newC) {
		final EdaptTestC oldC = c;
		c = newC;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, EdaptTestBPackage.B__C, oldC, c));
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
		case EdaptTestBPackage.B__A:
			if (resolve) {
				return getA();
			}
			return basicGetA();
		case EdaptTestBPackage.B__C:
			if (resolve) {
				return getC();
			}
			return basicGetC();
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
		case EdaptTestBPackage.B__A:
			setA((EdaptTestA) newValue);
			return;
		case EdaptTestBPackage.B__C:
			setC((EdaptTestC) newValue);
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
		case EdaptTestBPackage.B__A:
			setA((EdaptTestA) null);
			return;
		case EdaptTestBPackage.B__C:
			setC((EdaptTestC) null);
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
		case EdaptTestBPackage.B__A:
			return a != null;
		case EdaptTestBPackage.B__C:
			return c != null;
		}
		return super.eIsSet(featureID);
	}

} // EdaptTestBImpl
