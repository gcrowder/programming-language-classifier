/**
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 */
package org.eclipse.emfforms.core.services.databinding.testmodel.test.model.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.EcoreEMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>C</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.impl.CImpl#getD <em>D</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.impl.CImpl#getEClassToString
 * <em>EClass To String</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.impl.CImpl#getEClassToA
 * <em>EClass To A</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.impl.CImpl#getA <em>A</em>}</li>
 * </ul>
 *
 * @generated
 */
public class CImpl extends MinimalEObjectImpl.Container implements C {
	/**
	 * The cached value of the '{@link #getD() <em>D</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getD()
	 * @generated
	 * @ordered
	 */
	protected D d;

	/**
	 * The cached value of the '{@link #getEClassToString() <em>EClass To String</em>}' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getEClassToString()
	 * @generated
	 * @ordered
	 */
	protected EMap<EClass, String> eClassToString;

	/**
	 * The cached value of the '{@link #getEClassToA() <em>EClass To A</em>}' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getEClassToA()
	 * @generated
	 * @ordered
	 */
	protected EMap<EClass, A> eClassToA;

	/**
	 * The cached value of the '{@link #getA() <em>A</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getA()
	 * @generated
	 * @ordered
	 */
	protected A a;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected CImpl() {
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
		return TestPackage.Literals.C;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public D getD() {
		return d;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetD(D newD, NotificationChain msgs) {
		final D oldD = d;
		d = newD;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, TestPackage.C__D, oldD,
				newD);
			if (msgs == null) {
				msgs = notification;
			} else {
				msgs.add(notification);
			}
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setD(D newD) {
		if (newD != d) {
			NotificationChain msgs = null;
			if (d != null) {
				msgs = ((InternalEObject) d).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - TestPackage.C__D, null,
					msgs);
			}
			if (newD != null) {
				msgs = ((InternalEObject) newD).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - TestPackage.C__D, null,
					msgs);
			}
			msgs = basicSetD(newD, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.C__D, newD, newD));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EMap<EClass, String> getEClassToString() {
		if (eClassToString == null) {
			eClassToString = new EcoreEMap<EClass, String>(TestPackage.Literals.ECLASS_TO_ESTRING_MAP,
				EClassToEStringMapImpl.class, this, TestPackage.C__ECLASS_TO_STRING);
		}
		return eClassToString;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EMap<EClass, A> getEClassToA() {
		if (eClassToA == null) {
			eClassToA = new EcoreEMap<EClass, A>(TestPackage.Literals.ECLASS_TO_AMAP, EClassToAMapImpl.class, this,
				TestPackage.C__ECLASS_TO_A);
		}
		return eClassToA;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public A getA() {
		if (a != null && a.eIsProxy()) {
			final InternalEObject oldA = (InternalEObject) a;
			a = (A) eResolveProxy(oldA);
			if (a != oldA) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestPackage.C__A, oldA, a));
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
	public A basicGetA() {
		return a;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setA(A newA) {
		final A oldA = a;
		a = newA;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.C__A, oldA, a));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.C__D:
			return basicSetD(null, msgs);
		case TestPackage.C__ECLASS_TO_STRING:
			return ((InternalEList<?>) getEClassToString()).basicRemove(otherEnd, msgs);
		case TestPackage.C__ECLASS_TO_A:
			return ((InternalEList<?>) getEClassToA()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
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
		case TestPackage.C__D:
			return getD();
		case TestPackage.C__ECLASS_TO_STRING:
			if (coreType) {
				return getEClassToString();
			}
			return getEClassToString().map();
		case TestPackage.C__ECLASS_TO_A:
			if (coreType) {
				return getEClassToA();
			}
			return getEClassToA().map();
		case TestPackage.C__A:
			if (resolve) {
				return getA();
			}
			return basicGetA();
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
		case TestPackage.C__D:
			setD((D) newValue);
			return;
		case TestPackage.C__ECLASS_TO_STRING:
			((EStructuralFeature.Setting) getEClassToString()).set(newValue);
			return;
		case TestPackage.C__ECLASS_TO_A:
			((EStructuralFeature.Setting) getEClassToA()).set(newValue);
			return;
		case TestPackage.C__A:
			setA((A) newValue);
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
		case TestPackage.C__D:
			setD((D) null);
			return;
		case TestPackage.C__ECLASS_TO_STRING:
			getEClassToString().clear();
			return;
		case TestPackage.C__ECLASS_TO_A:
			getEClassToA().clear();
			return;
		case TestPackage.C__A:
			setA((A) null);
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
		case TestPackage.C__D:
			return d != null;
		case TestPackage.C__ECLASS_TO_STRING:
			return eClassToString != null && !eClassToString.isEmpty();
		case TestPackage.C__ECLASS_TO_A:
			return eClassToA != null && !eClassToA.isEmpty();
		case TestPackage.C__A:
			return a != null;
		}
		return super.eIsSet(featureID);
	}

} // CImpl
