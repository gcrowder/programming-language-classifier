/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 */
package org.eclipse.emf.ecp.view.mapping.test.example.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.BasicEMap;
import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.mapping.test.example.AbstractChild;
import org.eclipse.emf.ecp.view.mapping.test.example.ExamplePackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>EClass To Addition Map</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.mapping.test.example.impl.EClassToAdditionMapImpl#getTypedKey <em>Key</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.mapping.test.example.impl.EClassToAdditionMapImpl#getTypedValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EClassToAdditionMapImpl extends MinimalEObjectImpl.Container implements
	BasicEMap.Entry<EClass, AbstractChild> {
	/**
	 * The cached value of the '{@link #getTypedKey() <em>Key</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getTypedKey()
	 * @generated
	 * @ordered
	 */
	protected EClass key;

	/**
	 * The cached value of the '{@link #getTypedValue() <em>Value</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getTypedValue()
	 * @generated
	 * @ordered
	 */
	protected AbstractChild value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected EClassToAdditionMapImpl() {
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
		return ExamplePackage.Literals.ECLASS_TO_ADDITION_MAP;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getTypedKey() {
		if (key != null && key.eIsProxy()) {
			final InternalEObject oldKey = (InternalEObject) key;
			key = (EClass) eResolveProxy(oldKey);
			if (key != oldKey) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY, oldKey, key));
				}
			}
		}
		return key;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass basicGetTypedKey() {
		return key;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void setTypedKey(EClass newKey) {
		final EClass oldKey = key;
		key = newKey;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY, oldKey,
				key));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public AbstractChild getTypedValue() {
		if (value != null && value.eIsProxy()) {
			final InternalEObject oldValue = (InternalEObject) value;
			value = (AbstractChild) eResolveProxy(oldValue);
			if (value != oldValue) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE, oldValue, value));
				}
			}
		}
		return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public AbstractChild basicGetTypedValue() {
		return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void setTypedValue(AbstractChild newValue) {
		final AbstractChild oldValue = value;
		value = newValue;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE,
				oldValue, value));
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
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY:
			if (resolve) {
				return getTypedKey();
			}
			return basicGetTypedKey();
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE:
			if (resolve) {
				return getTypedValue();
			}
			return basicGetTypedValue();
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
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY:
			setTypedKey((EClass) newValue);
			return;
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE:
			setTypedValue((AbstractChild) newValue);
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
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY:
			setTypedKey((EClass) null);
			return;
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE:
			setTypedValue((AbstractChild) null);
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
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__KEY:
			return key != null;
		case ExamplePackage.ECLASS_TO_ADDITION_MAP__VALUE:
			return value != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected int hash = -1;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public int getHash() {
		if (hash == -1) {
			final Object theKey = getKey();
			hash = theKey == null ? 0 : theKey.hashCode();
		}
		return hash;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setHash(int hash) {
		this.hash = hash;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EClass getKey() {
		return getTypedKey();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setKey(EClass key) {
		setTypedKey(key);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public AbstractChild getValue() {
		return getTypedValue();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public AbstractChild setValue(AbstractChild value) {
		final AbstractChild oldValue = getValue();
		setTypedValue(value);
		return oldValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	public EMap<EClass, AbstractChild> getEMap() {
		final EObject container = eContainer();
		return container == null ? null : (EMap<EClass, AbstractChild>) container.eGet(eContainmentFeature());
	}

} // EClassToAdditionMapImpl
