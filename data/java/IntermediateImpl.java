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
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.mapping.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.mapping.test.example.Intermediate;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Intermediate</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.mapping.test.example.impl.IntermediateImpl#getContainer <em>Container</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class IntermediateImpl extends MinimalEObjectImpl.Container implements Intermediate {
	/**
	 * The cached value of the '{@link #getContainer() <em>Container</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getContainer()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.emf.ecp.view.mapping.test.example.Container container;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected IntermediateImpl() {
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
		return ExamplePackage.Literals.INTERMEDIATE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public org.eclipse.emf.ecp.view.mapping.test.example.Container getContainer() {
		return container;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetContainer(org.eclipse.emf.ecp.view.mapping.test.example.Container newContainer,
		NotificationChain msgs) {
		final org.eclipse.emf.ecp.view.mapping.test.example.Container oldContainer = container;
		container = newContainer;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ExamplePackage.INTERMEDIATE__CONTAINER, oldContainer, newContainer);
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
	public void setContainer(org.eclipse.emf.ecp.view.mapping.test.example.Container newContainer) {
		if (newContainer != container) {
			NotificationChain msgs = null;
			if (container != null) {
				msgs = ((InternalEObject) container).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ExamplePackage.INTERMEDIATE__CONTAINER, null, msgs);
			}
			if (newContainer != null) {
				msgs = ((InternalEObject) newContainer).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ExamplePackage.INTERMEDIATE__CONTAINER, null, msgs);
			}
			msgs = basicSetContainer(newContainer, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ExamplePackage.INTERMEDIATE__CONTAINER, newContainer,
				newContainer));
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
		case ExamplePackage.INTERMEDIATE__CONTAINER:
			return basicSetContainer(null, msgs);
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
		case ExamplePackage.INTERMEDIATE__CONTAINER:
			return getContainer();
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
		case ExamplePackage.INTERMEDIATE__CONTAINER:
			setContainer((org.eclipse.emf.ecp.view.mapping.test.example.Container) newValue);
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
		case ExamplePackage.INTERMEDIATE__CONTAINER:
			setContainer((org.eclipse.emf.ecp.view.mapping.test.example.Container) null);
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
		case ExamplePackage.INTERMEDIATE__CONTAINER:
			return container != null;
		}
		return super.eIsSet(featureID);
	}

} // IntermediateImpl
