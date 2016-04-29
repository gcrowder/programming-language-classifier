/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.TestTMDPackage;

/**
 * <!-- begin-user-doc --> An implementation of the model object ' <em><b>Root</b></em>'. <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>
 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl#getName
 * <em>Name</em>}</li>
 * <li>
 * {@link org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.impl.RootImpl#getChildren
 * <em>Children</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RootImpl extends MinimalEObjectImpl.Container implements Root {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getChildren() <em>Children</em>}'
	 * containment reference list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see #getChildren()
	 * @generated
	 * @ordered
	 */
	protected EList<ChildLevel1> children;

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected RootImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestTMDPackage.Literals.ROOT;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				TestTMDPackage.ROOT__NAME, oldName, name));
		}
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<ChildLevel1> getChildren() {
		if (children == null) {
			children = new EObjectContainmentEList<ChildLevel1>(
				ChildLevel1.class, this, TestTMDPackage.ROOT__CHILDREN);
		}
		return children;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
		int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestTMDPackage.ROOT__CHILDREN:
			return ((InternalEList<?>) getChildren()).basicRemove(otherEnd,
				msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case TestTMDPackage.ROOT__NAME:
			return getName();
		case TestTMDPackage.ROOT__CHILDREN:
			return getChildren();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case TestTMDPackage.ROOT__NAME:
			setName((String) newValue);
			return;
		case TestTMDPackage.ROOT__CHILDREN:
			getChildren().clear();
			getChildren().addAll((Collection<? extends ChildLevel1>) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case TestTMDPackage.ROOT__NAME:
			setName(NAME_EDEFAULT);
			return;
		case TestTMDPackage.ROOT__CHILDREN:
			getChildren().clear();
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case TestTMDPackage.ROOT__NAME:
			return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT
				.equals(name);
		case TestTMDPackage.ROOT__CHILDREN:
			return children != null && !children.isEmpty();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) {
			return super.toString();
		}

		final StringBuffer result = new StringBuffer(super.toString());
		result.append(" (name: "); //$NON-NLS-1$
		result.append(name);
		result.append(')');
		return result.toString();
	}

} // RootImpl
