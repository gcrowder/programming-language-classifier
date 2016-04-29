/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 */
package org.eclipse.emf.ecp.view.validation.test.model.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Cross Reference Content</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.CrossReferenceContentImpl#getParent <em>Parent</em>}
 * </li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.CrossReferenceContentImpl#getSingleParent
 * <em>Single Parent</em>}</li>
 * </ul>
 *
 * @generated
 */
public class CrossReferenceContentImpl extends EObjectImpl implements CrossReferenceContent {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected CrossReferenceContentImpl() {
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
		return TestPackage.Literals.CROSS_REFERENCE_CONTENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public CrossReferenceContainer getParent() {
		if (eContainerFeatureID() != TestPackage.CROSS_REFERENCE_CONTENT__PARENT) {
			return null;
		}
		return (CrossReferenceContainer) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetParent(CrossReferenceContainer newParent, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newParent, TestPackage.CROSS_REFERENCE_CONTENT__PARENT, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setParent(CrossReferenceContainer newParent) {
		if (newParent != eInternalContainer()
			|| eContainerFeatureID() != TestPackage.CROSS_REFERENCE_CONTENT__PARENT && newParent != null) {
			if (EcoreUtil.isAncestor(this, newParent)) {
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
			}
			NotificationChain msgs = null;
			if (eInternalContainer() != null) {
				msgs = eBasicRemoveFromContainer(msgs);
			}
			if (newParent != null) {
				msgs = ((InternalEObject) newParent).eInverseAdd(this, TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS,
					CrossReferenceContainer.class, msgs);
			}
			msgs = basicSetParent(newParent, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.CROSS_REFERENCE_CONTENT__PARENT,
				newParent, newParent));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public CrossReferenceContainer getSingleParent() {
		if (eContainerFeatureID() != TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT) {
			return null;
		}
		return (CrossReferenceContainer) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetSingleParent(CrossReferenceContainer newSingleParent, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newSingleParent, TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT,
			msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setSingleParent(CrossReferenceContainer newSingleParent) {
		if (newSingleParent != eInternalContainer()
			|| eContainerFeatureID() != TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT && newSingleParent != null) {
			if (EcoreUtil.isAncestor(this, newSingleParent)) {
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
			}
			NotificationChain msgs = null;
			if (eInternalContainer() != null) {
				msgs = eBasicRemoveFromContainer(msgs);
			}
			if (newSingleParent != null) {
				msgs = ((InternalEObject) newSingleParent).eInverseAdd(this,
					TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT, CrossReferenceContainer.class, msgs);
			}
			msgs = basicSetSingleParent(newSingleParent, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT,
				newSingleParent, newSingleParent));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			if (eInternalContainer() != null) {
				msgs = eBasicRemoveFromContainer(msgs);
			}
			return basicSetParent((CrossReferenceContainer) otherEnd, msgs);
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			if (eInternalContainer() != null) {
				msgs = eBasicRemoveFromContainer(msgs);
			}
			return basicSetSingleParent((CrossReferenceContainer) otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
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
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			return basicSetParent(null, msgs);
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			return basicSetSingleParent(null, msgs);
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
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			return eInternalContainer().eInverseRemove(this, TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS,
				CrossReferenceContainer.class, msgs);
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			return eInternalContainer().eInverseRemove(this, TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT,
				CrossReferenceContainer.class, msgs);
		}
		return super.eBasicRemoveFromContainerFeature(msgs);
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
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			return getParent();
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			return getSingleParent();
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
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			setParent((CrossReferenceContainer) newValue);
			return;
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			setSingleParent((CrossReferenceContainer) newValue);
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
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			setParent((CrossReferenceContainer) null);
			return;
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			setSingleParent((CrossReferenceContainer) null);
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
		case TestPackage.CROSS_REFERENCE_CONTENT__PARENT:
			return getParent() != null;
		case TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT:
			return getSingleParent() != null;
		}
		return super.eIsSet(featureID);
	}

} // CrossReferenceContentImpl
