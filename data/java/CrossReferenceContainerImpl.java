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

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Cross Reference Container</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.CrossReferenceContainerImpl#getContents
 * <em>Contents</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.CrossReferenceContainerImpl#getSingleContent
 * <em>Single Content</em>}</li>
 * </ul>
 *
 * @generated
 */
public class CrossReferenceContainerImpl extends EObjectImpl implements CrossReferenceContainer {
	/**
	 * The cached value of the '{@link #getContents() <em>Contents</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getContents()
	 * @generated
	 * @ordered
	 */
	protected EList<CrossReferenceContent> contents;

	/**
	 * The cached value of the '{@link #getSingleContent() <em>Single Content</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getSingleContent()
	 * @generated
	 * @ordered
	 */
	protected CrossReferenceContent singleContent;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected CrossReferenceContainerImpl() {
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
		return TestPackage.Literals.CROSS_REFERENCE_CONTAINER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<CrossReferenceContent> getContents() {
		if (contents == null) {
			contents = new EObjectContainmentWithInverseEList<CrossReferenceContent>(CrossReferenceContent.class, this,
				TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS, TestPackage.CROSS_REFERENCE_CONTENT__PARENT);
		}
		return contents;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public CrossReferenceContent getSingleContent() {
		return singleContent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetSingleContent(CrossReferenceContent newSingleContent, NotificationChain msgs) {
		final CrossReferenceContent oldSingleContent = singleContent;
		singleContent = newSingleContent;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT, oldSingleContent, newSingleContent);
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
	public void setSingleContent(CrossReferenceContent newSingleContent) {
		if (newSingleContent != singleContent) {
			NotificationChain msgs = null;
			if (singleContent != null) {
				msgs = ((InternalEObject) singleContent).eInverseRemove(this,
					TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT, CrossReferenceContent.class, msgs);
			}
			if (newSingleContent != null) {
				msgs = ((InternalEObject) newSingleContent).eInverseAdd(this,
					TestPackage.CROSS_REFERENCE_CONTENT__SINGLE_PARENT, CrossReferenceContent.class, msgs);
			}
			msgs = basicSetSingleContent(newSingleContent, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT,
				newSingleContent, newSingleContent));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			return ((InternalEList<InternalEObject>) (InternalEList<?>) getContents()).basicAdd(otherEnd, msgs);
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			if (singleContent != null) {
				msgs = ((InternalEObject) singleContent).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT, null, msgs);
			}
			return basicSetSingleContent((CrossReferenceContent) otherEnd, msgs);
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
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			return ((InternalEList<?>) getContents()).basicRemove(otherEnd, msgs);
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			return basicSetSingleContent(null, msgs);
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
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			return getContents();
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			return getSingleContent();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			getContents().clear();
			getContents().addAll((Collection<? extends CrossReferenceContent>) newValue);
			return;
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			setSingleContent((CrossReferenceContent) newValue);
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
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			getContents().clear();
			return;
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			setSingleContent((CrossReferenceContent) null);
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
		case TestPackage.CROSS_REFERENCE_CONTAINER__CONTENTS:
			return contents != null && !contents.isEmpty();
		case TestPackage.CROSS_REFERENCE_CONTAINER__SINGLE_CONTENT:
			return singleContent != null;
		}
		return super.eIsSet(featureID);
	}

} // CrossReferenceContainerImpl
