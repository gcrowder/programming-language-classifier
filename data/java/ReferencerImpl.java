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
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.Referencer;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Referencer</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ReferencerImpl#getReferencedContent
 * <em>Referenced Content</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ReferencerImpl extends EObjectImpl implements Referencer {
	/**
	 * The cached value of the '{@link #getReferencedContent() <em>Referenced Content</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getReferencedContent()
	 * @generated
	 * @ordered
	 */
	protected Computer referencedContent;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected ReferencerImpl() {
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
		return TestPackage.Literals.REFERENCER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Computer getReferencedContent() {
		if (referencedContent != null && referencedContent.eIsProxy()) {
			final InternalEObject oldReferencedContent = (InternalEObject) referencedContent;
			referencedContent = (Computer) eResolveProxy(oldReferencedContent);
			if (referencedContent != oldReferencedContent) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						TestPackage.REFERENCER__REFERENCED_CONTENT, oldReferencedContent, referencedContent));
				}
			}
		}
		return referencedContent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Computer basicGetReferencedContent() {
		return referencedContent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setReferencedContent(Computer newReferencedContent) {
		final Computer oldReferencedContent = referencedContent;
		referencedContent = newReferencedContent;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.REFERENCER__REFERENCED_CONTENT,
				oldReferencedContent, referencedContent));
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
		case TestPackage.REFERENCER__REFERENCED_CONTENT:
			if (resolve) {
				return getReferencedContent();
			}
			return basicGetReferencedContent();
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
		case TestPackage.REFERENCER__REFERENCED_CONTENT:
			setReferencedContent((Computer) newValue);
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
		case TestPackage.REFERENCER__REFERENCED_CONTENT:
			setReferencedContent((Computer) null);
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
		case TestPackage.REFERENCER__REFERENCED_CONTENT:
			return referencedContent != null;
		}
		return super.eIsSet(featureID);
	}

} // ReferencerImpl
