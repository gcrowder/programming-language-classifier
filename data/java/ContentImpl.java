/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 *******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test.model.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecp.view.validation.test.model.Content;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Content</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ContentImpl#getUniqueAttribute
 * <em>Unique Attribute</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ContentImpl#getSecondAttribute
 * <em>Second Attribute</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ContentImpl extends EObjectImpl implements Content {
	/**
	 * The default value of the '{@link #getUniqueAttribute() <em>Unique Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getUniqueAttribute()
	 * @generated
	 * @ordered
	 */
	protected static final String UNIQUE_ATTRIBUTE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUniqueAttribute() <em>Unique Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getUniqueAttribute()
	 * @generated
	 * @ordered
	 */
	protected String uniqueAttribute = UNIQUE_ATTRIBUTE_EDEFAULT;

	/**
	 * The default value of the '{@link #getSecondAttribute() <em>Second Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getSecondAttribute()
	 * @generated
	 * @ordered
	 */
	protected static final String SECOND_ATTRIBUTE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSecondAttribute() <em>Second Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getSecondAttribute()
	 * @generated
	 * @ordered
	 */
	protected String secondAttribute = SECOND_ATTRIBUTE_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected ContentImpl() {
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
		return TestPackage.Literals.CONTENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getUniqueAttribute() {
		return uniqueAttribute;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setUniqueAttribute(String newUniqueAttribute) {
		final String oldUniqueAttribute = uniqueAttribute;
		uniqueAttribute = newUniqueAttribute;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.CONTENT__UNIQUE_ATTRIBUTE,
				oldUniqueAttribute, uniqueAttribute));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getSecondAttribute() {
		return secondAttribute;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setSecondAttribute(String newSecondAttribute) {
		final String oldSecondAttribute = secondAttribute;
		secondAttribute = newSecondAttribute;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.CONTENT__SECOND_ATTRIBUTE,
				oldSecondAttribute, secondAttribute));
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
		case TestPackage.CONTENT__UNIQUE_ATTRIBUTE:
			return getUniqueAttribute();
		case TestPackage.CONTENT__SECOND_ATTRIBUTE:
			return getSecondAttribute();
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
		case TestPackage.CONTENT__UNIQUE_ATTRIBUTE:
			setUniqueAttribute((String) newValue);
			return;
		case TestPackage.CONTENT__SECOND_ATTRIBUTE:
			setSecondAttribute((String) newValue);
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
		case TestPackage.CONTENT__UNIQUE_ATTRIBUTE:
			setUniqueAttribute(UNIQUE_ATTRIBUTE_EDEFAULT);
			return;
		case TestPackage.CONTENT__SECOND_ATTRIBUTE:
			setSecondAttribute(SECOND_ATTRIBUTE_EDEFAULT);
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
		case TestPackage.CONTENT__UNIQUE_ATTRIBUTE:
			return UNIQUE_ATTRIBUTE_EDEFAULT == null ? uniqueAttribute != null
				: !UNIQUE_ATTRIBUTE_EDEFAULT.equals(uniqueAttribute);
		case TestPackage.CONTENT__SECOND_ATTRIBUTE:
			return SECOND_ATTRIBUTE_EDEFAULT == null ? secondAttribute != null
				: !SECOND_ATTRIBUTE_EDEFAULT.equals(secondAttribute);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) {
			return super.toString();
		}

		final StringBuffer result = new StringBuffer(super.toString());
		result.append(" (uniqueAttribute: ");
		result.append(uniqueAttribute);
		result.append(", secondAttribute: ");
		result.append(secondAttribute);
		result.append(')');
		return result.toString();
	}

} // ContentImpl
