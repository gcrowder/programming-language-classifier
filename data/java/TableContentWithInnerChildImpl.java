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
 *******************************************************************************/

package org.eclipse.emf.ecp.view.validation.test.model.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecp.view.validation.test.model.TableContent;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Table Content With Inner Child</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.TableContentWithInnerChildImpl#getInnerChild
 * <em>Inner Child</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.TableContentWithInnerChildImpl#getStuff <em>Stuff</em>
 * }</li>
 * </ul>
 *
 * @generated
 */
public class TableContentWithInnerChildImpl extends TableContentImpl implements TableContentWithInnerChild {
	/**
	 * The cached value of the '{@link #getInnerChild() <em>Inner Child</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getInnerChild()
	 * @generated
	 * @ordered
	 */
	protected TableContent innerChild;

	/**
	 * The default value of the '{@link #getStuff() <em>Stuff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getStuff()
	 * @generated
	 * @ordered
	 */
	protected static final String STUFF_EDEFAULT = null;
	/**
	 * The cached value of the '{@link #getStuff() <em>Stuff</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getStuff()
	 * @generated
	 * @ordered
	 */
	protected String stuff = STUFF_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected TableContentWithInnerChildImpl() {
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
		return TestPackage.Literals.TABLE_CONTENT_WITH_INNER_CHILD;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TableContent getInnerChild() {
		return innerChild;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetInnerChild(TableContent newInnerChild, NotificationChain msgs) {
		final TableContent oldInnerChild = innerChild;
		innerChild = newInnerChild;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD, oldInnerChild, newInnerChild);
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
	public void setInnerChild(TableContent newInnerChild) {
		if (newInnerChild != innerChild) {
			NotificationChain msgs = null;
			if (innerChild != null) {
				msgs = ((InternalEObject) innerChild).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD, null, msgs);
			}
			if (newInnerChild != null) {
				msgs = ((InternalEObject) newInnerChild).eInverseAdd(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD, null, msgs);
			}
			msgs = basicSetInnerChild(newInnerChild, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD, newInnerChild, newInnerChild));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getStuff() {
		return stuff;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setStuff(String newStuff) {
		final String oldStuff = stuff;
		stuff = newStuff;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__STUFF,
				oldStuff, stuff));
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
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD:
			return basicSetInnerChild(null, msgs);
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
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD:
			return getInnerChild();
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__STUFF:
			return getStuff();
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
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD:
			setInnerChild((TableContent) newValue);
			return;
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__STUFF:
			setStuff((String) newValue);
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
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD:
			setInnerChild((TableContent) null);
			return;
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__STUFF:
			setStuff(STUFF_EDEFAULT);
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
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__INNER_CHILD:
			return innerChild != null;
		case TestPackage.TABLE_CONTENT_WITH_INNER_CHILD__STUFF:
			return STUFF_EDEFAULT == null ? stuff != null : !STUFF_EDEFAULT.equals(stuff);
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
		result.append(" (stuff: ");
		result.append(stuff);
		result.append(')');
		return result.toString();
	}

} // TableContentWithInnerChildImpl
