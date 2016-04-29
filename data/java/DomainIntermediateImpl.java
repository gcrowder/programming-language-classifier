/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 */
package org.eclipse.emf.ecp.view.dynamictree.model.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Domain Intermediate</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DomainIntermediateImpl#getTestElementContainer <em>Test
 * Element Container</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DomainIntermediateImpl extends EObjectImpl implements DomainIntermediate
{
	/**
	 * The cached value of the '{@link #getTestElementContainer() <em>Test Element Container</em>}' containment
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getTestElementContainer()
	 * @generated
	 * @ordered
	 */
	protected TestElementContainer testElementContainer;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected DomainIntermediateImpl()
	{
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EClass eStaticClass()
	{
		return ModelPackage.Literals.DOMAIN_INTERMEDIATE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public TestElementContainer getTestElementContainer()
	{
		return testElementContainer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetTestElementContainer(TestElementContainer newTestElementContainer,
		NotificationChain msgs) {
		final TestElementContainer oldTestElementContainer = testElementContainer;
		testElementContainer = newTestElementContainer;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER, oldTestElementContainer,
				newTestElementContainer);
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
	public void setTestElementContainer(TestElementContainer newTestElementContainer)
	{
		if (newTestElementContainer != testElementContainer) {
			NotificationChain msgs = null;
			if (testElementContainer != null) {
				msgs = ((InternalEObject) testElementContainer).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER, null, msgs);
			}
			if (newTestElementContainer != null) {
				msgs = ((InternalEObject) newTestElementContainer).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER, null, msgs);
			}
			msgs = basicSetTestElementContainer(newTestElementContainer, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER, newTestElementContainer,
				newTestElementContainer));
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
		case ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER:
			return basicSetTestElementContainer(null, msgs);
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
	public Object eGet(int featureID, boolean resolve, boolean coreType)
	{
		switch (featureID) {
		case ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER:
			return getTestElementContainer();
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
	public void eSet(int featureID, Object newValue)
	{
		switch (featureID) {
		case ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER:
			setTestElementContainer((TestElementContainer) newValue);
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
	public void eUnset(int featureID)
	{
		switch (featureID) {
		case ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER:
			setTestElementContainer((TestElementContainer) null);
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
	public boolean eIsSet(int featureID)
	{
		switch (featureID) {
		case ModelPackage.DOMAIN_INTERMEDIATE__TEST_ELEMENT_CONTAINER:
			return testElementContainer != null;
		}
		return super.eIsSet(featureID);
	}

} // DomainIntermediateImpl
