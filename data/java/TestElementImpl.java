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

import java.util.Collection;
import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElement;
import org.eclipse.emf.ecp.view.dynamictree.model.util.ModelValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Test Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.TestElementImpl#getId <em>Id</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.TestElementImpl#getElements <em>Elements</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.TestElementImpl#getParentId <em>Parent Id</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.TestElementImpl#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TestElementImpl extends EObjectImpl implements TestElement
{
	/**
	 * The default value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected static final String ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected String id = ID_EDEFAULT;

	/**
	 * The cached value of the '{@link #getElements() <em>Elements</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getElements()
	 * @generated
	 * @ordered
	 */
	protected EList<TestElement> elements;

	/**
	 * The default value of the '{@link #getParentId() <em>Parent Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getParentId()
	 * @generated
	 * @ordered
	 */
	protected static final String PARENT_ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getParentId() <em>Parent Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getParentId()
	 * @generated
	 * @ordered
	 */
	protected String parentId = PARENT_ID_EDEFAULT;

	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected TestElementImpl()
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
		return ModelPackage.Literals.TEST_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getId()
	{
		return id;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setId(String newId)
	{
		final String oldId = id;
		id = newId;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.TEST_ELEMENT__ID, oldId, id));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<TestElement> getElements()
	{
		if (elements == null) {
			elements = new EObjectContainmentEList<TestElement>(TestElement.class, this,
				ModelPackage.TEST_ELEMENT__ELEMENTS);
		}
		return elements;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getParentId()
	{
		return parentId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setParentId(String newParentId)
	{
		final String oldParentId = parentId;
		parentId = newParentId;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.TEST_ELEMENT__PARENT_ID, oldParentId,
				parentId));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.TEST_ELEMENT__NAME, oldName, name));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 *
	 * Validates this test element.
	 *
	 * @param diagnostic
	 *            the diagnostic chain
	 * @param context
	 *            the context
	 *
	 * @return {@code true} if this element is valid, {@code false} otherwise
	 *
	 *         <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	@Override
	public boolean validate(DiagnosticChain diagnostic, Map<?, ?> context) {
		if (getName() == null || getName().length() == 0) {
			if (diagnostic != null) {
				diagnostic.add
					(new BasicDiagnostic
					(Diagnostic.ERROR,
						ModelValidator.DIAGNOSTIC_SOURCE,
						ModelValidator.TEST_ELEMENT__VALIDATE,
						"Invalid name (custom validation rule).",
						new Object[] { this, ModelPackage.eINSTANCE.getTestElement_Name()
						}));
			}
			return false;
		}
		return true;
	}

	/**
	 * <!-- begin-user-doc -->
	 * .
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
	{
		switch (featureID) {
		case ModelPackage.TEST_ELEMENT__ELEMENTS:
			return ((InternalEList<?>) getElements()).basicRemove(otherEnd, msgs);
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
		case ModelPackage.TEST_ELEMENT__ID:
			return getId();
		case ModelPackage.TEST_ELEMENT__ELEMENTS:
			return getElements();
		case ModelPackage.TEST_ELEMENT__PARENT_ID:
			return getParentId();
		case ModelPackage.TEST_ELEMENT__NAME:
			return getName();
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
	public void eSet(int featureID, Object newValue)
	{
		switch (featureID) {
		case ModelPackage.TEST_ELEMENT__ID:
			setId((String) newValue);
			return;
		case ModelPackage.TEST_ELEMENT__ELEMENTS:
			getElements().clear();
			getElements().addAll((Collection<? extends TestElement>) newValue);
			return;
		case ModelPackage.TEST_ELEMENT__PARENT_ID:
			setParentId((String) newValue);
			return;
		case ModelPackage.TEST_ELEMENT__NAME:
			setName((String) newValue);
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
		case ModelPackage.TEST_ELEMENT__ID:
			setId(ID_EDEFAULT);
			return;
		case ModelPackage.TEST_ELEMENT__ELEMENTS:
			getElements().clear();
			return;
		case ModelPackage.TEST_ELEMENT__PARENT_ID:
			setParentId(PARENT_ID_EDEFAULT);
			return;
		case ModelPackage.TEST_ELEMENT__NAME:
			setName(NAME_EDEFAULT);
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
		case ModelPackage.TEST_ELEMENT__ID:
			return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
		case ModelPackage.TEST_ELEMENT__ELEMENTS:
			return elements != null && !elements.isEmpty();
		case ModelPackage.TEST_ELEMENT__PARENT_ID:
			return PARENT_ID_EDEFAULT == null ? parentId != null : !PARENT_ID_EDEFAULT.equals(parentId);
		case ModelPackage.TEST_ELEMENT__NAME:
			return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
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
	public String toString()
	{
		if (eIsProxy()) {
			return super.toString();
		}

		final StringBuffer result = new StringBuffer(super.toString());
		result.append(" (id: ");
		result.append(id);
		result.append(", parentId: ");
		result.append(parentId);
		result.append(", name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} // TestElementImpl
