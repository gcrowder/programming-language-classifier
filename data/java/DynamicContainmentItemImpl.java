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
import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.spi.categorization.model.ECPAction;
import org.eclipse.emf.ecp.view.spi.categorization.model.impl.VCategorizableElementImpl;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Dynamic Containment Item</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentItemImpl#getDomainModel <em>Domain Model
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentItemImpl#getItems <em>Items</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentItemImpl#getComposite <em>Composite
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentItemImpl#getBaseItemIndex <em>Base Item
 * Index</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DynamicContainmentItemImpl extends VCategorizableElementImpl implements DynamicContainmentItem
{
	/**
	 * The cached value of the '{@link #getDomainModel() <em>Domain Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getDomainModel()
	 * @generated
	 * @ordered
	 */
	protected EObject domainModel;

	/**
	 * The cached value of the '{@link #getItems() <em>Items</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getItems()
	 * @generated
	 * @ordered
	 */
	protected EList<DynamicContainmentItem> items;

	/**
	 * The cached value of the '{@link #getComposite() <em>Composite</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getComposite()
	 * @generated
	 * @ordered
	 */
	protected VContainedElement composite;

	/**
	 * The default value of the '{@link #getBaseItemIndex() <em>Base Item Index</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getBaseItemIndex()
	 * @generated
	 * @ordered
	 */
	protected static final Integer BASE_ITEM_INDEX_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getBaseItemIndex() <em>Base Item Index</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getBaseItemIndex()
	 * @generated
	 * @ordered
	 */
	protected Integer baseItemIndex = BASE_ITEM_INDEX_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	protected DynamicContainmentItemImpl()
	{
		super();
		addLocalDependencyToLabelAdapter(ModelPackage.Literals.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL);
	}

	/**
	 * <!-- begin-user-doc -->
	 * .
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EClass eStaticClass()
	{
		return ModelPackage.Literals.DYNAMIC_CONTAINMENT_ITEM;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EObject getDomainModel()
	{
		if (domainModel != null && domainModel.eIsProxy()) {
			final InternalEObject oldDomainModel = (InternalEObject) domainModel;
			domainModel = eResolveProxy(oldDomainModel);
			if (domainModel != oldDomainModel) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL, oldDomainModel, domainModel));
				}
			}
		}
		return domainModel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EObject basicGetDomainModel()
	{
		return domainModel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setDomainModel(EObject newDomainModel)
	{
		final EObject oldDomainModel = domainModel;
		domainModel = newDomainModel;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL,
				oldDomainModel, domainModel));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<DynamicContainmentItem> getItems()
	{
		if (items == null) {
			items = new EObjectContainmentEList<DynamicContainmentItem>(DynamicContainmentItem.class, this,
				ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS);
		}
		return items;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public VContainedElement getComposite()
	{
		return composite;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetComposite(VContainedElement newComposite, NotificationChain msgs)
	{
		final VContainedElement oldComposite = composite;
		composite = newComposite;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE, oldComposite, newComposite);
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
	public void setComposite(VContainedElement newComposite)
	{
		if (newComposite != composite) {
			NotificationChain msgs = null;
			if (composite != null) {
				msgs = ((InternalEObject) composite).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE, null, msgs);
			}
			if (newComposite != null) {
				msgs = ((InternalEObject) newComposite).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE, null, msgs);
			}
			msgs = basicSetComposite(newComposite, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE,
				newComposite, newComposite));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Integer getBaseItemIndex() {
		return baseItemIndex;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setBaseItemIndex(Integer newBaseItemIndex) {
		final Integer oldBaseItemIndex = baseItemIndex;
		baseItemIndex = newBaseItemIndex;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_ITEM__BASE_ITEM_INDEX, oldBaseItemIndex, baseItemIndex));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
	{
		switch (featureID) {
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS:
			return ((InternalEList<?>) getItems()).basicRemove(otherEnd, msgs);
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE:
			return basicSetComposite(null, msgs);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL:
			if (resolve) {
				return getDomainModel();
			}
			return basicGetDomainModel();
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS:
			return getItems();
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE:
			return getComposite();
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__BASE_ITEM_INDEX:
			return getBaseItemIndex();
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
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL:
			setDomainModel((EObject) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS:
			getItems().clear();
			getItems().addAll((Collection<? extends DynamicContainmentItem>) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE:
			setComposite((VContainedElement) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__BASE_ITEM_INDEX:
			setBaseItemIndex((Integer) newValue);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL:
			setDomainModel((EObject) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS:
			getItems().clear();
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE:
			setComposite((VContainedElement) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__BASE_ITEM_INDEX:
			setBaseItemIndex(BASE_ITEM_INDEX_EDEFAULT);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__DOMAIN_MODEL:
			return domainModel != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__ITEMS:
			return items != null && !items.isEmpty();
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__COMPOSITE:
			return composite != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_ITEM__BASE_ITEM_INDEX:
			return BASE_ITEM_INDEX_EDEFAULT == null ? baseItemIndex != null : !BASE_ITEM_INDEX_EDEFAULT
				.equals(baseItemIndex);
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
		result.append(" (baseItemIndex: ");
		result.append(baseItemIndex);
		result.append(')');
		return result.toString();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement#getLabelObject()
	 */
	@Override
	public EObject getLabelObject() {
		return getDomainModel();
	}

	private List<ECPAction> ecpActions;

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement#getECPActions()
	 */
	@Override
	public List<ECPAction> getECPActions() {
		return ecpActions;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement#setECPActions(java.util.List)
	 */
	@Override
	public void setECPActions(List<ECPAction> actions) {
		ecpActions = actions;
	}

} // DynamicContainmentItemImpl
