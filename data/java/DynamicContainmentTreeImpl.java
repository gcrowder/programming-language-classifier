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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.spi.categorization.model.impl.VCategoryImpl;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Dynamic Containment Tree</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeImpl#getDomainModel <em>Domain Model
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeImpl#getChildReference <em>Child
 * Reference</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeImpl#getPathToRoot <em>Path To Root
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeImpl#getChildComposite <em>Child
 * Composite</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeImpl#getItems <em>Items</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DynamicContainmentTreeImpl extends VCategoryImpl implements DynamicContainmentTree
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
	 * The cached value of the '{@link #getChildReference() <em>Child Reference</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getChildReference()
	 * @generated
	 * @ordered
	 */
	protected EReference childReference;

	/**
	 * The cached value of the '{@link #getPathToRoot() <em>Path To Root</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPathToRoot()
	 * @generated
	 * @ordered
	 */
	protected EList<EReference> pathToRoot;

	/**
	 * The cached value of the '{@link #getChildComposite() <em>Child Composite</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getChildComposite()
	 * @generated
	 * @ordered
	 */
	protected VContainedElement childComposite;

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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	protected DynamicContainmentTreeImpl()
	{
		super();
		addLocalDependencyToLabelAdapter(ModelPackage.Literals.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL);
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
		return ModelPackage.Literals.DYNAMIC_CONTAINMENT_TREE;
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
						ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL, oldDomainModel, domainModel));
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
			eNotify(new ENotificationImpl(this, Notification.SET, ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL,
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
	public EReference getChildReference()
	{
		if (childReference != null && childReference.eIsProxy()) {
			final InternalEObject oldChildReference = (InternalEObject) childReference;
			childReference = (EReference) eResolveProxy(oldChildReference);
			if (childReference != oldChildReference) {
				if (eNotificationRequired()) {
					eNotify(new ENotificationImpl(this, Notification.RESOLVE,
						ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE, oldChildReference, childReference));
				}
			}
		}
		return childReference;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EReference basicGetChildReference()
	{
		return childReference;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public void setChildReference(EReference newChildReference)
	{
		final EReference oldChildReference = childReference;
		childReference = newChildReference;
		if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE, oldChildReference, childReference));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<EReference> getPathToRoot()
	{
		if (pathToRoot == null) {
			pathToRoot = new EObjectResolvingEList<EReference>(EReference.class, this,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE__PATH_TO_ROOT);
		}
		return pathToRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public VContainedElement getChildComposite()
	{
		return childComposite;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetChildComposite(VContainedElement newChildComposite, NotificationChain msgs)
	{
		final VContainedElement oldChildComposite = childComposite;
		childComposite = newChildComposite;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE, oldChildComposite, newChildComposite);
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
	public void setChildComposite(VContainedElement newChildComposite)
	{
		if (newChildComposite != childComposite) {
			NotificationChain msgs = null;
			if (childComposite != null) {
				msgs = ((InternalEObject) childComposite).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE, null, msgs);
			}
			if (newChildComposite != null) {
				msgs = ((InternalEObject) newChildComposite).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE, null, msgs);
			}
			msgs = basicSetChildComposite(newChildComposite, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE, newChildComposite, newChildComposite));
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
				ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS);
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
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
	{
		switch (featureID) {
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE:
			return basicSetChildComposite(null, msgs);
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS:
			return ((InternalEList<?>) getItems()).basicRemove(otherEnd, msgs);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL:
			if (resolve) {
				return getDomainModel();
			}
			return basicGetDomainModel();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE:
			if (resolve) {
				return getChildReference();
			}
			return basicGetChildReference();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__PATH_TO_ROOT:
			return getPathToRoot();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE:
			return getChildComposite();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS:
			return getItems();
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL:
			setDomainModel((EObject) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE:
			setChildReference((EReference) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__PATH_TO_ROOT:
			getPathToRoot().clear();
			getPathToRoot().addAll((Collection<? extends EReference>) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE:
			setChildComposite((VContainedElement) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS:
			getItems().clear();
			getItems().addAll((Collection<? extends DynamicContainmentItem>) newValue);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL:
			setDomainModel((EObject) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE:
			setChildReference((EReference) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__PATH_TO_ROOT:
			getPathToRoot().clear();
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE:
			setChildComposite((VContainedElement) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS:
			getItems().clear();
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__DOMAIN_MODEL:
			return domainModel != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_REFERENCE:
			return childReference != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__PATH_TO_ROOT:
			return pathToRoot != null && !pathToRoot.isEmpty();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__CHILD_COMPOSITE:
			return childComposite != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE__ITEMS:
			return items != null && !items.isEmpty();
		}
		return super.eIsSet(featureID);
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

} // DynamicContainmentTreeImpl
