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
import java.util.Iterator;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.spi.model.DomainModelReferenceChangeListener;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeNotification;
import org.eclipse.emf.ecp.view.spi.model.SettingPath;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Dynamic Containment Tree Domain Model Reference</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>
 * {@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeDomainModelReferenceImpl#getChangeListener
 * <em>Change Listener</em>}</li>
 * <li>
 * {@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeDomainModelReferenceImpl#getPathFromRoot
 * <em>Path From Root</em>}</li>
 * <li>
 * {@link org.eclipse.emf.ecp.view.dynamictree.model.impl.DynamicContainmentTreeDomainModelReferenceImpl#getPathFromBase
 * <em>Path From Base</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DynamicContainmentTreeDomainModelReferenceImpl extends EObjectImpl implements
	DynamicContainmentTreeDomainModelReference {
	/**
	 * The cached value of the '{@link #getChangeListener() <em>Change Listener</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getChangeListener()
	 * @generated
	 * @ordered
	 */
	protected EList<DomainModelReferenceChangeListener> changeListener;

	/**
	 * The cached value of the '{@link #getPathFromRoot() <em>Path From Root</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPathFromRoot()
	 * @generated
	 * @ordered
	 */
	protected VDomainModelReference pathFromRoot;

	/**
	 * The cached value of the '{@link #getPathFromBase() <em>Path From Base</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPathFromBase()
	 * @generated
	 * @ordered
	 */
	protected VDomainModelReference pathFromBase;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected DynamicContainmentTreeDomainModelReferenceImpl() {
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
		return ModelPackage.Literals.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EList<DomainModelReferenceChangeListener> getChangeListener() {
		if (changeListener == null) {
			changeListener = new EDataTypeUniqueEList<DomainModelReferenceChangeListener>(
				DomainModelReferenceChangeListener.class, this,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__CHANGE_LISTENER);
		}
		return changeListener;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public VDomainModelReference getPathFromRoot() {
		return pathFromRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetPathFromRoot(VDomainModelReference newPathFromRoot, NotificationChain msgs) {
		final VDomainModelReference oldPathFromRoot = pathFromRoot;
		pathFromRoot = newPathFromRoot;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT, oldPathFromRoot,
				newPathFromRoot);
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
	public void setPathFromRoot(VDomainModelReference newPathFromRoot) {
		if (newPathFromRoot != pathFromRoot) {
			NotificationChain msgs = null;
			if (pathFromRoot != null) {
				msgs = ((InternalEObject) pathFromRoot).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT, null, msgs);
			}
			if (newPathFromRoot != null) {
				msgs = ((InternalEObject) newPathFromRoot).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT, null, msgs);
			}
			msgs = basicSetPathFromRoot(newPathFromRoot, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT, newPathFromRoot,
				newPathFromRoot));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public VDomainModelReference getPathFromBase() {
		return pathFromBase;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetPathFromBase(VDomainModelReference newPathFromBase, NotificationChain msgs) {
		final VDomainModelReference oldPathFromBase = pathFromBase;
		pathFromBase = newPathFromBase;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE, oldPathFromBase,
				newPathFromBase);
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
	public void setPathFromBase(VDomainModelReference newPathFromBase) {
		if (newPathFromBase != pathFromBase) {
			NotificationChain msgs = null;
			if (pathFromBase != null) {
				msgs = ((InternalEObject) pathFromBase).eInverseRemove(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE, null, msgs);
			}
			if (newPathFromBase != null) {
				msgs = ((InternalEObject) newPathFromBase).eInverseAdd(this, EOPPOSITE_FEATURE_BASE
					- ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE, null, msgs);
			}
			msgs = basicSetPathFromBase(newPathFromBase, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		}
		else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET,
				ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE, newPathFromBase,
				newPathFromBase));
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT:
			return basicSetPathFromRoot(null, msgs);
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE:
			return basicSetPathFromBase(null, msgs);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__CHANGE_LISTENER:
			return getChangeListener();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT:
			return getPathFromRoot();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE:
			return getPathFromBase();
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__CHANGE_LISTENER:
			getChangeListener().clear();
			getChangeListener().addAll((Collection<? extends DomainModelReferenceChangeListener>) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT:
			setPathFromRoot((VDomainModelReference) newValue);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE:
			setPathFromBase((VDomainModelReference) newValue);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__CHANGE_LISTENER:
			getChangeListener().clear();
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT:
			setPathFromRoot((VDomainModelReference) null);
			return;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE:
			setPathFromBase((VDomainModelReference) null);
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
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__CHANGE_LISTENER:
			return changeListener != null && !changeListener.isEmpty();
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_ROOT:
			return pathFromRoot != null;
		case ModelPackage.DYNAMIC_CONTAINMENT_TREE_DOMAIN_MODEL_REFERENCE__PATH_FROM_BASE:
			return pathFromBase != null;
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
		result.append(" (changeListener: ");
		result.append(changeListener);
		result.append(')');
		return result.toString();
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.model.VDomainModelReference#init(EObject)
	 *
	 * @generated NOT
	 */
	@Override
	@SuppressWarnings("unchecked")
	public boolean init(EObject eObject) {
		boolean init = getPathFromRoot().init(eObject);
		final DynamicContainmentItem packingItem = getPackingItem();
		if (packingItem == null || packingItem.getBaseItemIndex() == null || packingItem.getBaseItemIndex() < 0) {
			return false;
		}
		final EList<EObject> eobjects = (EList<EObject>) getPathFromRoot().getIterator().next().get(true);
		if (eobjects.size() < packingItem.getBaseItemIndex()) {
			return false;
		}
		init = getPathFromBase().init(eobjects.get(packingItem.getBaseItemIndex()));

		return init;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.model.VDomainModelReference#getIterator()
	 *
	 * @generated NOT
	 */
	@Override
	public Iterator<Setting> getIterator() {
		return getPathFromBase().getIterator();
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.model.VDomainModelReference#getEStructuralFeatureIterator()
	 *
	 * @generated NOT
	 */
	@Override
	public Iterator<EStructuralFeature> getEStructuralFeatureIterator() {
		return getPathFromBase().getEStructuralFeatureIterator();
	}

	/**
	 *
	 * @generated NOT
	 */
	private DynamicContainmentItem getPackingItem() {

		EObject parent = eContainer();
		while (!DynamicContainmentItem.class.isInstance(parent) && parent != null) {
			parent = parent.eContainer();
		}
		if (parent == null) {
			return null;
		}
		final DynamicContainmentItem packingItem = (DynamicContainmentItem) parent;
		return packingItem;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.model.VDomainModelReference#getFullPathIterator()
	 */
	@Override
	public Iterator<SettingPath> getFullPathIterator() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.model.ModelChangeListener#notifyChange(org.eclipse.emf.ecp.view.spi.model.ModelChangeNotification)
	 */
	@Override
	public void notifyChange(ModelChangeNotification notification) {
		// TODO Auto-generated method stub

	}

} // DynamicContainmentTreeDomainModelReferenceImpl
