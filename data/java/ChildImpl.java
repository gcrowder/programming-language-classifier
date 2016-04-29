/**
 */
package org.eclipse.emf.ecp.view.keyattribute.test.example.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecp.view.keyattribute.test.example.Child;
import org.eclipse.emf.ecp.view.keyattribute.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.keyattribute.test.example.IntermediateTarget;
import org.eclipse.emf.ecp.view.keyattribute.test.example.KeyContainer;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Child</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link org.eclipse.emf.ecp.view.keyattribute.test.example.impl.ChildImpl#getIntermediateTarget <em>Intermediate Target</em>}</li>
 *   <li>{@link org.eclipse.emf.ecp.view.keyattribute.test.example.impl.ChildImpl#getKey <em>Key</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ChildImpl extends MinimalEObjectImpl.Container implements Child {
	/**
	 * The cached value of the '{@link #getIntermediateTarget() <em>Intermediate Target</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntermediateTarget()
	 * @generated
	 * @ordered
	 */
	protected IntermediateTarget intermediateTarget;

	/**
	 * The cached value of the '{@link #getKey() <em>Key</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKey()
	 * @generated
	 * @ordered
	 */
	protected KeyContainer key;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ChildImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExamplePackage.Literals.CHILD;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public IntermediateTarget getIntermediateTarget() {
		return intermediateTarget;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetIntermediateTarget(IntermediateTarget newIntermediateTarget, NotificationChain msgs) {
		IntermediateTarget oldIntermediateTarget = intermediateTarget;
		intermediateTarget = newIntermediateTarget;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExamplePackage.CHILD__INTERMEDIATE_TARGET, oldIntermediateTarget, newIntermediateTarget);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setIntermediateTarget(IntermediateTarget newIntermediateTarget) {
		if (newIntermediateTarget != intermediateTarget) {
			NotificationChain msgs = null;
			if (intermediateTarget != null)
				msgs = ((InternalEObject)intermediateTarget).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ExamplePackage.CHILD__INTERMEDIATE_TARGET, null, msgs);
			if (newIntermediateTarget != null)
				msgs = ((InternalEObject)newIntermediateTarget).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ExamplePackage.CHILD__INTERMEDIATE_TARGET, null, msgs);
			msgs = basicSetIntermediateTarget(newIntermediateTarget, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExamplePackage.CHILD__INTERMEDIATE_TARGET, newIntermediateTarget, newIntermediateTarget));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public KeyContainer getKey() {
		return key;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetKey(KeyContainer newKey, NotificationChain msgs) {
		KeyContainer oldKey = key;
		key = newKey;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExamplePackage.CHILD__KEY, oldKey, newKey);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setKey(KeyContainer newKey) {
		if (newKey != key) {
			NotificationChain msgs = null;
			if (key != null)
				msgs = ((InternalEObject)key).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ExamplePackage.CHILD__KEY, null, msgs);
			if (newKey != null)
				msgs = ((InternalEObject)newKey).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ExamplePackage.CHILD__KEY, null, msgs);
			msgs = basicSetKey(newKey, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExamplePackage.CHILD__KEY, newKey, newKey));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExamplePackage.CHILD__INTERMEDIATE_TARGET:
				return basicSetIntermediateTarget(null, msgs);
			case ExamplePackage.CHILD__KEY:
				return basicSetKey(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExamplePackage.CHILD__INTERMEDIATE_TARGET:
				return getIntermediateTarget();
			case ExamplePackage.CHILD__KEY:
				return getKey();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExamplePackage.CHILD__INTERMEDIATE_TARGET:
				setIntermediateTarget((IntermediateTarget)newValue);
				return;
			case ExamplePackage.CHILD__KEY:
				setKey((KeyContainer)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExamplePackage.CHILD__INTERMEDIATE_TARGET:
				setIntermediateTarget((IntermediateTarget)null);
				return;
			case ExamplePackage.CHILD__KEY:
				setKey((KeyContainer)null);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExamplePackage.CHILD__INTERMEDIATE_TARGET:
				return intermediateTarget != null;
			case ExamplePackage.CHILD__KEY:
				return key != null;
		}
		return super.eIsSet(featureID);
	}

} // ChildImpl
