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
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.Mainboard;
import org.eclipse.emf.ecp.view.validation.test.model.PowerBlock;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Computer</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ComputerImpl#getMainboard <em>Mainboard</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ComputerImpl#getName <em>Name</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.impl.ComputerImpl#getPowerBlock <em>Power Block</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ComputerImpl extends EObjectImpl implements Computer {
	/**
	 * The cached value of the '{@link #getMainboard() <em>Mainboard</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getMainboard()
	 * @generated
	 * @ordered
	 */
	protected Mainboard mainboard;

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
	 * The cached value of the '{@link #getPowerBlock() <em>Power Block</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getPowerBlock()
	 * @generated
	 * @ordered
	 */
	protected PowerBlock powerBlock;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected ComputerImpl() {
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
		return TestPackage.Literals.COMPUTER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public Mainboard getMainboard() {
		return mainboard;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetMainboard(Mainboard newMainboard, NotificationChain msgs) {
		final Mainboard oldMainboard = mainboard;
		mainboard = newMainboard;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.COMPUTER__MAINBOARD, oldMainboard, newMainboard);
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
	public void setMainboard(Mainboard newMainboard) {
		if (newMainboard != mainboard) {
			NotificationChain msgs = null;
			if (mainboard != null) {
				msgs = ((InternalEObject) mainboard).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.COMPUTER__MAINBOARD, null, msgs);
			}
			if (newMainboard != null) {
				msgs = ((InternalEObject) newMainboard).eInverseAdd(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.COMPUTER__MAINBOARD, null, msgs);
			}
			msgs = basicSetMainboard(newMainboard, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.COMPUTER__MAINBOARD, newMainboard,
				newMainboard));
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
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.COMPUTER__NAME, oldName, name));
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public PowerBlock getPowerBlock() {
		return powerBlock;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public NotificationChain basicSetPowerBlock(PowerBlock newPowerBlock, NotificationChain msgs) {
		final PowerBlock oldPowerBlock = powerBlock;
		powerBlock = newPowerBlock;
		if (eNotificationRequired()) {
			final ENotificationImpl notification = new ENotificationImpl(this, Notification.SET,
				TestPackage.COMPUTER__POWER_BLOCK, oldPowerBlock, newPowerBlock);
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
	public void setPowerBlock(PowerBlock newPowerBlock) {
		if (newPowerBlock != powerBlock) {
			NotificationChain msgs = null;
			if (powerBlock != null) {
				msgs = ((InternalEObject) powerBlock).eInverseRemove(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.COMPUTER__POWER_BLOCK, null, msgs);
			}
			if (newPowerBlock != null) {
				msgs = ((InternalEObject) newPowerBlock).eInverseAdd(this,
					EOPPOSITE_FEATURE_BASE - TestPackage.COMPUTER__POWER_BLOCK, null, msgs);
			}
			msgs = basicSetPowerBlock(newPowerBlock, msgs);
			if (msgs != null) {
				msgs.dispatch();
			}
		} else if (eNotificationRequired()) {
			eNotify(new ENotificationImpl(this, Notification.SET, TestPackage.COMPUTER__POWER_BLOCK, newPowerBlock,
				newPowerBlock));
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
		case TestPackage.COMPUTER__MAINBOARD:
			return basicSetMainboard(null, msgs);
		case TestPackage.COMPUTER__POWER_BLOCK:
			return basicSetPowerBlock(null, msgs);
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
		case TestPackage.COMPUTER__MAINBOARD:
			return getMainboard();
		case TestPackage.COMPUTER__NAME:
			return getName();
		case TestPackage.COMPUTER__POWER_BLOCK:
			return getPowerBlock();
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
		case TestPackage.COMPUTER__MAINBOARD:
			setMainboard((Mainboard) newValue);
			return;
		case TestPackage.COMPUTER__NAME:
			setName((String) newValue);
			return;
		case TestPackage.COMPUTER__POWER_BLOCK:
			setPowerBlock((PowerBlock) newValue);
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
		case TestPackage.COMPUTER__MAINBOARD:
			setMainboard((Mainboard) null);
			return;
		case TestPackage.COMPUTER__NAME:
			setName(NAME_EDEFAULT);
			return;
		case TestPackage.COMPUTER__POWER_BLOCK:
			setPowerBlock((PowerBlock) null);
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
		case TestPackage.COMPUTER__MAINBOARD:
			return mainboard != null;
		case TestPackage.COMPUTER__NAME:
			return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
		case TestPackage.COMPUTER__POWER_BLOCK:
			return powerBlock != null;
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
		result.append(" (name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} // ComputerImpl
