/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource - Generated code
 */
package org.eclipse.emf.ecp.test.university.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.BasicFeatureMap;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.emf.ecp.test.university.Assistant;
import org.eclipse.emf.ecp.test.university.Professor;
import org.eclipse.emf.ecp.test.university.Staff;
import org.eclipse.emf.ecp.test.university.UniversityPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Staff</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.test.university.impl.StaffImpl#getProfessors <em>Professors</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.test.university.impl.StaffImpl#getAssistants <em>Assistants</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.test.university.impl.StaffImpl#getStaff <em>Staff</em>}</li>
 * </ul>
 *
 * @generated
 */
public class StaffImpl extends MinimalEObjectImpl.Container implements Staff {
	/**
	 * The cached value of the '{@link #getStaff() <em>Staff</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see #getStaff()
	 * @generated
	 * @ordered
	 */
	protected FeatureMap staff;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected StaffImpl() {
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
		return UniversityPackage.Literals.STAFF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EList<Professor> getProfessors() {
		return getStaff().list(UniversityPackage.Literals.STAFF__PROFESSORS);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EList<Assistant> getAssistants() {
		return getStaff().list(UniversityPackage.Literals.STAFF__ASSISTANTS);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public FeatureMap getStaff() {
		if (staff == null) {
			staff = new BasicFeatureMap(this, UniversityPackage.STAFF__STAFF);
		}
		return staff;
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
		case UniversityPackage.STAFF__PROFESSORS:
			return ((InternalEList<?>) getProfessors()).basicRemove(otherEnd, msgs);
		case UniversityPackage.STAFF__ASSISTANTS:
			return ((InternalEList<?>) getAssistants()).basicRemove(otherEnd, msgs);
		case UniversityPackage.STAFF__STAFF:
			return ((InternalEList<?>) getStaff()).basicRemove(otherEnd, msgs);
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
		case UniversityPackage.STAFF__PROFESSORS:
			return getProfessors();
		case UniversityPackage.STAFF__ASSISTANTS:
			return getAssistants();
		case UniversityPackage.STAFF__STAFF:
			if (coreType) {
				return getStaff();
			}
			return ((FeatureMap.Internal) getStaff()).getWrapper();
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
		case UniversityPackage.STAFF__PROFESSORS:
			getProfessors().clear();
			getProfessors().addAll((Collection<? extends Professor>) newValue);
			return;
		case UniversityPackage.STAFF__ASSISTANTS:
			getAssistants().clear();
			getAssistants().addAll((Collection<? extends Assistant>) newValue);
			return;
		case UniversityPackage.STAFF__STAFF:
			((FeatureMap.Internal) getStaff()).set(newValue);
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
		case UniversityPackage.STAFF__PROFESSORS:
			getProfessors().clear();
			return;
		case UniversityPackage.STAFF__ASSISTANTS:
			getAssistants().clear();
			return;
		case UniversityPackage.STAFF__STAFF:
			getStaff().clear();
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
		case UniversityPackage.STAFF__PROFESSORS:
			return !getProfessors().isEmpty();
		case UniversityPackage.STAFF__ASSISTANTS:
			return !getAssistants().isEmpty();
		case UniversityPackage.STAFF__STAFF:
			return staff != null && !staff.isEmpty();
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
		result.append(" (staff: "); //$NON-NLS-1$
		result.append(staff);
		result.append(')');
		return result.toString();
	}

} // StaffImpl
