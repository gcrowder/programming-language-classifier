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
package org.eclipse.emf.ecp.test.university;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.FeatureMap;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Staff</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.test.university.Staff#getProfessors <em>Professors</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.test.university.Staff#getAssistants <em>Assistants</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.test.university.Staff#getStaff <em>Staff</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getStaff()
 * @model
 * @generated
 */
public interface Staff extends EObject {
	/**
	 * Returns the value of the '<em><b>Professors</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.test.university.Professor}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Professors</em>' containment reference list isn't clear, there really should be more
	 * of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Professors</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getStaff_Professors()
	 * @model containment="true" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="group='#staff'"
	 * @generated
	 */
	EList<Professor> getProfessors();

	/**
	 * Returns the value of the '<em><b>Assistants</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.test.university.Assistant}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Assistants</em>' containment reference list isn't clear, there really should be more
	 * of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Assistants</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getStaff_Assistants()
	 * @model containment="true" transient="true" volatile="true" derived="true"
	 *        extendedMetaData="group='#staff'"
	 * @generated
	 */
	EList<Assistant> getAssistants();

	/**
	 * Returns the value of the '<em><b>Staff</b></em>' attribute list.
	 * The list contents are of type {@link org.eclipse.emf.ecore.util.FeatureMap.Entry}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Staff</em>' attribute list isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Staff</em>' attribute list.
	 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getStaff_Staff()
	 * @model dataType="org.eclipse.emf.ecore.EFeatureMapEntry" many="true"
	 *        extendedMetaData="kind='group'"
	 * @generated
	 */
	FeatureMap getStaff();

} // Staff
