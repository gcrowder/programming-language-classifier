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
package org.eclipse.emf.ecp.view.validation.test.model;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Computer</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getMainboard <em>Mainboard</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getName <em>Name</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getPowerBlock <em>Power Block</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getComputer()
 * @model
 * @generated
 */
public interface Computer extends EObject {
	/**
	 * Returns the value of the '<em><b>Mainboard</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Mainboard</em>' reference isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Mainboard</em>' containment reference.
	 * @see #setMainboard(Mainboard)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getComputer_Mainboard()
	 * @model containment="true"
	 * @generated
	 */
	Mainboard getMainboard();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getMainboard
	 * <em>Mainboard</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Mainboard</em>' containment reference.
	 * @see #getMainboard()
	 * @generated
	 */
	void setMainboard(Mainboard value);

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getComputer_Name()
	 * @model required="true"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getName <em>Name</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Power Block</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Power Block</em>' reference isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Power Block</em>' containment reference.
	 * @see #setPowerBlock(PowerBlock)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getComputer_PowerBlock()
	 * @model containment="true"
	 * @generated
	 */
	PowerBlock getPowerBlock();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Computer#getPowerBlock
	 * <em>Power Block</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Power Block</em>' containment reference.
	 * @see #getPowerBlock()
	 * @generated
	 */
	void setPowerBlock(PowerBlock value);

} // Computer
