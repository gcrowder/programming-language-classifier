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
package org.eclipse.emf.ecp.view.dynamictree.model;

import java.util.Map;

import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Test Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getId <em>Id</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getElements <em>Elements</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getParentId <em>Parent Id</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElement()
 * @model
 * @generated
 */
public interface TestElement extends EObject
{
	/**
	 * Returns the value of the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Id</em>' attribute isn't clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Id</em>' attribute.
	 * @see #setId(String)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElement_Id()
	 * @model
	 * @generated
	 */
	String getId();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getId <em>Id</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Id</em>' attribute.
	 * @see #getId()
	 * @generated
	 */
	void setId(String value);

	/**
	 * Returns the value of the '<em><b>Elements</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Elements</em>' containment reference list isn't clear, there really should be more of
	 * a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Elements</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElement_Elements()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestElement> getElements();

	/**
	 * Returns the value of the '<em><b>Parent Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Parent Id</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Parent Id</em>' attribute.
	 * @see #setParentId(String)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElement_ParentId()
	 * @model
	 * @generated
	 */
	String getParentId();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getParentId
	 * <em>Parent Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Parent Id</em>' attribute.
	 * @see #getParentId()
	 * @generated
	 */
	void setParentId(String value);

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
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElement_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement#getName <em>Name</em>}'
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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @model
	 * @generated
	 */
	boolean validate(DiagnosticChain diagnostic, Map<?, ?> context);

} // TestElement
