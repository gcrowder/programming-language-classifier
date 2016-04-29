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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Test Element Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer#getTestElements <em>Test Elements</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer#getId <em>Id</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElementContainer()
 * @model
 * @generated
 */
public interface TestElementContainer extends EObject
{
	/**
	 * Returns the value of the '<em><b>Test Elements</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Test Elements</em>' containment reference list isn't clear, there really should be
	 * more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Test Elements</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElementContainer_TestElements()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestElement> getTestElements();

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
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getTestElementContainer_Id()
	 * @model
	 * @generated
	 */
	String getId();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer#getId <em>Id</em>}'
	 * attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Id</em>' attribute.
	 * @see #getId()
	 * @generated
	 */
	void setId(String value);

} // TestElementContainer
