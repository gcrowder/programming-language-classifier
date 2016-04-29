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
 * A representation of the model object '<em><b>Content</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Content#getUniqueAttribute <em>Unique Attribute</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Content#getSecondAttribute <em>Second Attribute</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getContent()
 * @model
 * @generated
 */
public interface Content extends EObject {
	/**
	 * Returns the value of the '<em><b>Unique Attribute</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Unique Attribute</em>' attribute isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Unique Attribute</em>' attribute.
	 * @see #setUniqueAttribute(String)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getContent_UniqueAttribute()
	 * @model
	 * @generated
	 */
	String getUniqueAttribute();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Content#getUniqueAttribute
	 * <em>Unique Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Unique Attribute</em>' attribute.
	 * @see #getUniqueAttribute()
	 * @generated
	 */
	void setUniqueAttribute(String value);

	/**
	 * Returns the value of the '<em><b>Second Attribute</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Second Attribute</em>' attribute isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Second Attribute</em>' attribute.
	 * @see #setSecondAttribute(String)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getContent_SecondAttribute()
	 * @model
	 * @generated
	 */
	String getSecondAttribute();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Content#getSecondAttribute
	 * <em>Second Attribute</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Second Attribute</em>' attribute.
	 * @see #getSecondAttribute()
	 * @generated
	 */
	void setSecondAttribute(String value);

} // Content
