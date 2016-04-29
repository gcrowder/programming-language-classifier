/**
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 */
package org.eclipse.emfforms.core.services.databinding.testmodel.test.model;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>D</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D#getX <em>X</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D#getYList <em>YList</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getD()
 * @model
 * @generated
 */
public interface D extends EObject {
	/**
	 * Returns the value of the '<em><b>X</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>X</em>' attribute isn't clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>X</em>' attribute.
	 * @see #setX(String)
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getD_X()
	 * @model
	 * @generated
	 */
	String getX();

	/**
	 * Sets the value of the '{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D#getX
	 * <em>X</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>X</em>' attribute.
	 * @see #getX()
	 * @generated
	 */
	void setX(String value);

	/**
	 * Returns the value of the '<em><b>YList</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.Integer}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>YList</em>' attribute list isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>YList</em>' attribute list.
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getD_YList()
	 * @model
	 * @generated
	 */
	EList<Integer> getYList();

} // D
