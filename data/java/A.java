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

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>A</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A#getB <em>B</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getA()
 * @model
 * @generated
 */
public interface A extends EObject {
	/**
	 * Returns the value of the '<em><b>B</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>B</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>B</em>' containment reference.
	 * @see #setB(B)
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getA_B()
	 * @model containment="true"
	 * @generated
	 */
	B getB();

	/**
	 * Sets the value of the '{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A#getB
	 * <em>B</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>B</em>' containment reference.
	 * @see #getB()
	 * @generated
	 */
	void setB(B value);

} // A
