/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 */
package org.eclipse.emf.ecp.view.mapping.test.example;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Intermediate</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.mapping.test.example.Intermediate#getContainer <em>Container</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.mapping.test.example.ExamplePackage#getIntermediate()
 * @model
 * @generated
 */
public interface Intermediate extends EObject {
	/**
	 * Returns the value of the '<em><b>Container</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Container</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Container</em>' containment reference.
	 * @see #setContainer(Container)
	 * @see org.eclipse.emf.ecp.view.mapping.test.example.ExamplePackage#getIntermediate_Container()
	 * @model containment="true"
	 * @generated
	 */
	Container getContainer();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.mapping.test.example.Intermediate#getContainer
	 * <em>Container</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Container</em>' containment reference.
	 * @see #getContainer()
	 * @generated
	 */
	void setContainer(Container value);

} // Intermediate
