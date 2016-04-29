/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 */
package org.eclipse.emf.ecp.view.index.test.example;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Child</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.index.test.example.Child#getIntermediateTarget <em>Intermediate Target</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.index.test.example.ExamplePackage#getChild()
 * @model
 * @generated
 */
public interface Child extends EObject {
	/**
	 * Returns the value of the '<em><b>Intermediate Target</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Intermediate Target</em>' containment reference list isn't clear, there really should
	 * be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Intermediate Target</em>' containment reference.
	 * @see #setIntermediateTarget(IntermediateTarget)
	 * @see org.eclipse.emf.ecp.view.index.test.example.ExamplePackage#getChild_IntermediateTarget()
	 * @model containment="true"
	 * @generated
	 */
	IntermediateTarget getIntermediateTarget();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.index.test.example.Child#getIntermediateTarget
	 * <em>Intermediate Target</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Intermediate Target</em>' containment reference.
	 * @see #getIntermediateTarget()
	 * @generated
	 */
	void setIntermediateTarget(IntermediateTarget value);

} // Child
