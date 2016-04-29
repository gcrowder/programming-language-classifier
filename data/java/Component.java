/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource Muenchen GmbH - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.view.editor.test.model.test;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.ui.view.editor.test.model.test.Component#getComponent <em>Component</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.ui.view.editor.test.model.test.TestPackage#getComponent()
 * @model
 * @generated
 */
public interface Component extends EObject {
	/**
	 * Returns the value of the '<em><b>Component</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Component</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Component</em>' containment reference.
	 * @see #setComponent(Component2)
	 * @see org.eclipse.emf.ecp.ui.view.editor.test.model.test.TestPackage#getComponent_Component()
	 * @model containment="true"
	 * @generated
	 */
	Component2 getComponent();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.ui.view.editor.test.model.test.Component#getComponent
	 * <em>Component</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Component</em>' containment reference.
	 * @see #getComponent()
	 * @generated
	 */
	void setComponent(Component2 value);

} // Component
