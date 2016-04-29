/**
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 */
package org.eclipse.emf.ecp.view.edapt.test.model;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Foo</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.test.model.Foo#getBar <em>Bar</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.test.model.ModelPackage#getFoo()
 * @model
 * @generated
 */
public interface Foo extends EObject {
	/**
	 * Returns the value of the '<em><b>Bar</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bar</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Bar</em>' attribute.
	 * @see #setBar(String)
	 * @see org.eclipse.emf.ecp.view.edapt.test.model.ModelPackage#getFoo_Bar()
	 * @model
	 * @generated
	 */
	String getBar();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.test.model.Foo#getBar <em>Bar</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Bar</em>' attribute.
	 * @see #getBar()
	 * @generated
	 */
	void setBar(String value);

} // Foo
