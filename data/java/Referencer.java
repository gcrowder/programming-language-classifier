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
package org.eclipse.emf.ecp.view.validation.test.model;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Referencer</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.Referencer#getReferencedContent <em>Referenced Content</em>
 * }</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getReferencer()
 * @model
 * @generated
 */
public interface Referencer extends EObject {
	/**
	 * Returns the value of the '<em><b>Referenced Content</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Referenced Content</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Referenced Content</em>' reference.
	 * @see #setReferencedContent(Computer)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getReferencer_ReferencedContent()
	 * @model
	 * @generated
	 */
	Computer getReferencedContent();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.Referencer#getReferencedContent
	 * <em>Referenced Content</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Referenced Content</em>' reference.
	 * @see #getReferencedContent()
	 * @generated
	 */
	void setReferencedContent(Computer value);

} // Referencer
