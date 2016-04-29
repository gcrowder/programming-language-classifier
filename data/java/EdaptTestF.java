/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 */
package org.eclipse.emf.ecp.view.edapt.util.test.model.f;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>F</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestF#getE <em>E</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestFPackage#getF()
 * @model
 * @generated
 */
public interface EdaptTestF extends EObject {
	/**
	 * Returns the value of the '<em><b>E</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>E</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>E</em>' reference.
	 * @see #setE(EdaptTestE)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestFPackage#getF_E()
	 * @model
	 * @generated
	 */
	EdaptTestE getE();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestF#getE <em>E</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>E</em>' reference.
	 * @see #getE()
	 * @generated
	 */
	void setE(EdaptTestE value);

} // EdaptTestF
