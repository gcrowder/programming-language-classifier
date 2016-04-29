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
package org.eclipse.emf.ecp.view.edapt.util.test.model.c;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>C</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC#getD <em>D</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCPackage#getC()
 * @model
 * @generated
 */
public interface EdaptTestC extends EObject {
	/**
	 * Returns the value of the '<em><b>D</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>D</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>D</em>' reference.
	 * @see #setD(EdaptTestD)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCPackage#getC_D()
	 * @model
	 * @generated
	 */
	EdaptTestD getD();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestC#getD <em>D</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>D</em>' reference.
	 * @see #getD()
	 * @generated
	 */
	void setD(EdaptTestD value);

} // EdaptTestC
