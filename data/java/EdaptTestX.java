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
package org.eclipse.emf.ecp.view.edapt.util.test.model.x;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestW;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>X</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX#getW <em>W</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestXPackage#getX()
 * @model
 * @generated
 */
public interface EdaptTestX extends EObject {
	/**
	 * Returns the value of the '<em><b>W</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>W</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>W</em>' reference.
	 * @see #setW(EdaptTestW)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestXPackage#getX_W()
	 * @model
	 * @generated
	 */
	EdaptTestW getW();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX#getW <em>W</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>W</em>' reference.
	 * @see #getW()
	 * @generated
	 */
	void setW(EdaptTestW value);

} // EdaptTestX
