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
package org.eclipse.emf.ecp.view.edapt.util.test.model.y;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestX;
import org.eclipse.emf.ecp.view.edapt.util.test.model.z.EdaptTestZ;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Y</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestY#getX <em>X</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestY#getZ <em>Z</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestYPackage#getY()
 * @model
 * @generated
 */
public interface EdaptTestY extends EObject {
	/**
	 * Returns the value of the '<em><b>X</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>X</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>X</em>' reference.
	 * @see #setX(EdaptTestX)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestYPackage#getY_X()
	 * @model
	 * @generated
	 */
	EdaptTestX getX();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestY#getX <em>X</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>X</em>' reference.
	 * @see #getX()
	 * @generated
	 */
	void setX(EdaptTestX value);

	/**
	 * Returns the value of the '<em><b>Z</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Z</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Z</em>' reference.
	 * @see #setZ(EdaptTestZ)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestYPackage#getY_Z()
	 * @model
	 * @generated
	 */
	EdaptTestZ getZ();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestY#getZ <em>Z</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Z</em>' reference.
	 * @see #getZ()
	 * @generated
	 */
	void setZ(EdaptTestZ value);

} // EdaptTestY
