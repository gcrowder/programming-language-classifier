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
package org.eclipse.emf.ecp.view.edapt.util.test.model.e;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestD;
import org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestF;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>E</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE#getD <em>D</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE#getF <em>F</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage#getE()
 * @model
 * @generated
 */
public interface EdaptTestE extends EObject {
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
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage#getE_D()
	 * @model
	 * @generated
	 */
	EdaptTestD getD();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE#getD <em>D</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>D</em>' reference.
	 * @see #getD()
	 * @generated
	 */
	void setD(EdaptTestD value);

	/**
	 * Returns the value of the '<em><b>F</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>F</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>F</em>' reference.
	 * @see #setF(EdaptTestF)
	 * @see org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage#getE_F()
	 * @model
	 * @generated
	 */
	EdaptTestF getF();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestE#getF <em>F</em>}'
	 * reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>F</em>' reference.
	 * @see #getF()
	 * @generated
	 */
	void setF(EdaptTestF value);

} // EdaptTestE
