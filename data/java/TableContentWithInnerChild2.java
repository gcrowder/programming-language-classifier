/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 *******************************************************************************/

package org.eclipse.emf.ecp.view.validation.test.model;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Table Content With Inner Child2</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild2#getInnerChild
 * <em>Inner Child</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getTableContentWithInnerChild2()
 * @model
 * @generated
 */
public interface TableContentWithInnerChild2 extends TableContent {
	/**
	 * Returns the value of the '<em><b>Inner Child</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Inner Child</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Inner Child</em>' containment reference.
	 * @see #setInnerChild(TableContent)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getTableContentWithInnerChild2_InnerChild()
	 * @model containment="true"
	 * @generated
	 */
	TableContent getInnerChild();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild2#getInnerChild
	 * <em>Inner Child</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Inner Child</em>' containment reference.
	 * @see #getInnerChild()
	 * @generated
	 */
	void setInnerChild(TableContent value);

} // TableContentWithInnerChild2
