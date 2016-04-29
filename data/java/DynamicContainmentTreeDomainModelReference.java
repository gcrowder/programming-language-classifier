/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 */
package org.eclipse.emf.ecp.view.dynamictree.model;

import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Dynamic Containment Tree Domain Model Reference</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference#getPathFromRoot <em>
 * Path From Root</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference#getPathFromBase <em>
 * Path From Base</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTreeDomainModelReference()
 * @model
 * @generated
 */
public interface DynamicContainmentTreeDomainModelReference extends VDomainModelReference {
	/**
	 * Returns the value of the '<em><b>Path From Root</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Path From Root</em>' containment reference isn't clear, there really should be more of
	 * a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Path From Root</em>' containment reference.
	 * @see #setPathFromRoot(VDomainModelReference)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTreeDomainModelReference_PathFromRoot()
	 * @model containment="true"
	 * @generated
	 */
	VDomainModelReference getPathFromRoot();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference#getPathFromRoot
	 * <em>Path From Root</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Path From Root</em>' containment reference.
	 * @see #getPathFromRoot()
	 * @generated
	 */
	void setPathFromRoot(VDomainModelReference value);

	/**
	 * Returns the value of the '<em><b>Path From Base</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Path From Base</em>' containment reference isn't clear, there really should be more of
	 * a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Path From Base</em>' containment reference.
	 * @see #setPathFromBase(VDomainModelReference)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTreeDomainModelReference_PathFromBase()
	 * @model containment="true" required="true"
	 * @generated
	 */
	VDomainModelReference getPathFromBase();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference#getPathFromBase
	 * <em>Path From Base</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Path From Base</em>' containment reference.
	 * @see #getPathFromBase()
	 * @generated
	 */
	void setPathFromBase(VDomainModelReference value);

} // DynamicContainmentTreeDomainModelReference
