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

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Domain Intermediate</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate#getTestElementContainer <em>Test Element
 * Container</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDomainIntermediate()
 * @model
 * @generated
 */
public interface DomainIntermediate extends EObject
{
	/**
	 * Returns the value of the '<em><b>Test Element Container</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Test Element Container</em>' reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Test Element Container</em>' containment reference.
	 * @see #setTestElementContainer(TestElementContainer)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDomainIntermediate_TestElementContainer()
	 * @model containment="true"
	 * @generated
	 */
	TestElementContainer getTestElementContainer();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate#getTestElementContainer
	 * <em>Test Element Container</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Test Element Container</em>' containment reference.
	 * @see #getTestElementContainer()
	 * @generated
	 */
	void setTestElementContainer(TestElementContainer value);

} // DomainIntermediate
