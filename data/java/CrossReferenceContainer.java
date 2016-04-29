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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Cross Reference Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getContents <em>Contents</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getSingleContent
 * <em>Single Content</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContainer()
 * @model
 * @generated
 */
public interface CrossReferenceContainer extends EObject {
	/**
	 * Returns the value of the '<em><b>Contents</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent}.
	 * It is bidirectional and its opposite is '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getParent <em>Parent</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contents</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Contents</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContainer_Contents()
	 * @see org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getParent
	 * @model opposite="parent" containment="true"
	 * @generated
	 */
	EList<CrossReferenceContent> getContents();

	/**
	 * Returns the value of the '<em><b>Single Content</b></em>' containment reference.
	 * It is bidirectional and its opposite is '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getSingleParent
	 * <em>Single Parent</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Single Content</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Single Content</em>' containment reference.
	 * @see #setSingleContent(CrossReferenceContent)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContainer_SingleContent()
	 * @see org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getSingleParent
	 * @model opposite="singleParent" containment="true"
	 * @generated
	 */
	CrossReferenceContent getSingleContent();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getSingleContent
	 * <em>Single Content</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Single Content</em>' containment reference.
	 * @see #getSingleContent()
	 * @generated
	 */
	void setSingleContent(CrossReferenceContent value);

} // CrossReferenceContainer
