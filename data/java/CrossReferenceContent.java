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
 * A representation of the model object '<em><b>Cross Reference Content</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getParent <em>Parent</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getSingleParent
 * <em>Single Parent</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContent()
 * @model
 * @generated
 */
public interface CrossReferenceContent extends EObject {
	/**
	 * Returns the value of the '<em><b>Parent</b></em>' container reference.
	 * It is bidirectional and its opposite is '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getContents <em>Contents</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Parent</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Parent</em>' container reference.
	 * @see #setParent(CrossReferenceContainer)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContent_Parent()
	 * @see org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getContents
	 * @model opposite="contents" transient="false"
	 * @generated
	 */
	CrossReferenceContainer getParent();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getParent
	 * <em>Parent</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Parent</em>' container reference.
	 * @see #getParent()
	 * @generated
	 */
	void setParent(CrossReferenceContainer value);

	/**
	 * Returns the value of the '<em><b>Single Parent</b></em>' container reference.
	 * It is bidirectional and its opposite is '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getSingleContent
	 * <em>Single Content</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Single Parent</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Single Parent</em>' container reference.
	 * @see #setSingleParent(CrossReferenceContainer)
	 * @see org.eclipse.emf.ecp.view.validation.test.model.TestPackage#getCrossReferenceContent_SingleParent()
	 * @see org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer#getSingleContent
	 * @model opposite="singleContent" transient="false"
	 * @generated
	 */
	CrossReferenceContainer getSingleParent();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent#getSingleParent
	 * <em>Single Parent</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Single Parent</em>' container reference.
	 * @see #getSingleParent()
	 * @generated
	 */
	void setSingleParent(CrossReferenceContainer value);

} // CrossReferenceContent
