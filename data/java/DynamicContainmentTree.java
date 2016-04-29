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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Dynamic Containment Tree</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getDomainModel <em>Domain Model</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getChildReference <em>Child Reference
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getPathToRoot <em>Path To Root</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getChildComposite <em>Child Composite
 * </em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getItems <em>Items</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree()
 * @model
 * @generated
 */
public interface DynamicContainmentTree extends VCategory
{
	/**
	 * Returns the value of the '<em><b>Domain Model</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Domain Model</em>' reference isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Domain Model</em>' reference.
	 * @see #setDomainModel(EObject)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree_DomainModel()
	 * @model transient="true"
	 * @generated
	 */
	EObject getDomainModel();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getDomainModel
	 * <em>Domain Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Domain Model</em>' reference.
	 * @see #getDomainModel()
	 * @generated
	 */
	void setDomainModel(EObject value);

	/**
	 * Returns the value of the '<em><b>Child Reference</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Child Reference</em>' reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Child Reference</em>' reference.
	 * @see #setChildReference(EReference)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree_ChildReference()
	 * @model required="true"
	 * @generated
	 */
	EReference getChildReference();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getChildReference
	 * <em>Child Reference</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Child Reference</em>' reference.
	 * @see #getChildReference()
	 * @generated
	 */
	void setChildReference(EReference value);

	/**
	 * Returns the value of the '<em><b>Path To Root</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecore.EReference}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Path To Root</em>' reference list isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Path To Root</em>' reference list.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree_PathToRoot()
	 * @model
	 * @generated
	 */
	EList<EReference> getPathToRoot();

	/**
	 * Returns the value of the '<em><b>Child Composite</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Child Composite</em>' containment reference isn't clear, there really should be more
	 * of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Child Composite</em>' containment reference.
	 * @see #setChildComposite(VContainedElement)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree_ChildComposite()
	 * @model containment="true"
	 * @generated
	 */
	VContainedElement getChildComposite();

	/**
	 * Sets the value of the '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree#getChildComposite
	 * <em>Child Composite</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Child Composite</em>' containment reference.
	 * @see #getChildComposite()
	 * @generated
	 */
	void setChildComposite(VContainedElement value);

	/**
	 * Returns the value of the '<em><b>Items</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Items</em>' containment reference list isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Items</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentTree_Items()
	 * @model containment="true"
	 * @generated
	 */
	EList<DynamicContainmentItem> getItems();

} // DynamicContainmentTree
