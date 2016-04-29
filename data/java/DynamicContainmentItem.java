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
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Dynamic Containment Item</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getDomainModel <em>Domain Model</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getItems <em>Items</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getComposite <em>Composite</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getBaseItemIndex <em>Base Item Index
 * </em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentItem()
 * @model
 * @generated
 */
public interface DynamicContainmentItem extends VCategorizableElement
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
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentItem_DomainModel()
	 * @model transient="true"
	 * @generated
	 */
	EObject getDomainModel();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getDomainModel
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
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentItem_Items()
	 * @model containment="true" transient="true"
	 * @generated
	 */
	EList<DynamicContainmentItem> getItems();

	/**
	 * Returns the value of the '<em><b>Composite</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Composite</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Composite</em>' containment reference.
	 * @see #setComposite(VContainedElement)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentItem_Composite()
	 * @model containment="true"
	 * @generated
	 */
	VContainedElement getComposite();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getComposite
	 * <em>Composite</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Composite</em>' containment reference.
	 * @see #getComposite()
	 * @generated
	 */
	void setComposite(VContainedElement value);

	/**
	 * Returns the value of the '<em><b>Base Item Index</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Item Index</em>' attribute isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Base Item Index</em>' attribute.
	 * @see #setBaseItemIndex(Integer)
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage#getDynamicContainmentItem_BaseItemIndex()
	 * @model transient="true"
	 * @generated
	 */
	Integer getBaseItemIndex();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem#getBaseItemIndex
	 * <em>Base Item Index</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param value the new value of the '<em>Base Item Index</em>' attribute.
	 * @see #getBaseItemIndex()
	 * @generated
	 */
	void setBaseItemIndex(Integer value);

} // DynamicContainmentItem
