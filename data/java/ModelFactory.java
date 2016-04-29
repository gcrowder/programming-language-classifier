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

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage
 * @generated
 */
public interface ModelFactory extends EFactory
{
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	ModelFactory eINSTANCE = org.eclipse.emf.ecp.view.dynamictree.model.impl.ModelFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Dynamic Containment Tree</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Dynamic Containment Tree</em>'.
	 * @generated
	 */
	DynamicContainmentTree createDynamicContainmentTree();

	/**
	 * Returns a new object of class '<em>Dynamic Containment Item</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Dynamic Containment Item</em>'.
	 * @generated
	 */
	DynamicContainmentItem createDynamicContainmentItem();

	/**
	 * Returns a new object of class '<em>Test Element</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Test Element</em>'.
	 * @generated
	 */
	TestElement createTestElement();

	/**
	 * Returns a new object of class '<em>Domain Root</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Domain Root</em>'.
	 * @generated
	 */
	DomainRoot createDomainRoot();

	/**
	 * Returns a new object of class '<em>Domain Intermediate</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Domain Intermediate</em>'.
	 * @generated
	 */
	DomainIntermediate createDomainIntermediate();

	/**
	 * Returns a new object of class '<em>Test Element Container</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Test Element Container</em>'.
	 * @generated
	 */
	TestElementContainer createTestElementContainer();

	/**
	 * Returns a new object of class '<em>Dynamic Containment Tree Domain Model Reference</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return a new object of class '<em>Dynamic Containment Tree Domain Model Reference</em>'.
	 * @generated
	 */
	DynamicContainmentTreeDomainModelReference createDynamicContainmentTreeDomainModelReference();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the package supported by this factory.
	 * @generated
	 */
	ModelPackage getModelPackage();

} // ModelFactory
