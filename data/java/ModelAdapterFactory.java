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
package org.eclipse.emf.ecp.view.dynamictree.model.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainRoot;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElement;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer;
import org.eclipse.emf.ecp.view.spi.categorization.model.VAbstractCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VElement;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage
 * @generated
 */
public class ModelAdapterFactory extends AdapterFactoryImpl
{
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected static ModelPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public ModelAdapterFactory()
	{
		if (modelPackage == null) {
			modelPackage = ModelPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance
	 * object of the model.
	 * <!-- end-user-doc -->
	 *
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object)
	{
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject) object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected ModelSwitch<Adapter> modelSwitch =
		new ModelSwitch<Adapter>() {
			@Override
			public Adapter caseDynamicContainmentTree(DynamicContainmentTree object) {
				return createDynamicContainmentTreeAdapter();
			}

			@Override
			public Adapter caseDynamicContainmentItem(DynamicContainmentItem object) {
				return createDynamicContainmentItemAdapter();
			}

			@Override
			public Adapter caseTestElement(TestElement object) {
				return createTestElementAdapter();
			}

			@Override
			public Adapter caseDomainRoot(DomainRoot object) {
				return createDomainRootAdapter();
			}

			@Override
			public Adapter caseDomainIntermediate(DomainIntermediate object) {
				return createDomainIntermediateAdapter();
			}

			@Override
			public Adapter caseTestElementContainer(TestElementContainer object) {
				return createTestElementContainerAdapter();
			}

			@Override
			public Adapter caseDynamicContainmentTreeDomainModelReference(
				DynamicContainmentTreeDomainModelReference object) {
				return createDynamicContainmentTreeDomainModelReferenceAdapter();
			}

			@Override
			public Adapter caseElement(VElement object) {
				return createElementAdapter();
			}

			@Override
			public Adapter caseCategorizableElement(VCategorizableElement object) {
				return createCategorizableElementAdapter();
			}

			@Override
			public Adapter caseAbstractCategorization(VAbstractCategorization object) {
				return createAbstractCategorizationAdapter();
			}

			@Override
			public Adapter caseCategory(VCategory object) {
				return createCategoryAdapter();
			}

			@Override
			public Adapter caseDomainModelReference(VDomainModelReference object) {
				return createDomainModelReferenceAdapter();
			}

			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target)
	{
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree <em>Dynamic Containment Tree</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree
	 * @generated
	 */
	public Adapter createDynamicContainmentTreeAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem <em>Dynamic Containment Item</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem
	 * @generated
	 */
	public Adapter createDynamicContainmentItemAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.emf.ecp.view.dynamictree.model.TestElement
	 * <em>Test Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.TestElement
	 * @generated
	 */
	public Adapter createTestElementAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.emf.ecp.view.dynamictree.model.DomainRoot
	 * <em>Domain Root</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.DomainRoot
	 * @generated
	 */
	public Adapter createDomainRootAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate <em>Domain Intermediate</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate
	 * @generated
	 */
	public Adapter createDomainIntermediateAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer <em>Test Element Container</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer
	 * @generated
	 */
	public Adapter createTestElementContainerAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference
	 * <em>Dynamic Containment Tree Domain Model Reference</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference
	 * @generated
	 */
	public Adapter createDynamicContainmentTreeDomainModelReferenceAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.emf.ecp.view.spi.model.VElement
	 * <em>Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.spi.model.VElement
	 * @generated
	 */
	public Adapter createElementAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement <em>Categorizable Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement
	 * @generated
	 */
	public Adapter createCategorizableElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '
	 * {@link org.eclipse.emf.ecp.view.spi.categorization.model.VAbstractCategorization
	 * <em>Abstract Categorization</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VAbstractCategorization
	 * @generated
	 */
	public Adapter createAbstractCategorizationAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.emf.ecp.view.spi.categorization.model.VCategory
	 * <em>Category</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.spi.categorization.model.VCategory
	 * @generated
	 */
	public Adapter createCategoryAdapter()
	{
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.emf.ecp.view.spi.model.VDomainModelReference
	 * <em>Domain Model Reference</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @see org.eclipse.emf.ecp.view.spi.model.VDomainModelReference
	 * @generated
	 */
	public Adapter createDomainModelReferenceAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 *
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter()
	{
		return null;
	}

} // ModelAdapterFactory
