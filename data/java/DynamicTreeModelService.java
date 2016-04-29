/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.dynamictree.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelFactory;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElement;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer;
import org.eclipse.emf.ecp.view.spi.categorization.model.ECPAction;
import org.eclipse.emf.ecp.view.spi.categorization.model.VAction;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizableElement;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.edit.provider.AdapterFactoryItemDelegator;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.osgi.framework.Bundle;

/**
 * @author Eugen Neufeld
 *
 */
public class DynamicTreeModelService implements ViewModelService {

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)
	 */
	@Override
	public void instantiate(ViewModelContext context) {
		final VElement viewModel = context.getViewModel();
		if (DynamicContainmentTree.class.isInstance(viewModel)) {
			updateViewModel((DynamicContainmentTree) viewModel, context);
		}
		else {
			final TreeIterator<EObject> treeIterator = viewModel.eAllContents();
			while (treeIterator.hasNext()) {
				final EObject eObject = treeIterator.next();
				if (DynamicContainmentTree.class.isInstance(eObject)) {
					updateViewModel((DynamicContainmentTree) eObject, context);
				}
			}
		}
	}

	/**
	 * @param eObject
	 * @param context
	 */
	private void updateViewModel(DynamicContainmentTree dynamicTree, ViewModelContext context) {
		EObject current = context.getDomainModel();
		for (final EReference eReference : dynamicTree.getPathToRoot()) {
			EObject eGet = (EObject) current.eGet(eReference);
			if (eGet == null) {
				final EObject newObject = EcoreUtil.create(eReference.getEReferenceType());
				current.eSet(eReference, newObject);
				eGet = newObject;
			}
			current = eGet;
		}

		final TestElementContainer type = (TestElementContainer) current;

		dynamicTree.setDomainModel(type);

		final ComposedAdapterFactory caf = new ComposedAdapterFactory(
			ComposedAdapterFactory.Descriptor.Registry.INSTANCE);

		final AdapterFactoryItemDelegator adapterFactoryItemDelegator = new AdapterFactoryItemDelegator(caf);

		final VAction[] actions = dynamicTree.getActions().toArray(new VAction[0]);
		@SuppressWarnings("unchecked")
		final List<TestElement> testElements = (List<TestElement>) adapterFactoryItemDelegator.getChildren(type);
		for (final TestElement testElement : testElements) {

			final DynamicContainmentItem item = ModelFactory.eINSTANCE.createDynamicContainmentItem();
			item.setComposite(EcoreUtil.copy(dynamicTree.getChildComposite()));
			item.setDomainModel(testElement);
			item.setBaseItemIndex(type.getTestElements().indexOf(testElement));
			// ViewModelUtil.resolveDomainReferences(item, testElement);
			dynamicTree.getItems().add(item);

			createActions(item, actions);

			addChildren(item, adapterFactoryItemDelegator,
				actions, type);
		}

		if (actions.length > 0) {
			createActions(dynamicTree, actions[0]);
		}
		caf.dispose();
	}

	/**
	 * Creates the actions for the given node.
	 *
	 * @param categorizableElement the {@link VCategorizableElement} to create the actions for
	 * @param actions
	 *            the {@link VAction}s to be created
	 */
	public static void createActions(VCategorizableElement categorizableElement, VAction... actions) {
		final List<ECPAction> nodeActions = new ArrayList<ECPAction>();
		for (final VAction action : actions) {
			try {
				final Bundle bundle = Platform.getBundle(action.getBundle());
				if (bundle == null) {
					// TODO externalize strings
					throw new ClassNotFoundException(action.getClassName()
						+ " cannot be loaded because bundle " + action.getBundle() //$NON-NLS-1$
						+ " cannot be resolved"); //$NON-NLS-1$
				}
				final Class<?> loadClass = bundle.loadClass(action.getClassName());
				final ECPAction treeAction = (ECPAction) loadClass.getConstructors()[0].newInstance();
				nodeActions.add(treeAction);
			} catch (final ClassNotFoundException e) {
				e.printStackTrace();
			} catch (final IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final InstantiationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (final InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		categorizableElement.setECPActions(nodeActions);
	}

	private void addChildren(DynamicContainmentItem containmentItem,
		AdapterFactoryItemDelegator adapterFactoryItemDelegator,
		VAction[] actions, TestElementContainer container) {
		@SuppressWarnings("unchecked")
		final List<TestElement> children = (List<TestElement>) adapterFactoryItemDelegator
			.getChildren(containmentItem.getDomainModel());

		for (final TestElement testElement : children) {
			final DynamicContainmentItem pi = ModelFactory.eINSTANCE.createDynamicContainmentItem();
			pi.setComposite(EcoreUtil.copy(containmentItem.getComposite()));
			pi.setDomainModel(testElement);
			pi.setBaseItemIndex(container.getTestElements().indexOf(testElement));
			// ViewModelUtil.resolveDomainReferences(pi, testElement);

			final EList<DynamicContainmentItem> uvbPackingItems = containmentItem.getItems();

			uvbPackingItems.add(pi);

			createActions(pi, actions);
			addChildren(pi, adapterFactoryItemDelegator, actions, container);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelService#dispose()
	 */
	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelService#getPriority()
	 */
	@Override
	public int getPriority() {
		return -5;
	}

}
