/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.model.edit.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.junit.Test;

public class ChildrenDescriptor_PTest {

	/**
	 * Needs to be adapted after refactoring
	 * These are the counts for the core model only
	 * If a model elements is moved out, the respective test can be removed here
	 */
	private static final int DEAFULT_ATTACHMENTS = 1;
	private static final int RENDERABLE_CHILD_COUNT = 0 + DEAFULT_ATTACHMENTS;
	// control
	private static final int NUMBER_OF_COMPOSITES = 1;

	private static final int VIEW_CHILD_COUNT = NUMBER_OF_COMPOSITES + RENDERABLE_CHILD_COUNT;

	private static final int CONTROL_CHILD_COUNT = RENDERABLE_CHILD_COUNT + 1;

	private final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(new ComposedAdapterFactory(
		ComposedAdapterFactory.Descriptor.Registry.INSTANCE), new BasicCommandStack());

	@Test
	public void testViewChildDescriptors() {
		final int size = getChildrenSize(VViewPackage.eINSTANCE.getView());
		assertEquals(VIEW_CHILD_COUNT, size);
	}

	/**
	 * Class is abstract, Exception expected
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testCompositeChildDescriptors() {
		getChildrenSize(VViewPackage.eINSTANCE.getContainedElement());
	}

	@Test
	public void testControlChildDescriptors() {
		final int size = getChildrenSize(VViewPackage.eINSTANCE.getControl());
		assertEquals(CONTROL_CHILD_COUNT, size);
	}

	/**
	 * Class is abstract, Exception expected
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testCompositeCollectionDescriptors() {
		getChildrenSize(VViewPackage.eINSTANCE.getContainer());
	}

	/**
	 * Class is abstract, Exception expected
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRenderableDescriptors() {
		getChildrenSize(VViewPackage.eINSTANCE.getElement());
	}

	/**
	 * @param category
	 * @return
	 */
	private int getChildrenSize(EClass eClass) {
		final EObject eObject = EcoreUtil.create(eClass);
		return domain.getNewChildDescriptors(eObject, null).size();
	}

}
