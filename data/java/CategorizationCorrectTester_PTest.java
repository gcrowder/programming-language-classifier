/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.categorization.swt.test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecp.view.internal.categorization.swt.CategorizationElementTabbedRendererService;
import org.eclipse.emf.ecp.view.internal.categorization.swt.CompositeCategoryJFaceTreeRendererService;
import org.eclipse.emf.ecp.view.internal.categorization.swt.CompositeCategorySWTTabRendererService;
import org.eclipse.emf.ecp.view.internal.categorization.swt.SWTCategorizationElementRendererService;
import org.eclipse.emf.ecp.view.internal.categorization.swt.SWTCategorizationRendererService;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererService;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Eugen
 *
 */
public class CategorizationCorrectTester_PTest {

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {

	}

	@Test
	public void testCorrectTesterForCategorizationWithDepth0() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(0);
		final CategorizationElementTabbedRendererService tabTester = new CategorizationElementTabbedRendererService();
		final double applicable = tabTester.isApplicable(categorizationElement, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable, 0d);

		final SWTCategorizationElementRendererService treeTester = new SWTCategorizationElementRendererService();
		final double applicableTree = treeTester.isApplicable(categorizationElement, null);
		assertEquals(1, applicableTree, 0d);
	}

	@Test
	public void testCorrectTesterForCategorizationWithDepth1() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(1);
		final CategorizationElementTabbedRendererService tabTester = new CategorizationElementTabbedRendererService();
		final double applicable = tabTester.isApplicable(categorizationElement, null);
		assertEquals(1, applicable, 0d);

		final SWTCategorizationElementRendererService treeTester = new SWTCategorizationElementRendererService();
		final double applicableTree = treeTester.isApplicable(categorizationElement, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree, 0d);
	}

	@Test
	public void testCorrectTesterForCategorizationWithDepth2() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(2);
		final CategorizationElementTabbedRendererService tabTester = new CategorizationElementTabbedRendererService();
		final double applicable = tabTester.isApplicable(categorizationElement, null);
		assertEquals(1, applicable, 0d);

		final SWTCategorizationElementRendererService treeTester = new SWTCategorizationElementRendererService();
		final double applicableTree = treeTester.isApplicable(categorizationElement, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree, 0d);
	}

	@Test
	public void testCorrectTesterForCompositeCategoryWithDepth0() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(0);
		final VCategorization categorization0 = mock(VCategorization.class);
		when(categorization0.eContainer()).thenReturn(categorizationElement);
		final VCategorization categorization1 = mock(VCategorization.class);
		when(categorization1.eContainer()).thenReturn(categorization0);
		final VCategorization categorization2 = mock(VCategorization.class);
		when(categorization2.eContainer()).thenReturn(categorization1);
		final VCategorization categorization3 = mock(VCategorization.class);
		when(categorization3.eContainer()).thenReturn(categorization2);

		final SWTCategorizationRendererService defaultTester = new SWTCategorizationRendererService();
		final CompositeCategorySWTTabRendererService tabTester = new CompositeCategorySWTTabRendererService();
		final CompositeCategoryJFaceTreeRendererService treeTester = new CompositeCategoryJFaceTreeRendererService();

		final double applicable0 = defaultTester.isApplicable(categorization0, null);
		assertEquals(1, applicable0, 0d);

		final double applicableTab0 = tabTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab0, 0d);

		final double applicableTree0 = treeTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree0, 0d);

		final double applicable1 = defaultTester.isApplicable(categorization1, null);
		assertEquals(1, applicable1, 0d);

		final double applicableTab1 = tabTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab1, 0d);

		final double applicableTree1 = treeTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree1, 0d);

		final double applicable2 = defaultTester.isApplicable(categorization2, null);
		assertEquals(1, applicable2, 0d);

		final double applicableTab2 = tabTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab2, 0d);

		final double applicableTree2 = treeTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree2, 0d);

		final double applicable3 = defaultTester.isApplicable(categorization3, null);
		assertEquals(1, applicable3, 0d);

		final double applicableTab3 = tabTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab3, 0d);

		final double applicableTree3 = treeTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree3, 0d);
	}

	@Test
	public void testCorrectTesterForCompositeCategoryWithDepth1() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(1);
		final VCategorization categorization0 = mock(VCategorization.class);
		when(categorization0.eContainer()).thenReturn(categorizationElement);
		final VCategorization categorization1 = mock(VCategorization.class);
		when(categorization1.eContainer()).thenReturn(categorization0);
		final VCategorization categorization2 = mock(VCategorization.class);
		when(categorization2.eContainer()).thenReturn(categorization1);
		final VCategorization categorization3 = mock(VCategorization.class);
		when(categorization3.eContainer()).thenReturn(categorization2);

		final SWTCategorizationRendererService defaultTester = new SWTCategorizationRendererService();
		final CompositeCategorySWTTabRendererService tabTester = new CompositeCategorySWTTabRendererService();
		final CompositeCategoryJFaceTreeRendererService treeTester = new CompositeCategoryJFaceTreeRendererService();

		final double applicable0 = defaultTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable0, 0d);

		final double applicableTab0 = tabTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab0, 0d);

		final double applicableTree0 = treeTester.isApplicable(categorization0, null);
		assertEquals(1, applicableTree0, 0d);

		final double applicable1 = defaultTester.isApplicable(categorization1, null);
		assertEquals(1, applicable1, 0d);

		final double applicableTab1 = tabTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab1, 0d);

		final double applicableTree1 = treeTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree1, 0d);

		final double applicable2 = defaultTester.isApplicable(categorization2, null);
		assertEquals(1, applicable2, 0d);

		final double applicableTab2 = tabTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab2, 0d);

		final double applicableTree2 = treeTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree2, 0d);

		final double applicable3 = defaultTester.isApplicable(categorization3, null);
		assertEquals(1, applicable3, 0d);

		final double applicableTab3 = tabTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab3, 0d);

		final double applicableTree3 = treeTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree3, 0d);
	}

	@Test
	public void testCorrectTesterForCompositeCategoryWithDepth2() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(2);
		final VCategorization categorization0 = mock(VCategorization.class);
		when(categorization0.eContainer()).thenReturn(categorizationElement);
		final VCategorization categorization1 = mock(VCategorization.class);
		when(categorization1.eContainer()).thenReturn(categorization0);
		final VCategorization categorization2 = mock(VCategorization.class);
		when(categorization2.eContainer()).thenReturn(categorization1);
		final VCategorization categorization3 = mock(VCategorization.class);
		when(categorization3.eContainer()).thenReturn(categorization2);

		final SWTCategorizationRendererService defaultTester = new SWTCategorizationRendererService();
		final CompositeCategorySWTTabRendererService tabTester = new CompositeCategorySWTTabRendererService();
		final CompositeCategoryJFaceTreeRendererService treeTester = new CompositeCategoryJFaceTreeRendererService();

		final double applicable0 = defaultTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable0, 0d);

		final double applicableTab0 = tabTester.isApplicable(categorization0, null);
		assertEquals(1, applicableTab0, 0d);

		final double applicableTree0 = treeTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree0, 0d);

		final double applicable1 = defaultTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable1, 0d);

		final double applicableTab1 = tabTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab1, 0d);

		final double applicableTree1 = treeTester.isApplicable(categorization1, null);
		assertEquals(1, applicableTree1, 0d);

		final double applicable2 = defaultTester.isApplicable(categorization2, null);
		assertEquals(1, applicable2, 0d);

		final double applicableTab2 = tabTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab2, 0d);

		final double applicableTree2 = treeTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree2, 0d);

		final double applicable3 = defaultTester.isApplicable(categorization3, null);
		assertEquals(1, applicable3, 0d);

		final double applicableTab3 = tabTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab3, 0d);

		final double applicableTree3 = treeTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree3, 0d);
	}

	@Test
	public void testCorrectTesterForCompositeCategoryWithDepth3() {
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		when(categorizationElement.getMainCategoryDepth()).thenReturn(3);
		final VCategorization categorization0 = mock(VCategorization.class);
		when(categorization0.eContainer()).thenReturn(categorizationElement);
		final VCategorization categorization1 = mock(VCategorization.class);
		when(categorization1.eContainer()).thenReturn(categorization0);
		final VCategorization categorization2 = mock(VCategorization.class);
		when(categorization2.eContainer()).thenReturn(categorization1);
		final VCategorization categorization3 = mock(VCategorization.class);
		when(categorization3.eContainer()).thenReturn(categorization2);

		final SWTCategorizationRendererService defaultTester = new SWTCategorizationRendererService();
		final CompositeCategorySWTTabRendererService tabTester = new CompositeCategorySWTTabRendererService();
		final CompositeCategoryJFaceTreeRendererService treeTester = new CompositeCategoryJFaceTreeRendererService();

		final double applicable0 = defaultTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable0, 0d);

		final double applicableTab0 = tabTester.isApplicable(categorization0, null);
		assertEquals(1, applicableTab0, 0d);

		final double applicableTree0 = treeTester.isApplicable(categorization0, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree0, 0d);

		final double applicable1 = defaultTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable1, 0d);

		final double applicableTab1 = tabTester.isApplicable(categorization1, null);
		assertEquals(1, applicableTab1, 0d);

		final double applicableTree1 = treeTester.isApplicable(categorization1, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree1, 0d);

		final double applicable2 = defaultTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicable2, 0d);

		final double applicableTab2 = tabTester.isApplicable(categorization2, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab2, 0d);

		final double applicableTree2 = treeTester.isApplicable(categorization2, null);
		assertEquals(1, applicableTree2, 0d);

		final double applicable3 = defaultTester.isApplicable(categorization3, null);
		assertEquals(1, applicable3, 0d);

		final double applicableTab3 = tabTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTab3, 0d);

		final double applicableTree3 = treeTester.isApplicable(categorization3, null);
		assertEquals(EMFFormsRendererService.NOT_APPLICABLE, applicableTree3, 0d);
	}
}
