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
package org.eclipse.emf.ecp.view.spi.categorization.swt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.categorization.model.VAbstractCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Eugen
 *
 */
@RunWith(DatabindingClassRunner.class)
public class CategorizationRenderer_PTest {

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testCategorizationElementTreeRenderer() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsRendererFactory emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);

		final SWTGridCell gridCell = mock(SWTGridCell.class);
		final Shell shell = new Shell();
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		final EList<VAbstractCategorization> categorizations = new BasicEList<VAbstractCategorization>();
		when(categorizationElement.getCategorizations()).thenReturn(categorizations);
		@SuppressWarnings("unchecked")
		final TreeIterator<EObject> iterator = mock(TreeIterator.class);
		when(iterator.hasNext()).thenReturn(false);
		when(categorizationElement.eAllContents()).thenReturn(iterator);
		final ViewModelContext vmc = mock(ViewModelContext.class);
		final SWTCategorizationElementRenderer categorizatrionElementRenderer = new SWTCategorizationElementRenderer(
			categorizationElement, vmc, reportService, emfFormsRendererFactory);
		categorizatrionElementRenderer.init();
		final Control render = categorizatrionElementRenderer.render(gridCell, shell);
		assertTrue(Composite.class.isInstance(render));
		assertEquals(2, Composite.class.cast(render).getChildren().length);
		assertTrue(Tree.class.isInstance(Composite.class.cast(render).getChildren()[0]));
		assertTrue(ScrolledComposite.class.isInstance(Composite.class.cast(render).getChildren()[1]));
	}

	@Test
	public void testCategorizationElementTabRenderer() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsRendererFactory emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);

		final SWTGridCell gridCell = mock(SWTGridCell.class);
		final Shell shell = new Shell();
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		final EList<VAbstractCategorization> categorizations = new BasicEList<VAbstractCategorization>();
		when(categorizationElement.getCategorizations()).thenReturn(categorizations);
		final ViewModelContext vmc = mock(ViewModelContext.class);
		final VTViewTemplateProvider viewTemplateProvider = mock(VTViewTemplateProvider.class);
		final CategorizationElementTabbedSWTRenderer categorizatrionElementRenderer = new CategorizationElementTabbedSWTRenderer(
			categorizationElement, vmc, reportService, emfFormsRendererFactory, viewTemplateProvider);
		categorizatrionElementRenderer.init();
		final Control render = categorizatrionElementRenderer.render(gridCell, shell);
		assertTrue(CTabFolder.class.isInstance(render));
	}

	@Test
	public void testCompositeCategoryTreeRenderer() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsRendererFactory emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);

		final SWTGridCell gridCell = mock(SWTGridCell.class);
		final Shell shell = new Shell();
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		final EList<VAbstractCategorization> categorizations = new BasicEList<VAbstractCategorization>();
		final VCategorization categorization = mock(VCategorization.class);
		categorizations.add(categorization);
		when(categorization.getCategorizations()).thenReturn(new BasicEList<VAbstractCategorization>());
		when(categorizationElement.getCategorizations()).thenReturn(categorizations);
		@SuppressWarnings("unchecked")
		final TreeIterator<EObject> iterator = mock(TreeIterator.class);
		when(iterator.hasNext()).thenReturn(false);
		when(categorization.eAllContents()).thenReturn(iterator);

		when(categorization.eContainer()).thenReturn(categorizationElement);

		final ViewModelContext vmc = mock(ViewModelContext.class);
		final CompositeCategoryJFaceTreeRenderer categorizatrionElementRenderer = new CompositeCategoryJFaceTreeRenderer(
			categorization, vmc, reportService, emfFormsRendererFactory);
		categorizatrionElementRenderer.init();
		final Control render = categorizatrionElementRenderer.render(gridCell, shell);
		assertTrue(Composite.class.isInstance(render));
		assertEquals(2, Composite.class.cast(render).getChildren().length);
		assertTrue(Tree.class.isInstance(Composite.class.cast(render).getChildren()[0]));
		assertTrue(ScrolledComposite.class.isInstance(Composite.class.cast(render).getChildren()[1]));
	}

	@Test
	public void testCompositeCategoryElementTabRenderer() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsRendererFactory emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);

		final SWTGridCell gridCell = mock(SWTGridCell.class);
		final Shell shell = new Shell();
		final VCategorizationElement categorizationElement = mock(VCategorizationElement.class);
		final EList<VAbstractCategorization> categorizations = new BasicEList<VAbstractCategorization>();
		final VCategorization categorization = mock(VCategorization.class);
		categorizations.add(categorization);
		when(categorization.getCategorizations()).thenReturn(new BasicEList<VAbstractCategorization>());
		when(categorizationElement.getCategorizations()).thenReturn(categorizations);
		final ViewModelContext vmc = mock(ViewModelContext.class);
		final VTViewTemplateProvider viewTemplateProvider = mock(VTViewTemplateProvider.class);
		final CompositeCategorySWTTabRenderer categorizatrionElementRenderer = new CompositeCategorySWTTabRenderer(
			categorization, vmc, reportService, emfFormsRendererFactory, viewTemplateProvider);
		categorizatrionElementRenderer.init();
		final Control render = categorizatrionElementRenderer.render(gridCell, shell);
		assertTrue(CTabFolder.class.isInstance(render));
	}
}
