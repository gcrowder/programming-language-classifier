/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.spi.swt.controlgrid.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.emfforms.spi.view.controlgrid.model.VControlGrid;
import org.eclipse.emf.emfforms.spi.view.controlgrid.model.VControlGridCell;
import org.eclipse.emf.emfforms.spi.view.controlgrid.model.VControlGridRow;
import org.eclipse.emf.emfforms.spi.view.controlgrid.model.VControlgridFactory;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ControlGridSWTRenderer_ITest {

	private DefaultRealm realm;
	private VControlGrid controlGrid;
	private ViewModelContext viewModelContext;
	private ReportService reportService;
	private EMFFormsRendererFactory emfFormsRendererFactory;
	private Shell shell;

	@Before
	public void before() {
		realm = new DefaultRealm();
		shell = new Shell();
		controlGrid = mock(VControlGrid.class);
		viewModelContext = mock(ViewModelContext.class);
		reportService = mock(ReportService.class);
		emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);
	}

	@After
	public void after() {
		shell.dispose();
		realm.dispose();
	}

	private ControlGridSWTRenderer createRenderer() {
		return new ControlGridSWTRenderer(controlGrid, viewModelContext, reportService, emfFormsRendererFactory);
	}

	@Test
	public void testGetRendererFactory() {
		final ControlGridSWTRenderer renderer = createRenderer();
		assertSame(emfFormsRendererFactory, renderer.getRendererFactory());
	}

	@Test
	public void testGetGridDescription() {
		final ControlGridSWTRenderer renderer = createRenderer();
		final SWTGridDescription gridDescription = renderer.getGridDescription(mock(SWTGridDescription.class));
		assertEquals(1, gridDescription.getColumns());
		assertEquals(1, gridDescription.getRows());
		assertEquals(1, gridDescription.getGrid().size());

		final SWTGridCell cell = gridDescription.getGrid().get(0);
		assertEquals(0, cell.getColumn());
		assertEquals(1, cell.getHorizontalSpan());
		assertEquals(0, cell.getRow());
		assertSame(renderer, cell.getRenderer());
		assertTrue(cell.isHorizontalFill());
		assertTrue(cell.isHorizontalGrab());
		assertTrue(cell.isVerticalFill());
		assertFalse(cell.isVerticalGrab());
	}

	@Test
	public void testGetChildRenderers() throws EMFFormsNoRendererException {
		/* setup */
		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().addAll(Arrays.asList(row1, row2));

		final VControlGridCell cell11 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell21 = VControlgridFactory.eINSTANCE.createControlGridCell();
		row1.getCells().add(cell11);
		row2.getCells().add(cell21);

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		cell11.setControl(control1);
		cell21.setControl(control2);

		final ControlGridSWTRenderer renderer = createRenderer();

		@SuppressWarnings("unchecked")
		final AbstractSWTRenderer<VElement> renderer1 = mock(AbstractSWTRenderer.class);
		when(emfFormsRendererFactory.getRendererInstance(control1, viewModelContext)).thenReturn(renderer1);

		@SuppressWarnings("unchecked")
		final AbstractSWTRenderer<VElement> renderer2 = mock(AbstractSWTRenderer.class);
		when(emfFormsRendererFactory.getRendererInstance(control2, viewModelContext)).thenReturn(renderer2);

		/* act */
		final Map<VControlGridCell, AbstractSWTRenderer<VElement>> renderers = renderer.getChildRenderers();

		/* assert */
		assertEquals(2, renderers.size());
		assertTrue(renderers.containsKey(cell11));
		assertTrue(renderers.containsKey(cell21));
		assertSame(renderer1, renderers.get(cell11));
		assertSame(renderer2, renderers.get(cell21));
	}

	@Test
	public void testGetChildRenderersEmptyCell() throws EMFFormsNoRendererException {
		/* setup */
		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().addAll(Arrays.asList(row1, row2));

		final VControlGridCell cell11 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell21 = VControlgridFactory.eINSTANCE.createControlGridCell();
		row1.getCells().add(cell11);
		row2.getCells().add(cell21);

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		cell11.setControl(control1);

		final ControlGridSWTRenderer renderer = createRenderer();

		@SuppressWarnings("unchecked")
		final AbstractSWTRenderer<VElement> renderer1 = mock(AbstractSWTRenderer.class);
		when(emfFormsRendererFactory.getRendererInstance(control1, viewModelContext)).thenReturn(renderer1);

		/* act */
		final Map<VControlGridCell, AbstractSWTRenderer<VElement>> renderers = renderer.getChildRenderers();

		/* assert */
		assertEquals(1, renderers.size());
		assertTrue(renderers.containsKey(cell11));
		assertFalse(renderers.containsKey(cell21));
		assertSame(renderer1, renderers.get(cell11));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testGetChildRenderersNoRendererException() throws EMFFormsNoRendererException {
		/* setup */
		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().addAll(Arrays.asList(row1, row2));

		final VControlGridCell cell11 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell21 = VControlgridFactory.eINSTANCE.createControlGridCell();
		row1.getCells().add(cell11);
		row2.getCells().add(cell21);

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		cell11.setControl(control1);
		cell21.setControl(control2);

		final ControlGridSWTRenderer renderer = createRenderer();

		final AbstractSWTRenderer<VElement> renderer1 = mock(AbstractSWTRenderer.class);
		when(emfFormsRendererFactory.getRendererInstance(control1, viewModelContext)).thenReturn(renderer1);

		when(emfFormsRendererFactory.getRendererInstance(control2, viewModelContext))
			.thenThrow(EMFFormsNoRendererException.class);

		/* act */
		final Map<VControlGridCell, AbstractSWTRenderer<VElement>> renderers = renderer.getChildRenderers();

		/* assert */
		assertEquals(1, renderers.size());
		assertTrue(renderers.containsKey(cell11));
		assertFalse(renderers.containsKey(cell21));
		assertSame(renderer1, renderers.get(cell11));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testGetGridDescriptions() {
		final AbstractSWTRenderer<VElement> renderer1 = mock(AbstractSWTRenderer.class);
		final SWTGridDescription swtGridDescription1 = mock(SWTGridDescription.class);
		when(renderer1.getGridDescription(any(SWTGridDescription.class))).thenReturn(swtGridDescription1);

		final AbstractSWTRenderer<VElement> renderer2 = mock(AbstractSWTRenderer.class);
		final SWTGridDescription swtGridDescription2 = mock(SWTGridDescription.class);
		when(renderer2.getGridDescription(any(SWTGridDescription.class))).thenReturn(swtGridDescription2);

		final ControlGridSWTRenderer renderer = createRenderer();

		final Map<AbstractSWTRenderer<VElement>, SWTGridDescription> gridDescriptions = renderer
			.getGridDescriptions(Arrays.asList(renderer1, renderer2));

		assertEquals(2, gridDescriptions.size());
		assertTrue(gridDescriptions.containsKey(renderer1));
		assertTrue(gridDescriptions.containsKey(renderer2));
		assertSame(swtGridDescription1, gridDescriptions.get(renderer1));
		assertSame(swtGridDescription2, gridDescriptions.get(renderer2));

	}

	@Test
	public void testGetRequiredColumnSizesOfRenderers() {
		final SWTGridDescription description1 = mock(SWTGridDescription.class);
		when(description1.getColumns()).thenReturn(1);

		final SWTGridDescription description2 = mock(SWTGridDescription.class);
		when(description2.getColumns()).thenReturn(2);

		final ControlGridSWTRenderer renderer = createRenderer();

		final Map<SWTGridDescription, Integer> sizes = renderer
			.getRequiredColumnSizesOfRenderers(Arrays.asList(description1, description2));

		assertEquals(2, sizes.size());
		assertTrue(sizes.containsKey(description1));
		assertTrue(sizes.containsKey(description2));
		assertEquals(new Integer(2), sizes.get(description1));
		assertEquals(new Integer(3), sizes.get(description2));
	}

	@Test
	public void testGetColumnsPerRenderer() {
		final ControlGridSWTRenderer renderer = createRenderer();
		assertEquals(12, renderer.getColumnsPerRenderer(Arrays.asList(1, 2, 3, 4)));
	}

	@Test
	public void testGetColumnCountsFromRows() {
		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row3 = VControlgridFactory.eINSTANCE.createControlGridRow();

		controlGrid.getRows().addAll(Arrays.asList(row1, row2, row3));

		final VControlGridCell cell11 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell21 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell22 = VControlgridFactory.eINSTANCE.createControlGridCell();

		row1.getCells().add(cell11);
		row2.getCells().addAll(Arrays.asList(cell21, cell22));

		final ControlGridSWTRenderer renderer = createRenderer();

		final Set<Integer> columnCountsFromRows = renderer.getColumnCountsFromRows();

		assertEquals(3, columnCountsFromRows.size());
		assertTrue(columnCountsFromRows.contains(0));
		assertTrue(columnCountsFromRows.contains(1));
		assertTrue(columnCountsFromRows.contains(2));
	}

	@Test
	public void testComputeColumnCountSoThatAllRowsCanBeRendered() {
		final ControlGridSWTRenderer renderer = createRenderer();
		assertEquals(12, renderer.computeColumnCountSoThatAllRowsCanBeRendered(Arrays.asList(0, 1, 2, 3, 4)));
	}

	@Test
	public void testComputeColumnsForSWTLayout() {
		assertEquals(12, createRenderer().computeColumnsForSWTLayout(2, 6));
	}

	@Test
	public void testCreateControlGridComposite() {
		final ControlGridSWTRenderer renderer = createRenderer();
		final Composite composite = renderer.createControlGridComposite(shell, 12);
		final Layout layout = composite.getLayout();
		assertNotNull(layout);
		assertTrue(GridLayout.class.isInstance(layout));
		assertEquals(12, GridLayout.class.cast(layout).numColumns);
	}

	@Test
	public void testRenderEmptyRow() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		when(swtGridCell.getRow()).thenReturn(0);

		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();
		final VControlGridRow row = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().add(row);

		final ControlGridSWTRenderer renderer = createRenderer();
		renderer.init();
		final Control control = renderer.render(swtGridCell, shell);

		assertTrue(Composite.class.isInstance(control));
		final Composite composite = Composite.class.cast(control);
		assertEquals(1, composite.getChildren().length);

		assertTrue(Label.class.isInstance(composite.getChildren()[0]));
		final Label label = Label.class.cast(composite.getChildren()[0]);
		assertLabelIsEmpty(label);
		assertGridData(label, 1, false);
	}

	@Test
	public void testRenderEmptyCell() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		when(swtGridCell.getRow()).thenReturn(0);

		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();
		final VControlGridRow row = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().add(row);
		final VControlGridCell cell = VControlgridFactory.eINSTANCE.createControlGridCell();
		row.getCells().add(cell);

		final ControlGridSWTRenderer renderer = createRenderer();
		renderer.init();
		final Control control = renderer.render(swtGridCell, shell);

		assertTrue(Composite.class.isInstance(control));
		final Composite composite = Composite.class.cast(control);
		assertEquals(2, composite.getChildren().length);

		assertTrue(Label.class.isInstance(composite.getChildren()[0]));
		final Label emptyCell = Label.class.cast(composite.getChildren()[0]);
		assertLabelIsEmpty(emptyCell);
		assertGridData(emptyCell, 1, false);

		assertTrue(Label.class.isInstance(composite.getChildren()[1]));
		final Label spacing = Label.class.cast(composite.getChildren()[1]);
		assertLabelIsEmpty(spacing);
		assertGridData(spacing, 1, false);
	}

	@Test
	public void testRenderControl()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		/* setup */
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		when(swtGridCell.getRow()).thenReturn(0);

		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();
		final VControlGridRow row = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().add(row);
		final VControlGridCell cell = VControlgridFactory.eINSTANCE.createControlGridCell();
		row.getCells().add(cell);

		final VControl vControl = VViewFactory.eINSTANCE.createControl();
		cell.setControl(vControl);

		final DummyRenderer dummyRenderer = new DummyRenderer(vControl, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dummyRenderer.init();
		when(emfFormsRendererFactory.getRendererInstance(vControl, viewModelContext)).thenReturn(dummyRenderer);

		final ControlGridSWTRenderer renderer = createRenderer();
		renderer.init();

		/* act */
		final Control control = renderer.render(swtGridCell, shell);

		/* assert */
		assertTrue(Composite.class.isInstance(control));
		final Composite composite = Composite.class.cast(control);
		assertEquals(4, composite.getChildren().length);

		assertTrue(Composite.class.isInstance(composite.getChildren()[0]));
		assertGridData(Composite.class.cast(composite.getChildren()[0]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[0]).getChildren()[0]));
		final Label controlLabel1 = Label.class.cast(Composite.class.cast(composite.getChildren()[0]).getChildren()[0]);
		assertLabelText(controlLabel1, 0);
		assertGridData(controlLabel1, 1, false);

		assertTrue(Composite.class.isInstance(composite.getChildren()[1]));
		assertGridData(Composite.class.cast(composite.getChildren()[1]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[1]).getChildren()[0]));
		final Label controlLabel2 = Label.class.cast(Composite.class.cast(composite.getChildren()[1]).getChildren()[0]);
		assertLabelText(controlLabel2, 1);
		assertGridData(controlLabel2, 1, false);

		assertTrue(Composite.class.isInstance(composite.getChildren()[2]));
		assertGridData(Composite.class.cast(composite.getChildren()[2]), 1, true);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[2]).getChildren()[0]));
		final Label controlLabel3 = Label.class.cast(Composite.class.cast(composite.getChildren()[2]).getChildren()[0]);
		assertLabelText(controlLabel3, 2);
		assertGridData(controlLabel3, 1, true);

		assertTrue(Label.class.isInstance(composite.getChildren()[3]));
		final Label spacing = Label.class.cast(composite.getChildren()[3]);
		assertLabelIsEmpty(spacing);
		assertGridData(spacing, 1, false);
	}

	// BEGIN COMPLEX CODE
	/**
	 * Expectation is
	 *
	 * L1 L2 L2 L3
	 * L1 L2 L3 L4
	 */
	@Test
	public void testRenderControlSpan()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		/* setup */
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		when(swtGridCell.getRow()).thenReturn(0);

		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		/* first row */
		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().add(row1);
		final VControlGridCell cell1 = VControlgridFactory.eINSTANCE.createControlGridCell();
		row1.getCells().add(cell1);
		final VControl vControl1 = VViewFactory.eINSTANCE.createControl();
		cell1.setControl(vControl1);
		final DummyRenderer dummyRenderer1 = new DummyRenderer(vControl1, viewModelContext, reportService,
			createGridDescription(false, true, false));
		dummyRenderer1.init();
		when(emfFormsRendererFactory.getRendererInstance(vControl1, viewModelContext)).thenReturn(dummyRenderer1);

		/* second row */
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().add(row2);
		final VControlGridCell cell2 = VControlgridFactory.eINSTANCE.createControlGridCell();
		row2.getCells().add(cell2);
		final VControl vControl2 = VViewFactory.eINSTANCE.createControl();
		cell2.setControl(vControl2);
		final DummyRenderer dummyRenderer2 = new DummyRenderer(vControl2, viewModelContext, reportService,
			createGridDescription(false, false, true, false));
		dummyRenderer2.init();
		when(emfFormsRendererFactory.getRendererInstance(vControl2, viewModelContext)).thenReturn(dummyRenderer2);

		final ControlGridSWTRenderer renderer = createRenderer();
		renderer.init();

		/* act */
		final Control control = renderer.render(swtGridCell, shell);

		/* assert */
		assertTrue(Composite.class.isInstance(control));
		final Composite composite = Composite.class.cast(control);
		assertGridLayout(composite, 20);
		assertEquals(9, composite.getChildren().length);

		/* control 1 */
		assertTrue(Composite.class.isInstance(composite.getChildren()[0]));
		assertGridData(Composite.class.cast(composite.getChildren()[0]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[0]).getChildren()[0]));
		final Label controlLabel1 = Label.class.cast(Composite.class.cast(composite.getChildren()[0]).getChildren()[0]);
		assertLabelText(controlLabel1, 0);
		assertGridData(controlLabel1, 1, false);

		assertTrue(Composite.class.isInstance(composite.getChildren()[1]));
		assertGridData(Composite.class.cast(composite.getChildren()[1]), 20 - 1 - 1 - 1, true);// 20-3times non spanning
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[1]).getChildren()[0]));
		final Label controlLabel2 = Label.class.cast(Composite.class.cast(composite.getChildren()[1]).getChildren()[0]);
		assertLabelText(controlLabel2, 1);
		assertGridData(controlLabel2, 1, true);

		assertTrue(Composite.class.isInstance(composite.getChildren()[2]));
		assertGridData(Composite.class.cast(composite.getChildren()[2]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[2]).getChildren()[0]));
		final Label controlLabel3 = Label.class.cast(Composite.class.cast(composite.getChildren()[2]).getChildren()[0]);
		assertLabelText(controlLabel3, 2);
		assertGridData(controlLabel3, 1, false);

		assertTrue(Label.class.isInstance(composite.getChildren()[3]));
		final Label spacing = Label.class.cast(composite.getChildren()[3]);
		assertLabelIsEmpty(spacing);
		assertGridData(spacing, 1, false);

		/* control 2 */
		assertTrue(Composite.class.isInstance(composite.getChildren()[4]));
		assertGridData(Composite.class.cast(composite.getChildren()[4]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[4]).getChildren()[0]));
		final Label controlLabel4 = Label.class.cast(Composite.class.cast(composite.getChildren()[4]).getChildren()[0]);
		assertLabelText(controlLabel4, 0);
		assertGridData(controlLabel4, 1, false);

		assertTrue(Composite.class.isInstance(composite.getChildren()[5]));
		assertGridData(Composite.class.cast(composite.getChildren()[5]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[5]).getChildren()[0]));
		final Label controlLabel5 = Label.class.cast(Composite.class.cast(composite.getChildren()[5]).getChildren()[0]);
		assertLabelText(controlLabel5, 1);
		assertGridData(controlLabel5, 1, false);

		assertTrue(Composite.class.isInstance(composite.getChildren()[6]));
		assertGridData(Composite.class.cast(composite.getChildren()[6]), 20 - 1 - 1 - 1 - 1, true);// 20-4 times non
																									// spanning
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[6]).getChildren()[0]));
		final Label controlLabel6 = Label.class.cast(Composite.class.cast(composite.getChildren()[6]).getChildren()[0]);
		assertLabelText(controlLabel6, 2);
		assertGridData(controlLabel6, 1, true);

		assertTrue(Composite.class.isInstance(composite.getChildren()[7]));
		assertGridData(Composite.class.cast(composite.getChildren()[7]), 1, false);
		assertTrue(Label.class.isInstance(Composite.class.cast(composite.getChildren()[7]).getChildren()[0]));
		final Label controlLabel7 = Label.class.cast(Composite.class.cast(composite.getChildren()[7]).getChildren()[0]);
		assertLabelText(controlLabel7, 3);
		assertGridData(controlLabel7, 1, false);

		assertTrue(Label.class.isInstance(composite.getChildren()[8]));
		final Label spacing2 = Label.class.cast(composite.getChildren()[8]);
		assertLabelIsEmpty(spacing2);
		assertGridData(spacing2, 1, false);
	}

	/**
	 * Control Control Control
	 * Control Empty-- Control
	 * Empty Row--------------
	 * Control---- Control----
	 */
	@Test
	public void testComplexScenario()
		throws EMFFormsNoRendererException, NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		/* Setup ************************************************************************************************/
		/* swt grid cell */
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		when(swtGridCell.getRow()).thenReturn(0);

		/* Control Grid */
		controlGrid = VControlgridFactory.eINSTANCE.createControlGrid();

		/* Rows */
		final VControlGridRow row1 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row2 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row3 = VControlgridFactory.eINSTANCE.createControlGridRow();
		final VControlGridRow row4 = VControlgridFactory.eINSTANCE.createControlGridRow();
		controlGrid.getRows().addAll(Arrays.asList(row1, row2, row3, row4));

		/* Row1 */
		final VControlGridCell cell11 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control11 = VViewFactory.eINSTANCE.createControl();
		cell11.setControl(control11);
		final VControlGridCell cell12 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control12 = VViewFactory.eINSTANCE.createControl();
		cell12.setControl(control12);
		final VControlGridCell cell13 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control13 = VViewFactory.eINSTANCE.createControl();
		cell13.setControl(control13);
		row1.getCells().addAll(Arrays.asList(cell11, cell12, cell13));

		/* Row2 */
		final VControlGridCell cell21 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control21 = VViewFactory.eINSTANCE.createControl();
		cell21.setControl(control21);
		final VControlGridCell cell22 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControlGridCell cell23 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control23 = VViewFactory.eINSTANCE.createControl();
		cell23.setControl(control23);
		row2.getCells().addAll(Arrays.asList(cell21, cell22, cell23));

		/* Row3 */
		/* empty */

		/* Row4 */
		final VControlGridCell cell31 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control31 = VViewFactory.eINSTANCE.createControl();
		cell31.setControl(control31);
		final VControlGridCell cell32 = VControlgridFactory.eINSTANCE.createControlGridCell();
		final VControl control32 = VViewFactory.eINSTANCE.createControl();
		cell32.setControl(control32);
		row4.getCells().addAll(Arrays.asList(cell31, cell32));

		/* Renderers */
		final DummyRenderer dr11 = new DummyRenderer(control11, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr11.init();
		final DummyRenderer dr12 = new DummyRenderer(control12, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr12.init();
		final DummyRenderer dr13 = new DummyRenderer(control13, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr13.init();
		final DummyRenderer dr21 = new DummyRenderer(control21, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr21.init();
		final DummyRenderer dr23 = new DummyRenderer(control23, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr23.init();
		final DummyRenderer dr31 = new DummyRenderer(control31, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr31.init();
		final DummyRenderer dr32 = new DummyRenderer(control32, viewModelContext, reportService,
			createGridDescription(false, false, true));
		dr32.init();

		/* Renderer factory */
		when(emfFormsRendererFactory.getRendererInstance(control11, viewModelContext)).thenReturn(dr11);
		when(emfFormsRendererFactory.getRendererInstance(control12, viewModelContext)).thenReturn(dr12);
		when(emfFormsRendererFactory.getRendererInstance(control13, viewModelContext)).thenReturn(dr13);
		when(emfFormsRendererFactory.getRendererInstance(control21, viewModelContext)).thenReturn(dr21);
		when(emfFormsRendererFactory.getRendererInstance(control23, viewModelContext)).thenReturn(dr23);
		when(emfFormsRendererFactory.getRendererInstance(control31, viewModelContext)).thenReturn(dr31);
		when(emfFormsRendererFactory.getRendererInstance(control32, viewModelContext)).thenReturn(dr32);

		/* control grid renderer */
		final ControlGridSWTRenderer renderer = createRenderer();
		renderer.init();
		/* Setup End ********************************************************************************************/

		/* Act */
		final Control render = renderer.render(swtGridCell, shell);
		/* Act End */

		/* Assert ************************************************************************************************/
		/* Composite with expected column count and children size */
		assertTrue(Composite.class.isInstance(render));
		final Composite composite = Composite.class.cast(render);
		// 1. row 3*4 (controls)
		// 2. row 2*4 (controls) + 2 (empty cell)
		// 3. row 1 (empty row)
		// 4. row 2*4 (controls)
		assertEquals(3 * 4 + 2 * 4 + 2 + 1 + 2 * 4, composite.getChildren().length);
		// renderers require 3 columns (+1 for spacing) -> 4 required
		// rows require 3, 2 and 1 columns -> 6 required
		// -> total = 4*6
		assertGridLayout(composite, 4 * 6);

		/* Row1 Control1 */
		final Composite control111 = Composite.class.cast(composite.getChildren()[0]);
		assertLabelText(control111, 0);
		assertGridData(control111, 1, false);

		final Composite control112 = Composite.class.cast(composite.getChildren()[1]);
		assertLabelText(control112, 1);
		assertGridData(control112, 1, false);

		final Composite control113 = Composite.class.cast(composite.getChildren()[2]);
		assertLabelText(control113, 2);
		assertGridData(control113, (4 * 6 - 3 * 3) / 3, true); // columns - 3*3 (1-spanning) devided by 3 (# controls)

		final Label control11s = Label.class.cast(composite.getChildren()[3]);
		assertLabelIsEmpty(control11s);
		assertGridData(control11s, 1, false);

		/* Row1 Control2 */
		final Composite control121 = Composite.class.cast(composite.getChildren()[4]);
		assertLabelText(control121, 0);
		assertGridData(control121, 1, false);

		final Composite control122 = Composite.class.cast(composite.getChildren()[5]);
		assertLabelText(control122, 1);
		assertGridData(control122, 1, false);

		final Composite control123 = Composite.class.cast(composite.getChildren()[6]);
		assertLabelText(control123, 2);
		assertGridData(control123, (4 * 6 - 3 * 3) / 3, true); // columns - 3*3 (1-spanning) devided by 3 (# controls)

		final Label control12s = Label.class.cast(composite.getChildren()[7]);
		assertLabelIsEmpty(control12s);
		assertGridData(control12s, 1, false);

		/* Row1 Control3 */
		final Composite control131 = Composite.class.cast(composite.getChildren()[8]);
		assertLabelText(control131, 0);
		assertGridData(control131, 1, false);

		final Composite control132 = Composite.class.cast(composite.getChildren()[9]);
		assertLabelText(control132, 1);
		assertGridData(control132, 1, false);

		final Composite control133 = Composite.class.cast(composite.getChildren()[10]);
		assertLabelText(control133, 2);
		assertGridData(control133, (4 * 6 - 3 * 3) / 3, true); // columns - 3*3 (1-spanning) devided by 3 (# controls)

		final Label control13s = Label.class.cast(composite.getChildren()[11]);
		assertLabelIsEmpty(control13s);
		assertGridData(control13s, 1, false);

		/* Row2 Control1 */
		final Composite control211 = Composite.class.cast(composite.getChildren()[12]);
		assertLabelText(control211, 0);
		assertGridData(control211, 1, false);

		final Composite control212 = Composite.class.cast(composite.getChildren()[13]);
		assertLabelText(control212, 1);
		assertGridData(control212, 1, false);

		final Composite control213 = Composite.class.cast(composite.getChildren()[14]);
		assertLabelText(control213, 2);
		assertGridData(control213, (4 * 6 - 3 * 3) / 3, true); // columns - 3*3 (1-spanning) devided by 3 (# controls)

		final Label control21s = Label.class.cast(composite.getChildren()[15]);
		assertLabelIsEmpty(control21s);
		assertGridData(control21s, 1, false);

		/* Row2 Empty cell */
		final Label empty22 = Label.class.cast(composite.getChildren()[16]);
		assertLabelIsEmpty(empty22);
		assertGridData(empty22, (4 * 6 - 3 * 3) / 3 + 2, false); // columns - 3*3 (1-spanning) devided by 3 (# controls)
																	// + 2 (since we only render 1 cell)

		final Label empty22s = Label.class.cast(composite.getChildren()[17]);
		assertLabelIsEmpty(empty22s);
		assertGridData(empty22s, 1, false);

		/* Row2 Control3 */
		final Composite control231 = Composite.class.cast(composite.getChildren()[18]);
		assertLabelText(control231, 0);
		assertGridData(control231, 1, false);

		final Composite control232 = Composite.class.cast(composite.getChildren()[19]);
		assertLabelText(control232, 1);
		assertGridData(control232, 1, false);

		final Composite control233 = Composite.class.cast(composite.getChildren()[20]);
		assertLabelText(control233, 2);
		assertGridData(control233, (4 * 6 - 3 * 3) / 3, true); // columns - 3*3 (1-spanning) devided by 3 (# controls)

		final Label control23s = Label.class.cast(composite.getChildren()[21]);
		assertLabelIsEmpty(control23s);
		assertGridData(control23s, 1, false);

		/* Row3 */
		final Label emptyRow3 = Label.class.cast(composite.getChildren()[22]);
		assertLabelIsEmpty(emptyRow3);
		assertGridData(emptyRow3, 4 * 6, false);// full width

		/* Row4 Control1 */
		final Composite control411 = Composite.class.cast(composite.getChildren()[23]);
		assertLabelText(control411, 0);
		assertGridData(control411, 1, false);

		final Composite control412 = Composite.class.cast(composite.getChildren()[24]);
		assertLabelText(control412, 1);
		assertGridData(control412, 1, false);

		final Composite control413 = Composite.class.cast(composite.getChildren()[25]);
		assertLabelText(control413, 2);
		assertGridData(control413, (4 * 6 - 2 * 3) / 2, true); // columns - 2*3 (1-spanning) devided by 2 (# controls)

		final Label control41s = Label.class.cast(composite.getChildren()[26]);
		assertLabelIsEmpty(control41s);
		assertGridData(control41s, 1, false);

		/* Row4 Control2 */
		final Composite control421 = Composite.class.cast(composite.getChildren()[27]);
		assertLabelText(control421, 0);
		assertGridData(control421, 1, false);

		final Composite control422 = Composite.class.cast(composite.getChildren()[28]);
		assertLabelText(control422, 1);
		assertGridData(control422, 1, false);

		final Composite control423 = Composite.class.cast(composite.getChildren()[29]);
		assertLabelText(control423, 2);
		assertGridData(control423, (4 * 6 - 2 * 3) / 2, true); // columns - 2*3 (1-spanning) devided by 2 (# controls)

		final Label control42s = Label.class.cast(composite.getChildren()[30]);
		assertLabelIsEmpty(control42s);
		assertGridData(control42s, 1, false);

		/* last child should have index 30 */
	}

	// END COMPLEX CODE

	private static SWTGridDescription createGridDescription(Boolean... grab) {
		final SWTGridDescription grid = GridDescriptionFactory.INSTANCE.createSimpleGrid(1, grab.length, null);
		for (int i = 0; i < grid.getGrid().size(); i++) {
			final SWTGridCell swtGridCell = grid.getGrid().get(i);
			swtGridCell.setHorizontalGrab(grab[i]);
		}
		return grid;
	}

	private static void assertLabelIsEmpty(Label label) {
		assertTrue((label.getText() == null || label.getText().isEmpty()) && label.getImage() == null);
	}

	private static void assertLabelText(Label label, int text) {
		assertEquals(String.valueOf(text), label.getText());
	}

	private static void assertLabelText(Composite composite, int text) {
		assertEquals(1, composite.getChildren().length);
		assertLabelText(Label.class.cast(composite.getChildren()[0]), text);

	}

	private static void assertGridData(Control control, int hSpan, boolean horizontalGrab) {
		assertTrue(GridData.class.isInstance(control.getLayoutData()));
		final GridData gridData = GridData.class.cast(control.getLayoutData());
		if (hSpan == 1) {
			assertTrue(MessageFormat.format("Expected {0} but was {1}", hSpan, gridData.horizontalSpan), //$NON-NLS-1$
				gridData.horizontalSpan == 0 || gridData.horizontalSpan == 1);
		} else {
			assertEquals(hSpan, gridData.horizontalSpan);
		}
		assertEquals(horizontalGrab, gridData.grabExcessHorizontalSpace);
	}

	private static void assertGridLayout(Composite composite, int numColumns) {
		assertTrue(GridLayout.class.isInstance(composite.getLayout()));
		final GridLayout gridLayout = GridLayout.class.cast(composite.getLayout());
		assertEquals(numColumns, gridLayout.numColumns);
	}

	private static final class DummyRenderer extends AbstractSWTRenderer<VElement> {

		private final SWTGridDescription rendererGridDescription;

		DummyRenderer(VControl vElement, ViewModelContext viewContext, ReportService reportService,
			SWTGridDescription rendererGridDescription) {
			super(vElement, viewContext, reportService);
			this.rendererGridDescription = rendererGridDescription;
		}

		@Override
		public SWTGridDescription getGridDescription(SWTGridDescription gridDescription) {
			for (final SWTGridCell swtGridCell : rendererGridDescription.getGrid()) {
				swtGridCell.setRenderer(this);
			}
			return rendererGridDescription;
		}

		@Override
		protected Control renderControl(SWTGridCell gridCell, Composite parent)
			throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
			return createLabel(gridCell.getColumn(), parent);

		}

		private Control createLabel(int string, Composite parent) {
			final Label label = new Label(parent, SWT.NONE);
			label.setText(String.valueOf(string));
			return label;
		}

	}

}
