/**
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 */
package org.eclipse.emf.ecp.view.spi.compoundcontrol.swt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.observable.Observables;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.compoundcontrol.model.VCompoundControl;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.LabelAlignment;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CompoundControlSWTRenderer_ITest {

	private static final String LABEL1 = "label1"; //$NON-NLS-1$
	private static final String LABEL2 = "label2"; //$NON-NLS-1$

	private static final String C_CONTROL = "CControl"; //$NON-NLS-1$
	private static final String C_VALIDATION = "CValidation"; //$NON-NLS-1$

	private VCompoundControl compoundControl;
	private ViewModelContext viewModelContext;
	private ReportService reportService;
	private EMFFormsLabelProvider emfFormsLabelProvider;
	private EMFFormsRendererFactory emfFormsRendererFactory;
	private Shell shell;
	private DefaultRealm realm;

	@Before
	public void before() {
		realm = new DefaultRealm();
		shell = new Shell(Display.getDefault());
		compoundControl = mock(VCompoundControl.class);
		viewModelContext = mock(ViewModelContext.class);
		reportService = mock(ReportService.class);
		emfFormsLabelProvider = mock(EMFFormsLabelProvider.class);
		emfFormsRendererFactory = mock(EMFFormsRendererFactory.class);
	}

	@After
	public void after() {
		shell.dispose();
		realm.dispose();
	}

	private CompoundControlSWTRenderer createRenderer() {
		return new CompoundControlSWTRenderer(compoundControl, viewModelContext, reportService, emfFormsLabelProvider,
			emfFormsRendererFactory);
	}

	@Test
	public void testGetLabelProvider() {
		final CompoundControlSWTRenderer renderer = createRenderer();
		assertSame(emfFormsLabelProvider, renderer.getLabelProvider());
	}

	@Test
	public void testGetRendererFactory() {
		final CompoundControlSWTRenderer renderer = createRenderer();
		assertSame(emfFormsRendererFactory, renderer.getRendererFactory());
	}

	@Test
	public void testGetGridDescription() {
		final CompoundControlSWTRenderer renderer = createRenderer();
		final SWTGridDescription gridDescription = renderer.getGridDescription(mock(SWTGridDescription.class));
		assertEquals(2, gridDescription.getColumns());
		assertEquals(1, gridDescription.getRows());
		assertEquals(2, gridDescription.getGrid().size());

		final SWTGridCell label = gridDescription.getGrid().get(0);
		assertEquals(0, label.getColumn());
		assertEquals(1, label.getHorizontalSpan());
		assertEquals(0, label.getRow());
		assertSame(renderer, label.getRenderer());
		assertTrue(label.isHorizontalFill());
		assertFalse(label.isHorizontalGrab());
		assertFalse(label.isVerticalFill());
		assertFalse(label.isVerticalGrab());

		final SWTGridCell controls = gridDescription.getGrid().get(1);
		assertEquals(1, controls.getColumn());
		assertEquals(1, controls.getHorizontalSpan());
		assertEquals(0, controls.getRow());
		assertSame(renderer, controls.getRenderer());
		assertTrue(controls.isHorizontalFill());
		assertTrue(controls.isHorizontalGrab());
		assertFalse(controls.isVerticalFill());
		assertFalse(controls.isVerticalGrab());
	}

	@Test
	public void testCreateLabel() throws NoLabelFoundException {
		/* setup */
		final VControl control1 = mock(VControl.class);
		final VFeaturePathDomainModelReference dmr1 = mock(VFeaturePathDomainModelReference.class);
		when(control1.getDomainModelReference()).thenReturn(dmr1);

		final VControl control2 = mock(VControl.class);
		final VFeaturePathDomainModelReference dmr2 = mock(VFeaturePathDomainModelReference.class);
		when(control2.getDomainModelReference()).thenReturn(dmr2);

		when(compoundControl.getControls()).thenReturn(ECollections.asEList(control1, control2));

		final EObject domainModel = mock(EObject.class);
		when(viewModelContext.getDomainModel()).thenReturn(domainModel);

		when(emfFormsLabelProvider.getDisplayName(dmr1, domainModel))
			.thenReturn(Observables.constantObservableValue(LABEL1, String.class));

		when(emfFormsLabelProvider.getDisplayName(dmr2, domainModel))
			.thenReturn(Observables.constantObservableValue(LABEL2, String.class));

		final CompoundControlSWTRenderer renderer = createRenderer();

		/* act */
		final Control label = renderer.createLabel(shell);

		/* assert */
		assertTrue(Composite.class.isInstance(label));
		final Composite labelComposite = Composite.class.cast(label);
		assertEquals(3, labelComposite.getChildren().length);
		for (final Control control : labelComposite.getChildren()) {
			assertTrue(Label.class.isInstance(control));
		}
		assertEquals(LABEL1, Label.class.cast(labelComposite.getChildren()[0]).getText());
		assertEquals("/", Label.class.cast(labelComposite.getChildren()[1]).getText()); //$NON-NLS-1$
		assertEquals(LABEL2, Label.class.cast(labelComposite.getChildren()[2]).getText());
	}

	@Test
	public void testCreateControls() throws EMFFormsNoRendererException {
		/* setup */
		final VControl control1 = mock(VControl.class);
		final VFeaturePathDomainModelReference dmr1 = mock(VFeaturePathDomainModelReference.class);
		when(control1.getDomainModelReference()).thenReturn(dmr1);

		final VControl control2 = mock(VControl.class);
		final VFeaturePathDomainModelReference dmr2 = mock(VFeaturePathDomainModelReference.class);
		when(control2.getDomainModelReference()).thenReturn(dmr2);

		when(compoundControl.getControls()).thenReturn(ECollections.asEList(control1, control2));

		final EObject domainModel = mock(EObject.class);
		when(viewModelContext.getDomainModel()).thenReturn(domainModel);

		final DummyRenderer dummyRenderer1 = createDummyRenderer(control1);
		final DummyRenderer dummyRenderer2 = createDummyRenderer(control2);

		when(emfFormsRendererFactory.getRendererInstance(control1, viewModelContext))
			.thenReturn(dummyRenderer1);

		when(emfFormsRendererFactory.getRendererInstance(control2, viewModelContext))
			.thenReturn(dummyRenderer2);

		final CompoundControlSWTRenderer renderer = spy(createRenderer());

		doReturn(GridLayoutFactory.fillDefaults().create()).when(renderer).getColumnLayout(any(Integer.class),
			any(Boolean.class));

		doReturn(GridDataFactory.fillDefaults().create()).when(renderer).getLayoutData(any(SWTGridCell.class),
			any(SWTGridDescription.class), any(SWTGridDescription.class),
			any(SWTGridDescription.class), any(VElement.class), any(EObject.class), any(Control.class));

		doReturn(GridDataFactory.fillDefaults().create()).when(renderer).getSpanningLayoutData(
			any(VContainedElement.class), any(Integer.class), any(Integer.class));

		/* act */
		final Control controls = renderer.createControls(shell);

		/* assert */
		assertTrue(Composite.class.isInstance(controls));
		final Composite controlsComposite = Composite.class.cast(controls);
		assertEquals(2, controlsComposite.getChildren().length);
		for (final Control control : controlsComposite.getChildren()) {
			assertTrue(Composite.class.isInstance(control));
		}

		final Composite control1Composite = Composite.class.cast(controlsComposite.getChildren()[0]);
		assertEquals(2, control1Composite.getChildren().length);
		for (final Control control : control1Composite.getChildren()) {
			assertTrue(Label.class.isInstance(control));
		}
		assertEquals(C_VALIDATION, Label.class.cast(control1Composite.getChildren()[0]).getText());
		assertEquals(C_CONTROL, Label.class.cast(control1Composite.getChildren()[1]).getText());

		final Composite control2Composite = Composite.class.cast(controlsComposite.getChildren()[1]);
		assertEquals(2, control2Composite.getChildren().length);
		for (final Control control : control2Composite.getChildren()) {
			assertTrue(Label.class.isInstance(control));
		}
		assertEquals(C_VALIDATION, Label.class.cast(control2Composite.getChildren()[0]).getText());
		assertEquals(C_CONTROL, Label.class.cast(control2Composite.getChildren()[1]).getText());

		verify(control1, times(1)).setLabelAlignment(LabelAlignment.NONE);
		verify(control2, times(1)).setLabelAlignment(LabelAlignment.NONE);
	}

	@Test
	public void testRenderControlColumn0() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(0);
		final CompoundControlSWTRenderer renderer = spy(createRenderer());
		doReturn(new Label(shell, SWT.NONE)).when(renderer).createLabel(shell);
		renderer.renderControl(swtGridCell, shell);
		verify(renderer, times(1)).createLabel(shell);
	}

	@Test
	public void testRenderControlColumn1() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(1);
		final CompoundControlSWTRenderer renderer = spy(createRenderer());
		doReturn(new Label(shell, SWT.NONE)).when(renderer).createControls(shell);
		renderer.renderControl(swtGridCell, shell);
		verify(renderer, times(1)).createControls(shell);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRenderControlColumnOther() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final SWTGridCell swtGridCell = mock(SWTGridCell.class);
		when(swtGridCell.getColumn()).thenReturn(2);
		final CompoundControlSWTRenderer renderer = spy(createRenderer());
		renderer.renderControl(swtGridCell, shell);
	}

	private DummyRenderer createDummyRenderer(final VControl control) {
		final DummyRenderer dummyRenderer = new DummyRenderer(control, viewModelContext, reportService);
		dummyRenderer.init();
		return dummyRenderer;
	}

	private static final class DummyRenderer extends AbstractSWTRenderer<VElement> {

		private SWTGridDescription rendererGridDescription;

		DummyRenderer(VControl vElement, ViewModelContext viewContext, ReportService reportService) {
			super(vElement, viewContext, reportService);
		}

		@Override
		public SWTGridDescription getGridDescription(SWTGridDescription gridDescription) {
			if (rendererGridDescription == null) {
				rendererGridDescription = GridDescriptionFactory.INSTANCE.createSimpleGrid(1, 2, this);
				for (int i = 0; i < rendererGridDescription.getGrid().size() - 1; i++) {
					final SWTGridCell swtGridCell = rendererGridDescription.getGrid().get(i);
					swtGridCell.setHorizontalGrab(false);
				}
			}
			return rendererGridDescription;
		}

		@Override
		protected Control renderControl(SWTGridCell gridCell, Composite parent)
			throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
			switch (gridCell.getColumn()) {
			case 0:
				return createLabel(C_VALIDATION, parent);
			case 1:
				return createLabel(C_CONTROL, parent);
			default:
				throw new IllegalArgumentException();
			}
		}

		private Control createLabel(String string, Composite parent) {
			final Label label = new Label(parent, SWT.NONE);
			label.setText(string);
			return label;
		}

	}

}
