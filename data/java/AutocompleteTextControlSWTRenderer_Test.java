/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.swt.control.text.autocomplete.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.databinding.Binding;
import org.eclipse.emf.databinding.EMFObservables;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.swt.control.text.autocomplete.viewservice.AutocompleteViewModelService;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class AutocompleteTextControlSWTRenderer_Test {

	private static final String CONTENT_PROPOSAL_ADAPTER = "ContentProposalAdapter"; //$NON-NLS-1$
	private static final String FOO = "foo"; //$NON-NLS-1$
	private static final String BAR = "bar"; //$NON-NLS-1$
	private static final String FOOBAR = "foobar"; //$NON-NLS-1$

	private static List<String> staticProposals;

	private DefaultRealm defaultRealm;

	private VControl control;
	private AutocompleteViewModelService autocompleteViewModelService;
	private ViewModelContext viewModelContext;
	private EMFFormsDatabinding emfFormsDatabinding;
	private D domainModel;
	private Shell shell;
	private Composite composite;

	@BeforeClass
	public static void setUpBeforeClass() {
		staticProposals = new ArrayList<String>();
		staticProposals.add(FOO);
		staticProposals.add(BAR);
	}

	@Before
	public void setUp() throws DatabindingFailedException {
		defaultRealm = new DefaultRealm();

		shell = new Shell();
		composite = new Composite(shell, SWT.NONE);

		domainModel = TestFactory.eINSTANCE.createD();
		domainModel.setX(FOOBAR);

		control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(TestPackage.eINSTANCE.getD_X());

		autocompleteViewModelService = mock(AutocompleteViewModelService.class);
		when(autocompleteViewModelService.getProposals(any(EObject.class), any(EAttribute.class)))
			.thenReturn(staticProposals);

		viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getService(AutocompleteViewModelService.class)).thenReturn(autocompleteViewModelService);
		when(viewModelContext.getDomainModel()).thenReturn(domainModel);

		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getObservableValue(control.getDomainModelReference(), domainModel))
			.thenReturn(EMFObservables.observeValue(domainModel, TestPackage.eINSTANCE.getD_X()));
	}

	@After
	public void tearDown() {
		shell.dispose();
		defaultRealm.dispose();
	}

	@Test
	public void testGetProposals() throws DatabindingFailedException {
		final AutocompleteTextControlSWTRenderer renderer = new AutocompleteTextControlSWTRenderer(
			control,
			viewModelContext,
			mock(ReportService.class),
			emfFormsDatabinding,
			mock(EMFFormsLabelProvider.class),
			mock(VTViewTemplateProvider.class));
		final List<String> proposals = renderer.getProposals();
		assertSame(staticProposals, proposals);
	}

	@Test
	public void testGetProposalsNoService() throws DatabindingFailedException {
		viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getService(AutocompleteViewModelService.class)).thenReturn(null);
		when(viewModelContext.getDomainModel()).thenReturn(domainModel);
		final AutocompleteTextControlSWTRenderer renderer = new AutocompleteTextControlSWTRenderer(
			control,
			viewModelContext,
			mock(ReportService.class),
			emfFormsDatabinding,
			mock(EMFFormsLabelProvider.class),
			mock(VTViewTemplateProvider.class));
		final List<String> proposals = renderer.getProposals();
		assertTrue(proposals.isEmpty());
	}

	@Test(expected = DatabindingFailedException.class)
	public void testGetProposalsDatabindingFailed() throws DatabindingFailedException {
		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getObservableValue(control.getDomainModelReference(), domainModel))
			.thenThrow(new DatabindingFailedException("")); //$NON-NLS-1$
		final AutocompleteTextControlSWTRenderer renderer = new AutocompleteTextControlSWTRenderer(
			control,
			viewModelContext,
			mock(ReportService.class),
			emfFormsDatabinding,
			mock(EMFFormsLabelProvider.class),
			mock(VTViewTemplateProvider.class));
		renderer.getProposals();
	}

	@Test
	public void testCreateJFaceViewer() throws DatabindingFailedException {
		final AutocompleteTextControlSWTRenderer renderer = new AutocompleteTextControlSWTRenderer(
			control,
			viewModelContext,
			mock(ReportService.class),
			emfFormsDatabinding,
			mock(EMFFormsLabelProvider.class),
			mock(VTViewTemplateProvider.class));
		final Viewer viewer = renderer.createJFaceViewer(composite);
		assertTrue(ComboViewer.class.isInstance(viewer));
		final ComboViewer comboViewer = ComboViewer.class.cast(viewer);
		assertEquals(1, composite.getChildren().length);
		assertSame(comboViewer.getCombo(), composite.getChildren()[0]);
		assertSame(staticProposals, comboViewer.getInput());

		/* check if contentproposal adapters have been set up */
		/* TODO better way to test this? */
		final Listener[] keyDownListeners = comboViewer.getCombo().getListeners(SWT.KeyDown);
		final Listener[] traverseListeners = comboViewer.getCombo().getListeners(SWT.Traverse);
		final Listener[] modifyListeners = comboViewer.getCombo().getListeners(SWT.Modify);

		assertTrue("Content assist not set up?", hasContentProposalAdapter(keyDownListeners)); //$NON-NLS-1$
		assertTrue("Content assist not set up?", hasContentProposalAdapter(traverseListeners)); //$NON-NLS-1$
		assertTrue("Content assist not set up?", hasContentProposalAdapter(modifyListeners)); //$NON-NLS-1$
	}

	private boolean hasContentProposalAdapter(Listener[] listeners) {
		for (final Listener listener : listeners) {
			if (listener.getClass().getName().contains(CONTENT_PROPOSAL_ADAPTER)) {
				return true;
			}
		}
		return false;
	}

	@Test
	public void testCreateBindings() throws DatabindingFailedException {
		final AutocompleteTextControlSWTRenderer renderer = new AutocompleteTextControlSWTRenderer(
			control,
			viewModelContext,
			mock(ReportService.class),
			emfFormsDatabinding,
			mock(EMFFormsLabelProvider.class),
			mock(VTViewTemplateProvider.class));
		final Viewer viewer = renderer.createJFaceViewer(composite);
		final Combo combo = ComboViewer.class.cast(viewer).getCombo();
		final Binding[] bindings = renderer.createBindings(viewer);
		assertEquals(1, bindings.length);

		domainModel.setX(FOO);
		assertEquals(FOO, combo.getText());

		combo.setText(BAR);
		assertEquals(BAR, domainModel.getX());
	}

}
