/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 * Lucas Koehler - databinding tests
 ******************************************************************************/
package org.eclipse.emf.ecp.view.internal.core.swt.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.core.swt.tests.ObservingWritableValue;
import org.eclipse.emf.ecp.view.spi.model.LabelAlignment;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTTestUtil;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

public class BooleanControlRenderer_PTest extends AbstractControl_PTest {

	private DefaultRealm realm;

	@Before
	public void before() throws DatabindingFailedException {
		realm = new DefaultRealm();
		final ReportService reportService = mock(ReportService.class);
		databindingService = mock(EMFFormsDatabinding.class);
		labelProvider = mock(EMFFormsLabelProvider.class);
		templateProvider = mock(VTViewTemplateProvider.class);
		setup();
		renderer = new BooleanControlSWTRenderer(vControl, context, reportService, databindingService, labelProvider,
			templateProvider);
		renderer.init();
	}

	@After
	public void testTearDown() {
		realm.dispose();
		dispose();
	}

	@Test
	public void renderControlLabelAlignmentNone()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		setMockLabelAlignment(LabelAlignment.NONE);
		final TestObservableValue mockedObservableValue = mock(TestObservableValue.class);
		when(mockedObservableValue.getRealm()).thenReturn(realm);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableValue);
		final Control render = renderControl(new SWTGridCell(0, 1, renderer));
		assertControl(render);
	}

	@Test
	public void renderControlLabelAlignmentLeft()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		setMockLabelAlignment(LabelAlignment.LEFT);
		final TestObservableValue mockedObservableValue = mock(TestObservableValue.class);
		when(mockedObservableValue.getRealm()).thenReturn(realm);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableValue);
		final Control render = renderControl(new SWTGridCell(0, 2, renderer));

		assertControl(render);
	}

	private void assertControl(Control render) {
		assertTrue(Button.class.isInstance(render));
		assertEquals(SWT.CHECK, Button.class.cast(render).getStyle()
			& SWT.CHECK);
		assertEquals("org_eclipse_emf_ecp_control_boolean", Button.class.cast(render).getData(CUSTOM_VARIANT));
	}

	@Override
	protected void mockControl() throws DatabindingFailedException {
		final EClass eObject = EcoreFactory.eINSTANCE.createEClass();
		final EStructuralFeature eStructuralFeature = EcorePackage.eINSTANCE
			.getEClass_Interface();
		super.mockControl(eObject, eStructuralFeature);
	}

	@Test
	public void testDatabindingServiceUsageInitialBinding() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final boolean initialValue = true;
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue, Boolean.class);

		final Button button = setUpDatabindingTest(mockedObservable);
		assertEquals(initialValue, button.getSelection());

	}

	@Test
	public void testDatabindingServiceUsageChangeObservable() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final boolean initialValue = true;
		final boolean changedValue = false;
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue, Boolean.class);

		final Button button = setUpDatabindingTest(mockedObservable);
		mockedObservable.setValue(changedValue);
		assertEquals(changedValue, button.getSelection());

	}

	@Test
	public void testDatabindingServiceUsageChangeControl() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final boolean initialValue = true;
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue, Boolean.class);

		final Button button = setUpDatabindingTest(mockedObservable);
		SWTTestUtil.clickButton(button);

		assertEquals(button.getSelection(), mockedObservable.getValue());

	}

	/**
	 * Universal set up stuff for the data binding test cases.
	 *
	 * @param mockedObservable
	 * @return
	 * @throws NoRendererFoundException
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws DatabindingFailedException
	 */
	private Button setUpDatabindingTest(final ObservingWritableValue mockedObservable) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		Mockito.reset(databindingService);

		mockDatabindingIsUnsettable();
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservable);

		final Control renderControl = renderControl(new SWTGridCell(0, 2, renderer));
		final Button button = (Button) renderControl;
		return button;
	}

	/**
	 * Tests whether the {@link EMFFormsLabelProvider} is used to get the labels of the control.
	 *
	 * @throws NoRendererFoundException
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testLabelServiceUsage() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		NoLabelFoundException {
		labelServiceUsage();
	}
}
