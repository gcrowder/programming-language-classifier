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
package org.eclipse.emf.ecp.view.spi.core.swt.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.observable.Observables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.core.swt.tests.ObservingWritableValue;
import org.eclipse.emf.ecp.view.internal.core.swt.renderer.AbstractControl_PTest;
import org.eclipse.emf.ecp.view.spi.model.LabelAlignment;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTTestUtil;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.editsupport.EMFFormsEditSupport;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

public class TextControlRenderer_PTest extends AbstractControl_PTest {

	private DefaultRealm realm;

	@Before
	public void before() throws DatabindingFailedException {
		realm = new DefaultRealm();
		final ReportService reportService = mock(ReportService.class);
		databindingService = mock(EMFFormsDatabinding.class);
		labelProvider = mock(EMFFormsLabelProvider.class);
		templateProvider = mock(VTViewTemplateProvider.class);
		final EMFFormsEditSupport editSupport = mock(EMFFormsEditSupport.class);
		setup();
		renderer = new TextControlSWTRenderer(vControl, context, reportService, databindingService, labelProvider,
			templateProvider, editSupport);
		renderer.init();
	}

	@After
	public void testTearDown() {
		realm.dispose();
		dispose();
	}

	@Test
	public void renderControlLabelAlignmentNone()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, DatabindingFailedException,
		NoLabelFoundException {
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			Observables.constantObservableValue("antiException", String.class));
		setMockLabelAlignment(LabelAlignment.NONE);
		final TestObservableValue mockedObservableValue = mock(TestObservableValue.class);
		when(mockedObservableValue.getRealm()).thenReturn(realm);
		final EObject mockedEObject = mock(EObject.class);
		when(mockedEObject.eIsSet(any(EStructuralFeature.class))).thenReturn(true);
		when(mockedObservableValue.getObserved()).thenReturn(mockedEObject);
		final EStructuralFeature mockedEStructuralFeature = EcorePackage.eINSTANCE.getENamedElement_Name();
		when(mockedObservableValue.getValueType()).thenReturn(mockedEStructuralFeature);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableValue);
		final Control render = renderControl(new SWTGridCell(0, 1, renderer));
		assertControl(render);
	}

	@Test
	public void renderControlLabelAlignmentLeft()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption, DatabindingFailedException,
		NoLabelFoundException {
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			Observables.constantObservableValue("antiException", String.class));
		setMockLabelAlignment(LabelAlignment.LEFT);
		final TestObservableValue mockedObservableValue = mock(TestObservableValue.class);
		when(mockedObservableValue.getRealm()).thenReturn(realm);
		final EObject mockedEObject = mock(EObject.class);
		when(mockedEObject.eIsSet(any(EStructuralFeature.class))).thenReturn(true);
		when(mockedObservableValue.getObserved()).thenReturn(mockedEObject);
		final EStructuralFeature mockedEStructuralFeature = mock(EStructuralFeature.class);
		when(mockedEStructuralFeature.isUnsettable()).thenReturn(false);
		when(mockedObservableValue.getValueType()).thenReturn(mockedEStructuralFeature);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableValue);
		final Control render = renderControl(new SWTGridCell(0, 2, renderer));

		assertControl(render);
	}

	/**
	 * Tests whether the {@link EMFFormsLabelProvider} is used to get the labels of the control.
	 *
	 * @throws NoRendererFoundException
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws DatabindingFailedException
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testLabelServiceUsage() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		DatabindingFailedException, NoLabelFoundException {
		labelServiceUsage();
	}

	private void assertControl(Control render) {
		final Control textRender = Composite.class.cast(render).getChildren()[0];
		assertTrue(Text.class.isInstance(textRender));
		assertEquals(SWT.LEFT, Text.class.cast(textRender).getStyle()
			& SWT.LEFT);

		assertEquals("org_eclipse_emf_ecp_control_string", Text.class.cast(textRender).getData(CUSTOM_VARIANT));
	}

	@Override
	protected void mockControl() throws DatabindingFailedException {
		final EStructuralFeature eObject = EcoreFactory.eINSTANCE.createEAttribute();
		final EStructuralFeature eStructuralFeature = EcorePackage.eINSTANCE.getENamedElement_Name();
		super.mockControl(eObject, eStructuralFeature);
	}

	@Test
	public void testDatabindingServiceUsageInitialBinding() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException, NoLabelFoundException {
		final String initialValue = "initial";
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue,
			EcorePackage.eINSTANCE.getENamedElement_Name());
		final Text text = setUpDatabindingTest(mockedObservable);

		assertEquals(initialValue, text.getText());

	}

	@Test
	public void testDatabindingServiceUsageChangeObservable() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException, NoLabelFoundException {
		final String initialValue = "initial";
		final String changedValue = "changed";
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue,
			EcorePackage.eINSTANCE.getENamedElement_Name());

		final Text text = setUpDatabindingTest(mockedObservable);
		mockedObservable.setValue(changedValue);

		assertEquals(changedValue, text.getText());

	}

	@Test
	public void testDatabindingServiceUsageChangeControl() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException, NoLabelFoundException {
		final String initialValue = "initial";
		final String changedValue = "changed";
		final ObservingWritableValue mockedObservable = new ObservingWritableValue(realm, initialValue,
			EcorePackage.eINSTANCE.getENamedElement_Name());

		final Text text = setUpDatabindingTest(mockedObservable);
		SWTTestUtil.typeAndFocusOut(text, changedValue);

		assertEquals(changedValue, mockedObservable.getValue());

	}

	/**
	 * Universal set up stuff for the data binding test cases.
	 *
	 * @param mockedObservable
	 * @return
	 * @throws NoRendererFoundException
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws DatabindingFailedException if the databinding failed
	 * @throws NoLabelFoundException
	 */
	private Text setUpDatabindingTest(final ObservingWritableValue mockedObservable) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException, NoLabelFoundException {
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			Observables.constantObservableValue("antiException"));
		Mockito.reset(databindingService);
		mockDatabindingIsUnsettable();
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservable, new ObservingWritableValue(mockedObservable));

		final Control renderControl = renderControl(new SWTGridCell(0, 2, renderer));
		final Text text = (Text) Composite.class.cast(renderControl).getChildren()[0];
		return text;
	}

	/**
	 * Tests whether the {@link EMFFormsLabelProvider} is used to get the message of the text field of the text
	 * control.
	 *
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws NoRendererFoundException
	 * @throws DatabindingFailedException
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testLabelServiceUsageTextField() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		DatabindingFailedException, NoLabelFoundException {
		final IObservableValue testDisplayName = Observables.constantObservableValue("test-displayname", String.class);
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			testDisplayName);

		setMockLabelAlignment(LabelAlignment.LEFT);

		final TestObservableValue mockedObservableValue = mock(TestObservableValue.class);
		when(mockedObservableValue.getRealm()).thenReturn(realm);
		final EObject mockedEObject = mock(EObject.class);
		when(mockedEObject.eIsSet(any(EStructuralFeature.class))).thenReturn(true);
		when(mockedObservableValue.getObserved()).thenReturn(mockedEObject);
		final EStructuralFeature mockedEStructuralFeature = EcorePackage.eINSTANCE.getENamedElement_Name();
		when(mockedObservableValue.getValueType()).thenReturn(mockedEStructuralFeature);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableValue);

		final Control renderControl = renderControl(new SWTGridCell(0, 2, renderer));
		final Control textRender = Composite.class.cast(renderControl).getChildren()[0];
		assertTrue(Text.class.isInstance(textRender));

		final Text text = (Text) textRender;
		assertEquals(testDisplayName.getValue(), text.getMessage());
	}
}
