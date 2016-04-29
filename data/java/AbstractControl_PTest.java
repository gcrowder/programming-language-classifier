/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 * Lucas Koehler - adjustments for databinding tests
 ******************************************************************************/
package org.eclipse.emf.ecp.view.internal.core.swt.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;

import java.util.Map;

import org.eclipse.core.databinding.observable.IObserving;
import org.eclipse.core.databinding.observable.Observables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceFactoryImpl;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.core.swt.AbstractControlSWTRenderer;
import org.eclipse.emf.ecp.view.spi.model.LabelAlignment;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.junit.Test;
import org.mockito.Mockito;

public abstract class AbstractControl_PTest {
	protected static final String CUSTOM_VARIANT = "org.eclipse.rap.rwt.customVariant"; //$NON-NLS-1$
	protected EMFFormsDatabinding databindingService;
	protected VTViewTemplateProvider templateProvider;
	protected AbstractControlSWTRenderer<VControl> renderer;
	protected EMFFormsLabelProvider labelProvider;

	private Resource createResource() {
		final Resource.Factory.Registry registry = Resource.Factory.Registry.INSTANCE;
		final Map<String, Object> extToFactoryMap = registry
			.getExtensionToFactoryMap();
		extToFactoryMap.put(Resource.Factory.Registry.DEFAULT_EXTENSION,
			new ResourceFactoryImpl());
		final ResourceSet resourceSet = new ResourceSetImpl();
		resourceSet.getPackageRegistry().put(EcorePackage.eNS_URI,
			EcorePackage.eINSTANCE);

		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(
				ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack(), resourceSet);
		resourceSet.eAdapters().add(
			new AdapterFactoryEditingDomain.EditingDomainProvider(domain));
		final Resource resource = resourceSet
			.createResource(URI.createURI("VIRTUAL"));
		return resource;

	}

	protected void mockControl(EObject eObject,
		final EStructuralFeature eStructuralFeature) throws DatabindingFailedException {
		final VDomainModelReference domainModelReference = Mockito
			.mock(VDomainModelReference.class);
		final Setting setting = mock(Setting.class);
		final Resource resource = createResource();
		resource.getContents().add(eObject);

		when(setting.getEObject()).thenReturn(eObject);
		when(setting.getEStructuralFeature()).thenReturn(eStructuralFeature);

		mockDatabindingIsUnsettable();

		Mockito.when(vControl.getDomainModelReference()).thenReturn(
			domainModelReference);
	}

	protected void mockDatabindingIsUnsettable() throws DatabindingFailedException {
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		when(structuralFeature.isUnsettable()).thenReturn(false);
		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);
		when(databindingService.getValueProperty(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			valueProperty);
	}

	protected void setMockLabelAlignment(LabelAlignment labelAlignment) {
		Mockito.when(vControl.getLabelAlignment()).thenReturn(labelAlignment);
	}

	protected ViewModelContext context;
	protected VControl vControl;
	protected Shell shell;

	protected void setup() throws DatabindingFailedException {
		vControl = Mockito.mock(VControl.class);
		mockControl();
		context = Mockito.mock(ViewModelContext.class);
		when(context.getDomainModel()).thenReturn(mock(EObject.class));
		shell = new Shell(Display.getDefault(), SWT.NONE);
	}

	protected void dispose() {
		shell.dispose();
	}

	@Test
	public void testGridDescriptionLabelAlignmentNone() {
		setMockLabelAlignment(LabelAlignment.NONE);
		final SWTGridDescription gridDescription = renderer.getGridDescription(GridDescriptionFactory.INSTANCE
			.createEmptyGridDescription());
		assertEquals(2, gridDescription.getColumns());
		assertEquals(1, gridDescription.getRows());
	}

	@Test
	public void testGridDescriptionLabelAlignmentLeft() {
		setMockLabelAlignment(LabelAlignment.LEFT);
		final SWTGridDescription gridDescription = renderer.getGridDescription(GridDescriptionFactory.INSTANCE
			.createEmptyGridDescription());
		assertEquals(3, gridDescription.getColumns());
		assertEquals(1, gridDescription.getRows());
	}

	@Test
	public void renderValidationIconLabelAlignmentNone()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		setMockLabelAlignment(LabelAlignment.NONE);
		renderValidationIcon(new SWTGridCell(0, 0, renderer));
	}

	@Test
	public void renderValidationIconLabelAlignmentLeft()
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		setMockLabelAlignment(LabelAlignment.LEFT);
		renderValidationIcon(new SWTGridCell(0, 1, renderer));
	}

	private void renderValidationIcon(SWTGridCell gridCell)
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final Control render = renderer.render(gridCell, shell);
		assertTrue(Label.class.isInstance(render));
		assertEquals("", Label.class.cast(render).getText());
	}

	protected void renderLabel(String text) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		setMockLabelAlignment(LabelAlignment.LEFT);
		final Control render = renderer.render(new SWTGridCell(0, 0, renderer), shell);
		assertTrue(Label.class.isInstance(render));
		assertEquals(text, Label.class.cast(render).getText());
	}

	/**
	 * Tests whether the {@link EMFFormsLabelProvider} is used to get the labels of a control.
	 *
	 * @throws NoRendererFoundException
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws NoLabelFoundException
	 */
	protected void labelServiceUsage() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		NoLabelFoundException {
		reset(labelProvider);
		final IObservableValue testDescription = Observables.constantObservableValue("test-description", String.class);
		final IObservableValue testDisplayName = Observables.constantObservableValue("test-displayname", String.class);
		when(labelProvider.getDescription(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			testDescription);
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			testDisplayName);

		setMockLabelAlignment(LabelAlignment.LEFT);
		final Control renderControl = renderControl(new SWTGridCell(0, 0, renderer));
		assertTrue(Label.class.isInstance(renderControl));

		final Label label = (Label) renderControl;
		assertEquals(testDescription.getValue(), label.getToolTipText());
		assertEquals(testDisplayName.getValue(), label.getText());
	}

	protected Control renderControl(SWTGridCell gridCell)
		throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		final Control render = renderer.render(gridCell, shell);
		return render;
	}

	protected abstract void mockControl() throws DatabindingFailedException;

	/**
	 * Helper Interface for mocking.
	 *
	 * @author Eugen Neufeld
	 *
	 */
	public interface TestObservableValue extends IObservableValue, IObserving {
	}
}
