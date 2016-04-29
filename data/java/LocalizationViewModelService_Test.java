/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.view.model.localization;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecp.view.internal.context.ViewModelContextImpl;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.LocalizationAdapter;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("restriction")
public class LocalizationViewModelService_Test {

	private LocalizationViewModelService localizationViewModelService;

	@Before
	public void setup() {
		localizationViewModelService = spy(new LocalizationViewModelService());
	}

	private String getLocalizableString(String name) {
		return name.substring(1);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateStatic() {
		final String viewName = "%view"; //$NON-NLS-1$
		final String layoutName = "%layout"; //$NON-NLS-1$
		final String controlName = "%control"; //$NON-NLS-1$
		final String control2Name = "Fix control name"; //$NON-NLS-1$
		final String viewLabel = "My super View"; //$NON-NLS-1$
		final String layoutLabel = "My super Layout"; //$NON-NLS-1$
		final String controlLabel = "My super Control"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();
		view.setName(viewName);
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		layout.setName(layoutName);
		view.getChildren().add(layout);
		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		control1.setName(controlName);
		layout.getChildren().add(control1);
		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		control2.setName(control2Name);
		layout.getChildren().add(control2);

		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getViewModel()).thenReturn(view);
		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(viewName))).thenReturn(viewLabel);
		when(adapter.localize(getLocalizableString(layoutName))).thenReturn(layoutLabel);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		view.eAdapters().add(adapter);

		localizationViewModelService.instantiate(viewModelContext);

		assertEquals(viewLabel, view.getLabel());
		assertEquals(layoutLabel, layout.getLabel());
		assertEquals(controlLabel, control1.getLabel());
		assertEquals(control2Name, control2.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateStaticNoLocalizationAdapter() {

		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		view.eAdapters().add(new AdapterImpl());

		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getViewModel()).thenReturn(view);

		localizationViewModelService.instantiate(viewModelContext);

		assertEquals("", view.getLabel()); //$NON-NLS-1$
		assertEquals("", control.getLabel()); //$NON-NLS-1$
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateDynamicNameChange() {

		final String controlName = "%control"; //$NON-NLS-1$
		final String controlName2 = "%control2"; //$NON-NLS-1$
		final String controlLabel = "My super Control"; //$NON-NLS-1$
		final String controlLabel2 = "My super Control Two"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(layout);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setName(controlName);
		layout.getChildren().add(control);

		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		when(adapter.localize(getLocalizableString(controlName2))).thenReturn(controlLabel2);
		view.eAdapters().add(adapter);

		final ViewModelContext viewModelContext = spy(new ViewModelContextImpl(view,
			EcoreFactory.eINSTANCE.createEObject(), localizationViewModelService));

		localizationViewModelService.instantiate(viewModelContext);
		control.setName(controlName2);

		assertEquals(controlLabel2, control.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateDynamicAddSingleControlOnRoot() {

		final String controlName = "%control"; //$NON-NLS-1$
		final String controlLabel = "My super Control"; //$NON-NLS-1$
		final VView view = VViewFactory.eINSTANCE.createView();

		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		view.eAdapters().add(adapter);

		final ViewModelContext viewModelContext = spy(new ViewModelContextImpl(view,
			EcoreFactory.eINSTANCE.createEObject(), localizationViewModelService));

		localizationViewModelService.instantiate(viewModelContext);

		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setName(controlName);
		view.getChildren().add(control);
		assertEquals(controlLabel, control.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateDynamicAddSingleControlOnChild() {

		final String controlName = "%control"; //$NON-NLS-1$

		final String controlLabel = "My super Control"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(layout);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		layout.getChildren().add(control);

		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		view.eAdapters().add(adapter);

		final ViewModelContext viewModelContext = spy(new ViewModelContextImpl(view,
			EcoreFactory.eINSTANCE.createEObject(), localizationViewModelService));

		localizationViewModelService.instantiate(viewModelContext);
		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		control2.setName(controlName);
		layout.getChildren().add(control2);

		assertEquals(controlLabel, control2.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateDynamicAddSubTreeOnRoot() {

		final String layoutName = "%layout"; //$NON-NLS-1$
		final String controlName = "%control"; //$NON-NLS-1$
		final String layoutLabel = "My super Layout"; //$NON-NLS-1$
		final String controlLabel = "My super Control"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();

		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		when(adapter.localize(getLocalizableString(layoutName))).thenReturn(layoutLabel);
		view.eAdapters().add(adapter);

		final ViewModelContext viewModelContext = spy(new ViewModelContextImpl(view,
			EcoreFactory.eINSTANCE.createEObject(), localizationViewModelService));

		localizationViewModelService.instantiate(viewModelContext);
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		layout.setName(layoutName);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setName(controlName);
		layout.getChildren().add(control);

		view.getChildren().add(layout);

		assertEquals(controlLabel, control.getLabel());
		assertEquals(layoutLabel, layout.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#instantiate(org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testInstantiateDynamicAddSubTreeOnChild() {

		final String layoutName = "%layout"; //$NON-NLS-1$
		final String controlName = "%control"; //$NON-NLS-1$
		final String layoutLabel = "My super Layout"; //$NON-NLS-1$
		final String controlLabel = "My super Control"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(layout);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		layout.getChildren().add(control);

		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(getLocalizableString(controlName))).thenReturn(controlLabel);
		when(adapter.localize(getLocalizableString(layoutName))).thenReturn(layoutLabel);
		view.eAdapters().add(adapter);
		final ViewModelContext viewModelContext = spy(new ViewModelContextImpl(view,
			EcoreFactory.eINSTANCE.createEObject(), localizationViewModelService));

		localizationViewModelService.instantiate(viewModelContext);

		final VVerticalLayout layout2 = VVerticalFactory.eINSTANCE.createVerticalLayout();
		layout2.setName(layoutName);
		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		control2.setName(controlName);
		layout2.getChildren().add(control2);

		layout.getChildren().add(layout2);

		assertEquals(controlLabel, control2.getLabel());
		assertEquals(layoutLabel, layout2.getLabel());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.view.model.localization.LocalizationViewModelService#getPriority()}.
	 */
	@Test
	public void testGetPriority() {
		assertEquals(-100, localizationViewModelService.getPriority());
	}

}
