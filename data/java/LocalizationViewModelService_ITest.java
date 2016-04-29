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
 ******************************************************************************/
package org.eclipse.emfforms.internal.view.model.localization;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Locale;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.LocalizationAdapter;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider;
import org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider;
import org.eclipse.emfforms.spi.localization.LocalizationServiceHelper;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceRegistration;

/**
 * Plugin Tests cases for the LocalizationViewModelService.
 *
 * @author Eugen Neufeld
 *
 */
public class LocalizationViewModelService_ITest {

	private static BundleContext bundleContext;
	private ServiceRegistration<EMFFormsLocaleProvider> registerService;
	private MockedEMFFormsLocaleProvider emfFormsLocaleProvider;

	private static class MockedEMFFormsLocaleProvider extends AbstractEMFFormsLocaleProvider {

		private Locale locale;

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider#getLocale()
		 */
		@Override
		public Locale getLocale() {
			return locale;
		}

		public void setLocale(Locale locale) {
			this.locale = locale;
			notifyListeners();
		}
	}

	@BeforeClass
	public static void setupBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(LocalizationViewModelService_ITest.class)
			.getBundleContext();

	}

	@Before
	public void setup() {
		emfFormsLocaleProvider = new MockedEMFFormsLocaleProvider();
		registerService = bundleContext.registerService(EMFFormsLocaleProvider.class, emfFormsLocaleProvider, null);
	}

	@After
	public void tearDown() {
		registerService.unregister();
	}

	@Test
	public void testDynamic() {
		final String viewName = "%view"; //$NON-NLS-1$
		final String layoutName = "%layout"; //$NON-NLS-1$
		final String controlName = "%control"; //$NON-NLS-1$

		final VView view = VViewFactory.eINSTANCE.createView();
		view.setName(viewName);
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		layout.setName(layoutName);
		view.getChildren().add(layout);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setName(controlName);
		layout.getChildren().add(control);

		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getViewModel()).thenReturn(view);
		final LocalizationAdapter adapter = mock(LocalizationAdapter.class);
		when(adapter.localize(Matchers.anyString())).then(new Answer<String>() {

			/**
			 * {@inheritDoc}
			 *
			 * @see org.mockito.stubbing.Answer#answer(org.mockito.invocation.InvocationOnMock)
			 * @generated
			 */
			@Override
			public String answer(InvocationOnMock invocation) throws Throwable {
				final Object object = invocation.getArguments()[0];
				final String parameter = object.toString();
				return LocalizationServiceHelper.getString(LocalizationViewModelService_ITest.class,
					parameter);
			}
		});
		view.eAdapters().add(adapter);

		emfFormsLocaleProvider.setLocale(Locale.ENGLISH);
		final LocalizationViewModelService localizationViewModelService = new LocalizationViewModelService();
		localizationViewModelService.instantiate(viewModelContext);

		assertEquals("My super View", view.getLabel()); //$NON-NLS-1$
		assertEquals("My super Layout", layout.getLabel()); //$NON-NLS-1$
		assertEquals("My super Control", control.getLabel()); //$NON-NLS-1$

		emfFormsLocaleProvider.setLocale(Locale.GERMAN);

		assertEquals("Mein super View", view.getLabel()); //$NON-NLS-1$
		assertEquals("Mein super Layout", layout.getLabel()); //$NON-NLS-1$
		assertEquals("Mein super Control", control.getLabel()); //$NON-NLS-1$
	}

}
