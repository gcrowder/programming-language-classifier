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
package org.eclipse.emfforms.spi.localization;

import static org.junit.Assert.assertEquals;

import java.util.Locale;

import org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider;
import org.junit.Test;
import org.mockito.Mockito;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceRegistration;

/**
 * Plugin tests for the LocalizationServiceHelper.
 *
 * @author Eugen Neufeld
 *
 */
public class LocalizationServiceHelper_PTest {

	private static final String TEST_KEY = "testKey"; //$NON-NLS-1$

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.localization.LocalizationServiceHelper#getString(java.lang.Class, java.lang.String)}
	 * .
	 */
	@Test
	public void testGetStringWithoutLocaleProvider() {
		final String string = LocalizationServiceHelper.getString(getClass(), TEST_KEY);
		assertEquals("Test Value", string); //$NON-NLS-1$
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.localization.LocalizationServiceHelper#getString(java.lang.Class, java.lang.String)}
	 * .
	 */
	@Test
	public void testGetUndefinedStringWithoutLocaleProvider() {
		final String myKey = "MyKey"; //$NON-NLS-1$
		final String string = LocalizationServiceHelper.getString(getClass(), myKey);
		assertEquals(myKey, string);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.localization.LocalizationServiceHelper#getString(java.lang.Class, java.lang.String)}
	 * .
	 */
	@Test
	public void testGetStringWithLocaleProvider() {
		final BundleContext bundleContext = FrameworkUtil.getBundle(getClass()).getBundleContext();
		final Locale locale = new Locale("test"); //$NON-NLS-1$
		final EMFFormsLocaleProvider localeProvider = Mockito.mock(EMFFormsLocaleProvider.class);
		Mockito.when(localeProvider.getLocale()).thenReturn(locale);
		final ServiceRegistration<EMFFormsLocaleProvider> registerService = bundleContext.registerService(
			EMFFormsLocaleProvider.class, localeProvider, null);

		final String string = LocalizationServiceHelper.getString(getClass(), TEST_KEY);

		assertEquals("The Translated Test Value", string); //$NON-NLS-1$
		registerService.unregister();
	}

}
