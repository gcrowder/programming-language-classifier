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
package org.eclipse.emfforms.spi.common.locale;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

/**
 * Test cases for the AbstractEMFFormsLocaleProvider.
 *
 * @author Eugen Neufeld
 *
 */
public class AbstractEMFFormsLocaleProvider_Test {

	private AbstractEMFFormsLocaleProvider provider;

	private static class MockAbstractEMFFormsLocaleProvider extends AbstractEMFFormsLocaleProvider {

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider#getLocale()
		 */
		@Override
		public Locale getLocale() {
			return Locale.ENGLISH;
		}

	}

	@Before
	public void setup() {
		provider = new MockAbstractEMFFormsLocaleProvider();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider#addEMFFormsLocaleChangeListener(org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleChangeListener)}
	 * .
	 */
	@Test
	public void testAddEMFFormsLocaleChangeListener() {
		final EMFFormsLocaleChangeListener listener = mock(EMFFormsLocaleChangeListener.class);
		provider.addEMFFormsLocaleChangeListener(listener);
		provider.notifyListeners();
		verify(listener).notifyLocaleChange();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider#addEMFFormsLocaleChangeListener(org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleChangeListener)}
	 * .
	 */
	@Test
	public void testAddEMFFormsLocaleChangeListenerTwice() {
		final EMFFormsLocaleChangeListener listener = mock(EMFFormsLocaleChangeListener.class);
		provider.addEMFFormsLocaleChangeListener(listener);
		provider.addEMFFormsLocaleChangeListener(listener);
		provider.notifyListeners();
		verify(listener).notifyLocaleChange();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider#addEMFFormsLocaleChangeListener(org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleChangeListener)}
	 * .
	 */
	@Test
	public void testAddEMFFormsLocaleChangeListenerDifferent() {
		final EMFFormsLocaleChangeListener listener1 = mock(EMFFormsLocaleChangeListener.class);
		provider.addEMFFormsLocaleChangeListener(listener1);
		final EMFFormsLocaleChangeListener listener2 = mock(EMFFormsLocaleChangeListener.class);
		provider.addEMFFormsLocaleChangeListener(listener2);
		provider.notifyListeners();
		verify(listener1).notifyLocaleChange();
		verify(listener2).notifyLocaleChange();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider#removeEMFFormsLocaleChangeListener(org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleChangeListener)}
	 * .
	 */
	@Test
	public void testRemoveEMFFormsLocaleChangeListener() {
		final EMFFormsLocaleChangeListener listener = mock(EMFFormsLocaleChangeListener.class);
		provider.addEMFFormsLocaleChangeListener(listener);
		provider.removeEMFFormsLocaleChangeListener(listener);
		provider.notifyListeners();
		verify(listener, never()).notifyLocaleChange();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.common.locale.AbstractEMFFormsLocaleProvider#removeEMFFormsLocaleChangeListener(org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleChangeListener)}
	 * .
	 */
	@Test
	public void testRemoveEMFFormsLocaleChangeListenerNotInList() {
		final EMFFormsLocaleChangeListener listener = mock(EMFFormsLocaleChangeListener.class);
		provider.removeEMFFormsLocaleChangeListener(listener);
		provider.notifyListeners();
		verify(listener, never()).notifyLocaleChange();
	}

}
