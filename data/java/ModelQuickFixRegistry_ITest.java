/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Alexandra Buzila - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.quickfix.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.emf.ecp.quickfix.ModelQuickFix;
import org.eclipse.emf.ecp.quickfix.ModelQuickFixRegistry;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * @author Alexandra Buzila
 *
 */
public class ModelQuickFixRegistry_ITest {

	@Test
	public void serviceProviderRegistryTest() {
		final BundleContext bundleContext = FrameworkUtil.getBundle(getClass()).getBundleContext();
		// Register a QuickFix
		final DummyModelQuickFix quickFix = new DummyModelQuickFix("");
		quickFix.setPriority(1);
		final ServiceRegistration<ModelQuickFix> serviceRegistration = bundleContext.registerService(
			ModelQuickFix.class, quickFix, null);
		final ServiceReference<ModelQuickFixRegistry> serviceReference = bundleContext.getServiceReference(
			ModelQuickFixRegistry.class);
		assertNotNull("ModelQuickFixRegistry service reference was not found", serviceReference); //$NON-NLS-1$
		final ModelQuickFixRegistry registry = bundleContext.getService(serviceReference);
		assertNotNull("ModelQuickFixRegistry was not found", registry); //$NON-NLS-1$

		final List<ModelQuickFix> quickFixes = registry.getAllModelQuickFixes();
		assertTrue("ModelQuickFix service not found in the registry", quickFixes.contains(quickFix)); //$NON-NLS-1$
		serviceRegistration.unregister();
		bundleContext.ungetService(serviceReference);
	}

}
