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
package org.eclipse.emf.ecp.view.model.provider.xmi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.junit.Test;

public class Migration_ITest {

	@Test
	public void testLoadingOutdatedViewmodel() {
		final URI uri = URI.createPlatformPluginURI("org.eclipse.emf.ecp.view.model.provider.xmi.test/User.view",
			false);
		final Resource resource = ViewModelFileExtensionsManager.loadResource(uri);
		assertEquals(1, resource.getContents().size());
		final EObject eObject = resource.getContents().get(0);
		assertTrue(VView.class.isInstance(eObject));
		final VView view = VView.class.cast(eObject);
		assertEquals(TaskPackage.eINSTANCE.getUser(), view.getRootEClass());
		assertEquals(2, view.getChildren().size());
	}

}
