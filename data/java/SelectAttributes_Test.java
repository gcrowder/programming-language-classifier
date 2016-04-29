/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.view.editor.controls.test;

import static org.junit.Assert.assertEquals;

import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecp.ui.view.editor.test.model.test.TestPackage;
import org.eclipse.emf.ecp.view.spi.editor.controls.Helper;
import org.junit.Test;

/**
 * @author Eugen Neufeld
 *
 */
public class SelectAttributes_Test {

	@Test
	public void testFindReferencableClasses() {
		final Set<EClass> datasegmentSubclasses = Helper.getDatasegmentSubclasses(TestPackage.eINSTANCE.getComponent());
		assertEquals(3, datasegmentSubclasses.size());

	}
}
