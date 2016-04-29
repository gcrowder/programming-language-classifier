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
package org.eclipse.emf.ecp.view.core.swt.tests;

import org.eclipse.emf.ecp.view.internal.core.swt.renderer.ViewRenderer_PTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * Test Suite for OSGi Tests.
 *
 * @author Eugen Neufeld
 *
 */
@RunWith(Suite.class)
@SuiteClasses({ CorrectDipose_PTest.class, DynamicDMR_PTest.class, ViewRenderer_PTest.class })
public class AllPluginTests {

}
