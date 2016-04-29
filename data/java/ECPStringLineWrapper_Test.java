/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.view.linewrapper.tests;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.ecp.internal.ui.view.linewrapper.ECPStringLineWrapper;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Eugen
 *
 */
public class ECPStringLineWrapper_Test {

	private ECPStringLineWrapper wrapper;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		wrapper = new ECPStringLineWrapper();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emf.ecp.internal.ui.view.linewrapper.ECPStringLineWrapper#modifyString(java.lang.String, org.eclipse.emf.ecore.EStructuralFeature.Setting)}
	 * .
	 */
	@Test
	public void testModifyStringShort() {
		final String modifyString = wrapper.modifyString("abc", null);
		assertEquals("abc", modifyString);
	}

	@Test
	public void testModifyStringLong() {
		final String modifyString = wrapper
			.modifyString(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et dolor at molestie. Suspendisse condimentum sem quis ligula semper.",
				null);
		assertEquals(
			"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et\ndolor at molestie. Suspendisse condimentum sem quis ligula semper.",
			modifyString);
	}

	@Test
	public void testModifyStringLongSingleWord() {
		final String modifyString = wrapper
			.modifyString(
				"Lorem ipsumdolorsitametconsecteturadipiscingelitPhasellusvariusetdoloratmolestie. Suspendisse condimentum sem quis ligula semper.",
				null);
		assertEquals(
			"Lorem\nipsumdolorsitametconsecteturadipiscingelitPhasellusvariusetdoloratmolestie.\nSuspendisse condimentum sem quis ligula semper.",
			modifyString);
	}

	@Test
	public void testModifyStringLongSingleWord2() {
		final String modifyString = wrapper
			.modifyString(
				"kurz, sehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrlang, kurz",
				null);
		assertEquals(
			"kurz,\nsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrsehrlang,\nkurz",
			modifyString);
	}

	@Test
	public void testModifyStringLongWithLineBreaks() {
		final String modifyString = wrapper
			.modifyString(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et dolor at molestie. Suspendisse condimentum sem quis ligula semper. \nLorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et dolor at molestie. Suspendisse condimentum sem quis ligula semper.",
				null);
		assertEquals(
			"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et\ndolor at molestie. Suspendisse condimentum sem quis ligula semper. \nLorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus varius et\ndolor at molestie. Suspendisse condimentum sem quis ligula semper.",
			modifyString);
	}
}
