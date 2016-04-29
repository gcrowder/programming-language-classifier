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
package org.eclipse.emf.ecp.view.model.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewModelLoadingProperties;
import org.junit.Before;
import org.junit.Test;

public class DefaultViewModelProperties_Test {

	private static final String KEY1 = "key1";
	private static final String VALUE1 = "value1";

	private static final String KEY2 = "key2";
	private static final String VALUE2 = "value2";

	private VViewModelLoadingProperties properties;

	@Before
	public void before() {
		properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
	}

	@Test
	public void testContainsKeyEmpty() {
		assertFalse(properties.containsKey(KEY1));
	}

	@Test
	public void testContainsKeyInherited() {
		properties.getInheritableProperties().put(KEY1, VALUE1);
		assertTrue(properties.containsKey(KEY1));
	}

	@Test
	public void testContainsKeyNonInherited() {
		properties.getNonInheritableProperties().put(KEY1, VALUE1);
		assertTrue(properties.containsKey(KEY1));
	}

	@Test
	public void testGetEmpty() {
		assertNull(properties.get(KEY1));
	}

	@Test
	public void testGetInherited() {
		properties.getInheritableProperties().put(KEY1, VALUE1);
		assertEquals(VALUE1, properties.get(KEY1));
	}

	@Test
	public void testGetNonInherited() {
		properties.getNonInheritableProperties().put(KEY1, VALUE1);
		assertEquals(VALUE1, properties.get(KEY1));
	}

	@Test
	public void testInherit() {
		properties.getInheritableProperties().put(KEY1, VALUE1);
		properties.getNonInheritableProperties().put(KEY2, VALUE2);
		final VViewModelLoadingProperties inherit = (VViewModelLoadingProperties) properties.inherit();
		assertTrue(inherit.getNonInheritableProperties().isEmpty());
		assertEquals(1, inherit.getInheritableProperties().size());
		assertEquals(VALUE1, inherit.getInheritableProperties().get(KEY1));
		assertEquals(1, properties.getInheritableProperties().size());
		assertEquals(1, properties.getNonInheritableProperties().size());
		assertEquals(VALUE1, properties.get(KEY1));
		assertEquals(VALUE2, properties.get(KEY2));
	}

	@Test
	public void testAddInheritablePropertyEmpty() {
		assertNull(properties.addInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getNonInheritableProperties().isEmpty());
		assertEquals(1, properties.getInheritableProperties().size());
		assertEquals(VALUE1, properties.getInheritableProperties().get(KEY1));
	}

	@Test
	public void testAddInheritablePropertyKeyUsedInInheritable() {
		properties.getInheritableProperties().put(KEY1, VALUE2);
		assertEquals(VALUE2, properties.addInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getNonInheritableProperties().isEmpty());
		assertEquals(1, properties.getInheritableProperties().size());
		assertEquals(VALUE1, properties.getInheritableProperties().get(KEY1));
	}

	@Test
	public void testAddInheritablePropertyKeyUsedInNonInheritable() {
		properties.getNonInheritableProperties().put(KEY1, VALUE2);
		assertEquals(VALUE2, properties.addInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getNonInheritableProperties().isEmpty());
		assertEquals(1, properties.getInheritableProperties().size());
		assertEquals(VALUE1, properties.getInheritableProperties().get(KEY1));
	}

	@Test
	public void testAddNonInheritablePropertyEmpty() {
		assertNull(properties.addNonInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getInheritableProperties().isEmpty());
		assertEquals(1, properties.getNonInheritableProperties().size());
		assertEquals(VALUE1, properties.getNonInheritableProperties().get(KEY1));
	}

	@Test
	public void testAddNonInheritablePropertyKeyUsedInInheritable() {
		properties.getInheritableProperties().put(KEY1, VALUE2);
		assertEquals(VALUE2, properties.addNonInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getInheritableProperties().isEmpty());
		assertEquals(1, properties.getNonInheritableProperties().size());
		assertEquals(VALUE1, properties.getNonInheritableProperties().get(KEY1));
	}

	@Test
	public void testAddNonInheritablePropertyKeyUsedInNonInheritable() {
		properties.getNonInheritableProperties().put(KEY1, VALUE2);
		assertEquals(VALUE2, properties.addNonInheritableProperty(KEY1, VALUE1));
		assertTrue(properties.getInheritableProperties().isEmpty());
		assertEquals(1, properties.getNonInheritableProperties().size());
		assertEquals(VALUE1, properties.getNonInheritableProperties().get(KEY1));
	}

}
