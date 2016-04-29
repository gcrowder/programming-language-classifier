/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.model.provider.xmi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.view.model.provider.xmi.ViewModelFileExtensionsManager.ExtensionDescription;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewModelLoadingProperties;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Jonas
 *
 */
public class ViewModelFileExtensionsManager_ITest {

	private static final String FILEPATH = "viewmodel.view";
	// private static final String FILEPATH2 = "viewmodel2.view";

	private static final String VIEWNAME = "the view name";
	private ViewModelFileExtensionsManager manager;
	private final EClass eClass1 = BowlingPackage.eINSTANCE.getLeague();

	@Before
	public void init() throws IOException {
		ViewModelFileExtensionsManager.dispose();
		manager = ViewModelFileExtensionsManager.getInstance();
	}

	// @Test
	// public void testGetExtensionURIs() {
	// final Map<URI, ExtensionDescription> extensionURIS = ViewModelFileExtensionsManager.getExtensionURIS();
	// assertEquals(2, extensionURIS.size());
	// final Set<URI> keySet = extensionURIS.keySet();
	// assertTrue(keySet.contains(uri(FILEPATH)));
	// assertTrue(keySet.contains(uri(FILEPATH2)));
	// }
	// private static URI uri(String filepath) {
	// return URI.createPlatformPluginURI("org.eclipse.emf.ecp.view.model.provider.xmi"
	// + "/" + filepath, false);
	// }

	@Test
	public void testGetExtensionURIs() {
		final Map<URI, List<ExtensionDescription>> extensionURIS = ViewModelFileExtensionsManager.getExtensionURIS();
		assertEquals(1, extensionURIS.size());
		final Set<URI> keySet = extensionURIS.keySet();
		assertTrue(keySet.contains(uri(FILEPATH)));

		assertEquals(2, extensionURIS.get(uri(FILEPATH)).size());
	}

	private static URI uri(String filepath) {
		return URI.createPlatformPluginURI("org.eclipse.emf.ecp.view.model.provider.xmi"
			+ "/" + filepath, false);
	}

	@Test
	public void testHasViewModelForNoFilterNullProperties() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.<String, String> emptyMap(), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		assertTrue(manager.hasViewModelFor(eObject, null));
	}

	@Test
	public void testHasViewModelForNoFilterEmptyProperty() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.<String, String> emptyMap(), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		assertTrue(manager.hasViewModelFor(eObject, VViewFactory.eINSTANCE.createViewModelLoadingProperties()));
	}

	@Test
	public void testHasViewModelForNoFilterWithInheritableProperty() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.<String, String> emptyMap(), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addInheritableProperty("key", "value");
		assertTrue(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForNoFilterWithNonInheritableProperty() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.<String, String> emptyMap(), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addNonInheritableProperty("key", "value");
		assertTrue(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForWithFilterNullProperties() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		assertFalse(manager.hasViewModelFor(eObject, null));
	}

	@Test
	public void testHasViewModelForWithFilterEmptyProperties() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		assertFalse(manager.hasViewModelFor(eObject, VViewFactory.eINSTANCE.createViewModelLoadingProperties()));
	}

	@Test
	public void testHasViewModelForWithFilterWithInheritableProperty() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addInheritableProperty("key", "value");
		assertTrue(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForWithFilterWithNonInheritableProperty() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addNonInheritableProperty("key", "value");
		assertTrue(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForFilterInheritablePropertyNotFit() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key1", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addInheritableProperty("key", "value");
		assertFalse(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForFilterNonInheritablePropertyNotFit() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key1", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addNonInheritableProperty("key", "value");
		assertFalse(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForFilterInheritablePropertyNotFit2() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final Map<String, String> filter = new LinkedHashMap<String, String>();
		filter.put("key", "value");
		filter.put("key11", "value");
		final ExtensionDescription extensionDescription = new ExtensionDescription(filter, "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addInheritableProperty("key", "value");
		viewModelLoadingProperties.addInheritableProperty("key1", "value");
		assertFalse(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testHasViewModelForFilterNonInheritablePropertyNotFit2() {
		final VView view = mock(VView.class);
		when(view.getRootEClass()).thenReturn(eClass1);
		final Map<String, String> filter = new LinkedHashMap<String, String>();
		filter.put("key", "value");
		filter.put("key11", "value");
		final ExtensionDescription extensionDescription = new ExtensionDescription(filter, "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VViewModelLoadingProperties viewModelLoadingProperties = VViewFactory.eINSTANCE
			.createViewModelLoadingProperties();
		viewModelLoadingProperties.addNonInheritableProperty("key", "value");
		viewModelLoadingProperties.addNonInheritableProperty("key1", "value");
		assertFalse(manager.hasViewModelFor(eObject, viewModelLoadingProperties));
	}

	@Test
	public void testCreateViewModelNoProperties() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(eClass1);
		view.setName(VIEWNAME);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.<String, String> emptyMap(), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);
		final VView foundView = manager.createView(eObject, null);
		assertNull(view.getLoadingProperties());
		assertEquals(VIEWNAME, foundView.getName());
	}

	@Test
	public void testCreateViewModelFittingProperties() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(eClass1);
		view.setName(VIEWNAME);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);

		final VViewModelLoadingProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
		properties.addInheritableProperty("key", "value");

		final VView foundView = manager.createView(eObject, properties);
		assertNotNull(foundView.getLoadingProperties());
		assertEquals("value", foundView.getLoadingProperties().get("key"));
		assertEquals(VIEWNAME, foundView.getName());
	}

	@Test
	public void testCreateViewModelUnFittingProperties() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(eClass1);
		view.setName(VIEWNAME);
		final ExtensionDescription extensionDescription = new ExtensionDescription(
			Collections.singletonMap("key", "value"), "");
		manager.registerView(view, extensionDescription);
		final EObject eObject = EcoreUtil.create(eClass1);

		final VViewModelLoadingProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();

		assertNull(manager.createView(eObject, properties));
	}

	@Test
	public void testCreateViewModelHigherPrioFittingProperties() {
		final VView view1 = VViewFactory.eINSTANCE.createView();
		view1.setRootEClass(eClass1);
		view1.setName(VIEWNAME + "1");
		final Map<String, String> filter1 = new LinkedHashMap<String, String>();
		filter1.put("a", "value");

		final ExtensionDescription extensionDescription1 = new ExtensionDescription(filter1, "");
		manager.registerView(view1, extensionDescription1);

		final VView view2 = VViewFactory.eINSTANCE.createView();
		view2.setRootEClass(eClass1);
		view2.setName(VIEWNAME + "2");
		final Map<String, String> filter2 = new LinkedHashMap<String, String>();
		filter2.put("a", "value");
		filter2.put("b", "value");

		final ExtensionDescription extensionDescription2 = new ExtensionDescription(filter2, "");
		manager.registerView(view2, extensionDescription2);

		final EObject eObject = EcoreUtil.create(eClass1);

		final VViewModelLoadingProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
		properties.addInheritableProperty("a", "value");
		properties.addInheritableProperty("b", "value");

		final VView foundView = manager.createView(eObject, properties);
		assertNotNull(foundView.getLoadingProperties());
		assertEquals("value", foundView.getLoadingProperties().get("a"));
		assertEquals("value", foundView.getLoadingProperties().get("b"));
		assertEquals(VIEWNAME + "2", foundView.getName());
	}

	@Test
	public void testCreateViewModelHigherPrioFittingProperties2() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(eClass1);
		view.setName(VIEWNAME);
		final Map<String, String> filter1 = new LinkedHashMap<String, String>();
		filter1.put("a", "value");
		filter1.put("b", "value");

		final ExtensionDescription extensionDescription1 = new ExtensionDescription(filter1, "");
		manager.registerView(view, extensionDescription1);

		final Map<String, String> filter2 = new LinkedHashMap<String, String>();
		filter2.put("z", "value");

		final ExtensionDescription extensionDescription2 = new ExtensionDescription(filter2, "");
		manager.registerView(view, extensionDescription2);

		final EObject eObject = EcoreUtil.create(eClass1);

		final VViewModelLoadingProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
		properties.addInheritableProperty("a", "value");
		properties.addInheritableProperty("b", "value");

		final VView foundView = manager.createView(eObject, properties);
		assertNotNull(foundView);
		assertNotNull(foundView.getLoadingProperties());
		assertEquals("value", foundView.getLoadingProperties().get("a"));
		assertEquals("value", foundView.getLoadingProperties().get("b"));
	}

	@Test
	public void testCreateViewModelHigherPrioFittingProperties3() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(eClass1);
		view.setName(VIEWNAME);
		final Map<String, String> filter1 = new LinkedHashMap<String, String>();
		filter1.put("z", "value");

		final ExtensionDescription extensionDescription1 = new ExtensionDescription(filter1, "");
		manager.registerView(view, extensionDescription1);

		final Map<String, String> filter2 = new LinkedHashMap<String, String>();
		filter2.put("a", "value");
		filter2.put("b", "value");

		final ExtensionDescription extensionDescription2 = new ExtensionDescription(filter2, "");
		manager.registerView(view, extensionDescription2);

		final EObject eObject = EcoreUtil.create(eClass1);

		final VViewModelLoadingProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
		properties.addInheritableProperty("a", "value");
		properties.addInheritableProperty("b", "value");

		final VView foundView = manager.createView(eObject, properties);
		assertNotNull(foundView);
		assertNotNull(foundView.getLoadingProperties());
		assertEquals("value", foundView.getLoadingProperties().get("a"));
		assertEquals("value", foundView.getLoadingProperties().get("b"));
	}
}
