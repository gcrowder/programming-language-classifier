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
package org.eclipse.emf.ecp.ide.view.service.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.emf.ecp.ide.view.internal.service.IDEViewModelRegistryImpl;
import org.eclipse.emf.ecp.ide.view.service.IDEViewModelRegistry;
import org.eclipse.emf.ecp.ide.view.service.ViewModelEditorCallback;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Alexandra Buzila
 *
 */
@SuppressWarnings("restriction")
public class IDEViewModelRegistry_PTest {

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = root.getProject("TestEcoreHelperProjectResources");
		// create resources to register and unregister
		if (!project.exists()) {
			installResourcesProject();
		}
	}

	private static void installResourcesProject() {
		final ProjectInstallerWizard wiz = new ProjectInstallerWizard();
		try {
			wiz.installExample(new NullProgressMonitor());
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	private VView view;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		view = VViewFactory.eINSTANCE.createView();
		view.setEcorePath("/TestIDEViewRegistryProjectResources/task.ecore");
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emf.ecp.ide.view.internal.service.IDEViewModelRegistryImpl#register(java.lang.String, org.eclipse.emf.ecp.view.spi.model.VView)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testRegisterAndUnregisterOneViewPerEcore() {
		final IDEViewModelRegistry registry = new IDEViewModelRegistryImpl();
		final String ecorePath = "test/ecore/path";
		Map<String, Set<VView>> ecoreViewMapping = null;
		Map<String, IResourceChangeListener> resourceChangeListeners = null;
		try {
			ecoreViewMapping = (Map<String, Set<VView>>) getFieldByName(registry, "ecoreViewMapping");
			resourceChangeListeners = (Map<String, IResourceChangeListener>) getFieldByName(registry,
				"resourceChangeListeners");
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		assertFalse("Registry already contains ecore path in ecoreViewMapping", ecoreViewMapping.containsKey(ecorePath));
		assertFalse("Registry already contains ecore path in resourceChangeListeners",
			resourceChangeListeners.containsKey(ecorePath));

		registry.register(ecorePath, view);

		assertTrue("Registering of ecore failed: ecoreViewMapping does not contain the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view));
		assertTrue(
			"Registering of ecore failed: resourceChangeListeners map does not contain the registered ecorePath.",
			resourceChangeListeners.containsKey(ecorePath));

		registry.unregister(ecorePath, view);

		assertFalse("Unregistering of ecore failed: ecoreViewMapping still contains the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view));
		assertFalse(
			"Unregistering of ecore failed: resourceChangeListeners map still contains the registered ecorePath.",
			resourceChangeListeners.containsKey(ecorePath));

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emf.ecp.ide.view.internal.service.IDEViewModelRegistryImpl#register(java.lang.String, org.eclipse.emf.ecp.view.spi.model.VView)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testRegisterAndUnregisterTwoViewsPerEcore() {

		final IDEViewModelRegistry registry = new IDEViewModelRegistryImpl();
		final String ecorePath = "test/ecore/path";

		final VView view1 = VViewFactory.eINSTANCE.createView();
		final VView view2 = VViewFactory.eINSTANCE.createView();

		Map<String, Set<VView>> ecoreViewMapping = null;
		Map<String, IResourceChangeListener> resourceChangeListeners = null;
		try {
			ecoreViewMapping = (Map<String, Set<VView>>) getFieldByName(registry, "ecoreViewMapping");
			resourceChangeListeners = (Map<String, IResourceChangeListener>) getFieldByName(registry,
				"resourceChangeListeners");
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		assertFalse("Registry already contains ecore path in ecoreViewMapping", ecoreViewMapping.containsKey(ecorePath));
		assertFalse("Registry already contains ecore path in resourceChangeListeners",
			resourceChangeListeners.containsKey(ecorePath));

		registry.register(ecorePath, view1);
		assertTrue("Registering of ecore failed: ecoreViewMapping does not contain the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view1));
		assertTrue(
			"Registering of ecore failed: resourceChangeListeners map does not contain the registered ecorePath.",
			resourceChangeListeners.containsKey(ecorePath));

		registry.register(ecorePath, view2);
		assertTrue("Registering of ecore failed: ecoreViewMapping does not contain the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view2));
		assertTrue(
			"Registering of ecore failed: resourceChangeListeners map does not contain the registered ecorePath.",
			resourceChangeListeners.containsKey(ecorePath));

		registry.unregister(ecorePath, view1);
		assertFalse("Unregistering of ecore failed: ecoreViewMapping still contains the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view1));
		assertTrue("Unregistering failed: incorrect view was removed from ecoreViewMapping.",
			ecoreViewMapping.get(ecorePath).contains(view2));
		assertTrue("Unregistering failed: incorrect ecorePath was removed from resourceChangeListeners map.",
			resourceChangeListeners.containsKey(ecorePath));

		registry.unregister(ecorePath, view2);
		assertFalse("Unregistering of ecore failed: ecoreViewMapping still contains the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view1));
		assertFalse("Unregistering of ecore failed: ecoreViewMapping still contains the registered view.",
			ecoreViewMapping.get(ecorePath).contains(view2));
		assertFalse(
			"Unregistering of ecore failed: resourceChangeListeners map still contains the registered ecorePath.",
			resourceChangeListeners.containsKey(ecorePath));

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emf.ecp.ide.view.internal.service.IDEViewModelRegistryImpl#registerViewModelEditor(org.eclipse.emf.ecp.view.spi.model.VView, org.eclipse.emf.ecp.ide.view.service.ViewModelEditorCallback)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testRegisterUnregisterViewModelEditor() {
		final IDEViewModelRegistry registry = new IDEViewModelRegistryImpl();
		Map<VView, ViewModelEditorCallback> viewModelViewModelEditorMapping = null;
		try {
			viewModelViewModelEditorMapping = (Map<VView, ViewModelEditorCallback>) getFieldByName(registry,
				"viewModelViewModelEditorMapping");
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		assertFalse("", viewModelViewModelEditorMapping.containsKey(view));

		final ViewModelEditorCallback viewModelEditor = new ViewModelEditorCallback() {

			@Override
			public void signalEcoreOutOfSync() {
				// TODO Auto-generated method stub

			}

			@Override
			public void reloadViewModel() {
				// TODO Auto-generated method stub

			}
		};
		try {
			registry.registerViewModelEditor(view, viewModelEditor);
		} catch (final IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		assertTrue("", viewModelViewModelEditorMapping.containsKey(view));

		registry.unregisterViewModelEditor(view, viewModelEditor);

		assertFalse("", viewModelViewModelEditorMapping.containsKey(view));
	}

	@Test
	public void testRegisterViewModelEditorWithEcoreChange() throws IOException, InterruptedException {
		// setup
		final CountDownLatch latch = new CountDownLatch(1);
		final IDEViewModelRegistry registry = new IDEViewModelRegistryImpl();
		final ViewModelEditorCallback callback = new ViewModelEditorCallback() {

			@Override
			public void signalEcoreOutOfSync() {
				latch.countDown();
			}

			@Override
			public void reloadViewModel() {
				// not tested
			}
		};
		registry.register(view.getEcorePath(), view);
		registry.registerViewModelEditor(view, callback);

		// act
		final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		IProject project = null;
		for (final IProject p : projects) {
			if ("TestIDEViewRegistryProjectResources".equals(p.getName())) {
				project = p;
			}
		}
		final IFile file = project.getFile("task.ecore");
		final Resource resource = loadEcore(file);
		// calling save without a change will trigger listener
		resource.save(null);

		// assert
		assertTrue("signalEcoreOutOfSync has not been called", latch.await(1, TimeUnit.SECONDS));
		registry.unregisterViewModelEditor(view, callback);
		registry.unregister(view.getEcorePath(), view);
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testRegisterViewModel() throws Exception {
		final IDEViewModelRegistry registry = new IDEViewModelRegistryImpl();
		final Map<VView, String> viewModelviewModelFileMapping = (Map<VView, String>) getFieldByName(registry,
			"viewModelviewModelFileMapping");

		assertTrue(viewModelviewModelFileMapping.isEmpty());
		final String path = "path1234";
		registry.registerViewModel(view, path);
		assertTrue(viewModelviewModelFileMapping.containsKey(view));
		assertEquals(path, viewModelviewModelFileMapping.get(view));
	}

	private Object getFieldByName(Object instance, String fieldName) throws Exception
	{
		final Field f = instance.getClass().getDeclaredField(fieldName);
		f.setAccessible(true);

		return f.get(instance);
	}

	private static Resource loadEcore(IFile file) {
		final ResourceSet resourceSet = new ResourceSetImpl();
		final Map<String, Object> extensionMap = resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap();
		extensionMap.put("*", new XMIResourceFactoryImpl());
		final URI uri = URI.createPlatformResourceURI(file.getFullPath().toString(), false);
		return resourceSet.getResource(uri, true);
	}

}
