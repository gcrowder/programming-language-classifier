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
package org.eclipse.emf.ecp.view.internal.context;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Dictionary;
import java.util.Hashtable;

import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecp.view.spi.context.EMFFormsLegacyServicesManager;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsViewServiceFactory;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsViewServicePolicy;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsViewServiceScope;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceRegistration;

@SuppressWarnings("rawtypes")
public class ViewModelContextImpl_ITest {

	private static BundleContext bundleContext;
	private EMFFormsViewServiceFactory<?> scopedServiceProvider;
	private ServiceRegistration<EMFFormsViewServiceFactory> registerService;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(ViewModelContextImpl_ITest.class)
			.getBundleContext();
	}

	@Before
	public void setUp() throws DatabindingFailedException {

	}

	@After
	public void tearDown() {
		if (registerService != null) {
			registerService.unregister();
		}
	}

	@Test
	public void testGlobalImmediateServiceProviderCalled() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final ViewModelContext vmc = spy(new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject()));
		verify(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		final Object object = vmc.getService(Object.class);
		assertNotNull(object);
		assertSame(mockedService, object);

		final Object object2 = vmc.getService(Object.class);
		assertNotNull(object2);
		assertSame(mockedService, object2);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider);
		inOrder.verify(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(2)).getService(Object.class);
	}

	@Test
	public void testGlobalImmediateServiceProviderNotCalledFromChild() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = spy(new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject()));

		final ViewModelContext vmcChild = spy(vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView()));

		final Object object = vmcChild.getService(Object.class);
		assertSame(mockedService, object);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider, vmcChild);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmcChild, times(1)).getService(Object.class);
		inOrder.verify(vmc, times(1)).getService(Object.class);
	}

	@Test
	public void testLocalImmediateServiceProviderCalled() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final ViewModelContext vmc = spy(new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject()));
		verify(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		final Object object = vmc.getService(Object.class);
		assertNotNull(object);
		assertSame(mockedService, object);

		final Object object2 = vmc.getService(Object.class);
		assertNotNull(object2);
		assertSame(mockedService, object2);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider);
		inOrder.verify(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(2)).getService(Object.class);
	}

	@Test
	public void testLocalImmediateServiceProviderCalledFromChild() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		final Object mockedService2 = mock(Object.class);
		doReturn(mockedService).doReturn(mockedService2).when(scopedServiceProvider)
			.createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = spy(new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject()));

		final ViewModelContext vmcChild = spy(vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView()));
		final Object object2 = vmcChild.getService(Object.class);
		assertSame(mockedService2, object2);
		final Object object = vmc.getService(Object.class);
		assertSame(mockedService, object);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider, vmcChild);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmcChild, times(1)).getService(Object.class);
		inOrder.verify(vmc, times(1)).getService(Object.class);
	}

	@Test
	public void testLocalLazyServiceProviderCalled() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final ViewModelContext vmc = spy(new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject()));
		final Object object = vmc.getService(Object.class);
		assertNotNull(object);
		assertSame(mockedService, object);

		final Object object2 = vmc.getService(Object.class);
		assertNotNull(object2);
		assertSame(mockedService, object2);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider);
		inOrder.verify(vmc, times(1)).getService(Object.class);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(1)).getService(Object.class);
	}

	@Test
	public void testLocalLazyServiceProviderCalledFromChild() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		final Object mockedService2 = mock(Object.class);
		doReturn(mockedService).doReturn(mockedService2).when(scopedServiceProvider)
			.createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = spy(new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject()));

		final ViewModelContext vmcChild = spy(vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView()));
		final Object object2 = vmcChild.getService(Object.class);
		assertSame(mockedService, object2);
		final Object object = vmc.getService(Object.class);
		assertSame(mockedService2, object);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider, vmcChild);
		inOrder.verify(vmcChild, times(1)).getService(Object.class);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(1)).getService(Object.class);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
	}

	@Test
	public void testGlobalLazyServiceProviderCalled() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final ViewModelContext vmc = spy(new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject()));
		final Object object = vmc.getService(Object.class);
		assertNotNull(object);
		assertSame(mockedService, object);

		final Object object2 = vmc.getService(Object.class);
		assertNotNull(object2);
		assertSame(mockedService, object2);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider);
		inOrder.verify(vmc, times(1)).getService(Object.class);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(1)).getService(Object.class);
	}

	@Test
	public void testGlobalLazyServiceProviderCalledFromChild() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProvider).getType();
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);

		registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class, scopedServiceProvider,
			dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = spy(new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject()));

		final ViewModelContext vmcChild = spy(vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView()));
		final Object object2 = vmcChild.getService(Object.class);
		assertSame(mockedService, object2);

		final Object object = vmc.getService(Object.class);
		assertSame(mockedService, object);

		final InOrder inOrder = inOrder(vmc, scopedServiceProvider, vmcChild);
		inOrder.verify(vmcChild, times(1)).getService(Object.class);
		inOrder.verify(vmc, times(1)).getService(Object.class);
		inOrder.verify(scopedServiceProvider, times(1)).createService(any(EMFFormsViewContext.class));
		inOrder.verify(vmc, times(1)).getService(Object.class);
	}

	@Test
	public void testLegacyServiceFactoryCalled() {

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		final EMFFormsLegacyServicesManager emfFormsLegacyServices = mock(EMFFormsLegacyServicesManager.class);

		final ServiceRegistration<EMFFormsLegacyServicesManager> serviceRegistration = bundleContext.registerService(
			EMFFormsLegacyServicesManager.class, emfFormsLegacyServices,
			dictionary);

		new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		verify(emfFormsLegacyServices).instantiate();
		serviceRegistration.unregister();
	}

	@Test
	public void testLegacyServiceFactoryCalledBeforeImmediateServices() {

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		final EMFFormsLegacyServicesManager emfFormsLegacyServices = mock(EMFFormsLegacyServicesManager.class);

		final ServiceRegistration<EMFFormsLegacyServicesManager> serviceRegistration = bundleContext.registerService(
			EMFFormsLegacyServicesManager.class, emfFormsLegacyServices,
			dictionary);
		final EMFFormsViewServiceFactory<?> scopedServiceProvider = mock(EMFFormsViewServiceFactory.class);
		when(scopedServiceProvider.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProvider.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProvider.getPriority()).thenReturn(1d);
		final Object mockedService = mock(Object.class);
		doReturn(mockedService).when(scopedServiceProvider).createService(any(EMFFormsViewContext.class));
		doReturn(Object.class).when(scopedServiceProvider).getType();
		Mockito.doAnswer(new Answer<Void>() {

			// BEGIN SUPRESS CATCH EXCEPTION
			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				registerService = bundleContext.registerService(EMFFormsViewServiceFactory.class,
					scopedServiceProvider,
					dictionary);
				return null;
			}
			// END SUPRESS CATCH EXCEPTION
		}).when(emfFormsLegacyServices).instantiate();

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		verify(emfFormsLegacyServices).instantiate();
		assertSame(mockedService, vmc.getService(Object.class));
		serviceRegistration.unregister();
	}

	@Test
	public void testOSGiServiceCalled() {
		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		final Object objectService = mock(Object.class);
		final ServiceRegistration<Object> serviceRegistration = bundleContext.registerService(
			Object.class, objectService, dictionary);

		final Object object = vmc.getService(Object.class);
		assertSame(objectService, object);

		serviceRegistration.unregister();
	}

	@Test
	public void testOSGiServiceCalledAfterUnregister() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		final Object objectService = mock(Object.class);

		final ServiceRegistration<Object> serviceRegistration = bundleContext.registerService(
			Object.class, objectService, dictionary);

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		final Object object = vmc.getService(Object.class);
		assertSame(objectService, object);

		serviceRegistration.unregister();

		final Object objectService2 = mock(Object.class);
		final ServiceRegistration<Object> serviceRegistration2 = bundleContext.registerService(
			Object.class, objectService2, dictionary);

		final Object object2 = vmc.getService(Object.class);
		assertSame(objectService2, object2);

		serviceRegistration2.unregister();
	}

	@Test
	public void testOSGiServiceUnregisteredOnDispose() {
		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		final Object objectService = mock(Object.class);
		final ServiceRegistration<Object> serviceRegistration = bundleContext.registerService(
			Object.class, objectService, dictionary);

		final Object object = vmc.getService(Object.class);
		assertSame(objectService, object);

		vmc.dispose();

		try {
			assertNull(serviceRegistration.getReference().getUsingBundles());
		} finally {
			serviceRegistration.unregister();
		}
	}

	@Test
	public void testNoAvailableServiceReturnsNull() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		final Object object = vmc.getService(Object.class);
		assertNull(object);
	}

	@Test
	public void testConstructorServiceBeforeLocalImmediateParentContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLI = mock(EMFFormsViewServiceFactory.class);
		doReturn(ViewModelService.class).when(scopedServiceProviderLI).getType();
		final Object mockedServiceLI = mock(ViewModelService.class);
		doReturn(mockedServiceLI).when(scopedServiceProviderLI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderLI.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLI, dictionary);

		final ViewModelService constructorService = mock(ViewModelService.class);
		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject(), constructorService);
		final ViewModelService service = vmc.getService(ViewModelService.class);
		assertSame(constructorService, service);

		serviceRegistrationLI.unregister();
	}

	@Test
	public void testConstructorServiceBeforeLocalImmediateChildContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLI = mock(EMFFormsViewServiceFactory.class);
		doReturn(ViewModelService.class).when(scopedServiceProviderLI).getType();
		final Object mockedServiceLI = mock(ViewModelService.class);
		doReturn(mockedServiceLI).when(scopedServiceProviderLI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderLI.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLI, dictionary);

		final ViewModelService constructorService = mock(ViewModelService.class);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject());

		final ViewModelContext vmcChild = vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView(), constructorService);

		final ViewModelService service = vmcChild.getService(ViewModelService.class);
		assertSame(constructorService, service);

		serviceRegistrationLI.unregister();
	}

	@Test
	public void testLocalImmediateBeforeLocalLazyParentContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLI).getType();
		final Object mockedServiceLI = mock(Object.class);
		doReturn(mockedServiceLI).when(scopedServiceProviderLI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderLI.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLI, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLL).getType();
		final Object mockedServiceLL = mock(Object.class);
		doReturn(mockedServiceLL).when(scopedServiceProviderLL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderLL.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLL, dictionary);

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		final Object service = vmc.getService(Object.class);
		assertSame(mockedServiceLI, service);

		serviceRegistrationLI.unregister();
		serviceRegistrationLL.unregister();
	}

	@Test
	public void testLocalImmediateBeforeLocalLazyChildContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLI).getType();
		final Object mockedServiceLI = mock(Object.class);
		doReturn(mockedServiceLI).when(scopedServiceProviderLI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderLI.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLI, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLL).getType();
		final Object mockedServiceLL = mock(Object.class);
		doReturn(mockedServiceLL).when(scopedServiceProviderLL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderLL.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLL, dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject());

		final ViewModelContext vmcChild = vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView());

		final Object service = vmcChild.getService(Object.class);
		assertSame(mockedServiceLI, service);

		serviceRegistrationLI.unregister();
		serviceRegistrationLL.unregister();
	}

	@Test
	public void testLocalLazyBeforeGlobalImmediateParentContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLL).getType();
		final Object mockedServiceLL = mock(Object.class);
		doReturn(mockedServiceLL).when(scopedServiceProviderLL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderLL.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLL, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGI).getType();
		final Object mockedServiceGI = mock(Object.class);
		doReturn(mockedServiceGI).when(scopedServiceProviderGI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderGI.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGI, dictionary);

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		final Object service = vmc.getService(Object.class);
		assertSame(mockedServiceGI, service);

		serviceRegistrationGI.unregister();
		serviceRegistrationLL.unregister();
	}

	@Test
	public void testLocalLazyBeforeGlobalImmediateChildContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderLL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderLL).getType();
		final Object mockedServiceLL = mock(Object.class);
		doReturn(mockedServiceLL).when(scopedServiceProviderLL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderLL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderLL.getScope()).thenReturn(EMFFormsViewServiceScope.LOCAL);
		when(scopedServiceProviderLL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationLL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderLL, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGI).getType();
		final Object mockedServiceGI = mock(Object.class);
		doReturn(mockedServiceGI).when(scopedServiceProviderGI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderGI.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGI, dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject());

		final ViewModelContext vmcChild = vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView());

		final Object service = vmcChild.getService(Object.class);
		assertSame(mockedServiceLL, service);

		serviceRegistrationGI.unregister();
		serviceRegistrationLL.unregister();
	}

	@Test
	public void testGlobalImmediateBeforeGlobalLazyParentContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGI).getType();
		final Object mockedServiceGI = mock(Object.class);
		doReturn(mockedServiceGI).when(scopedServiceProviderGI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderGI.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGI, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGL).getType();
		final Object mockedServiceGL = mock(Object.class);
		doReturn(mockedServiceGL).when(scopedServiceProviderGL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderGL.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGL, dictionary);

		final ViewModelContext vmc = new ViewModelContextImpl(VViewFactory.eINSTANCE.createView(),
			EcoreFactory.eINSTANCE.createEObject());
		final Object service = vmc.getService(Object.class);
		assertSame(mockedServiceGI, service);

		serviceRegistrationGI.unregister();
		serviceRegistrationGL.unregister();
	}

	@Test
	public void testGlobalImmediateBeforeGlobalLazyChildContext() {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGI = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGI).getType();
		final Object mockedServiceGI = mock(Object.class);
		doReturn(mockedServiceGI).when(scopedServiceProviderGI).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGI.getPolicy()).thenReturn(EMFFormsViewServicePolicy.IMMEDIATE);
		when(scopedServiceProviderGI.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGI.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGI = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGI, dictionary);

		final EMFFormsViewServiceFactory<?> scopedServiceProviderGL = mock(EMFFormsViewServiceFactory.class);
		doReturn(Object.class).when(scopedServiceProviderGL).getType();
		final Object mockedServiceGL = mock(Object.class);
		doReturn(mockedServiceGL).when(scopedServiceProviderGL).createService(any(EMFFormsViewContext.class));
		when(scopedServiceProviderGL.getPolicy()).thenReturn(EMFFormsViewServicePolicy.LAZY);
		when(scopedServiceProviderGL.getScope()).thenReturn(EMFFormsViewServiceScope.GLOBAL);
		when(scopedServiceProviderGL.getPriority()).thenReturn(1d);
		final ServiceRegistration<EMFFormsViewServiceFactory> serviceRegistrationGL = bundleContext.registerService(
			EMFFormsViewServiceFactory.class, scopedServiceProviderGL, dictionary);

		final VView parentView = VViewFactory.eINSTANCE.createView();
		final ViewModelContext vmc = new ViewModelContextImpl(parentView, EcoreFactory.eINSTANCE.createEObject());

		final ViewModelContext vmcChild = vmc.getChildContext(EcoreFactory.eINSTANCE.createEObject(), parentView,
			VViewFactory.eINSTANCE.createView());

		final Object service = vmcChild.getService(Object.class);
		assertSame(mockedServiceGI, service);

		serviceRegistrationGI.unregister();
		serviceRegistrationGL.unregister();
	}
}
