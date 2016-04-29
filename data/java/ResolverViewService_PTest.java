/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.internal.viewproxy.resolver;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeListener;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.ecp.view.spi.viewproxy.model.VViewProxy;
import org.eclipse.emf.ecp.view.spi.viewproxy.model.VViewproxyFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener;
import org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener;
import org.junit.Before;
import org.junit.Test;

public class ResolverViewService_PTest {

	private static final String GENDER = "gender"; //$NON-NLS-1$
	private static final String ID = "player2"; //$NON-NLS-1$
	private static final String NAME = "name"; //$NON-NLS-1$
	private EObject domain;
	private ProxyResolverViewService service;
	private VView view;
	private VViewProxy viewProxy;

	@Before
	public void before() {
		domain = BowlingFactory.eINSTANCE.createPlayer();
		service = new ProxyResolverViewService();
		view = VViewFactory.eINSTANCE.createView();
		viewProxy = VViewproxyFactory.eINSTANCE.createViewProxy();
		view.getChildren().add(viewProxy);
	}

	@Test
	public void testNoProxyId() {
		service.instantiate(new DummyContext());
		assertEquals(1, view.getChildren().size());
		assertTrue(VVerticalLayout.class.isInstance(view.getChildren().get(0)));
		final VVerticalLayout layout = VVerticalLayout.class.cast(view.getChildren().get(0));
		assertEquals(1, layout.getChildren().size());
		assertEquals(NAME, layout.getChildren().get(0).getName());
	}

	@Test
	public void testWithProxyId() {
		viewProxy.setId(ID);
		service.instantiate(new DummyContext());
		assertEquals(1, view.getChildren().size());
		assertTrue(VVerticalLayout.class.isInstance(view.getChildren().get(0)));
		final VVerticalLayout layout = VVerticalLayout.class.cast(view.getChildren().get(0));
		assertEquals(1, layout.getChildren().size());
		assertEquals(GENDER, layout.getChildren().get(0).getName());
	}

	@Test
	public void testWithGeneratedView() {
		domain = EcoreFactory.eINSTANCE.createEClass();
		service.instantiate(new DummyContext());
		assertEquals(1, view.getChildren().size());
		assertTrue(VVerticalLayout.class.isInstance(view.getChildren().get(0)));
		final VVerticalLayout layout = VVerticalLayout.class.cast(view.getChildren().get(0));
		assertEquals(4, layout.getChildren().size());
	}

	private class DummyContext implements ViewModelContext {

		private final Map<String, Object> context = new LinkedHashMap<String, Object>();

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDomainChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void registerDomainChangeListener(ModelChangeListener modelChangeListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#unregisterDomainChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void unregisterDomainChangeListener(ModelChangeListener modelChangeListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getViewModel()
		 */
		@Override
		public VElement getViewModel() {
			return view;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getDomainModel()
		 */
		@Override
		public EObject getDomainModel() {
			return domain;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerViewChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void registerViewChangeListener(ModelChangeListener modelChangeListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#unregisterViewChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void unregisterViewChangeListener(ModelChangeListener modelChangeListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#dispose()
		 */
		@Override
		public void dispose() {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#hasService(java.lang.Class)
		 */
		@Override
		public <T> boolean hasService(Class<T> serviceType) {
			return false;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getService(java.lang.Class)
		 */
		@Override
		public <T> T getService(Class<T> serviceType) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecore.EStructuralFeature.Setting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VControl> getControlsFor(Setting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecp.common.spi.UniqueSetting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VElement> getControlsFor(org.eclipse.emf.ecp.common.spi.UniqueSetting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getContextValue(java.lang.String)
		 */
		@Override
		public Object getContextValue(String key) {
			return context.get(key);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#putContextValue(java.lang.String,
		 *      java.lang.Object)
		 */
		@Override
		public void putContextValue(String key, Object value) {
			context.put(key, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getChildContext(org.eclipse.emf.ecore.EObject,
		 *      org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.model.VView,
		 *      org.eclipse.emf.ecp.view.spi.context.ViewModelService[])
		 */
		@Override
		public ViewModelContext getChildContext(EObject eObject, VElement parent, VView vView,
			ViewModelService... viewModelServices) {
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDisposeListener(org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener)
		 */
		@Override
		public void registerDisposeListener(ViewModelContextDisposeListener listener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#addContextUser(java.lang.Object)
		 */
		@Override
		public void addContextUser(Object user) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#removeContextUser(java.lang.Object)
		 */
		@Override
		public void removeContextUser(Object user) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void registerEMFFormsContextListener(EMFFormsContextListener contextListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void unregisterEMFFormsContextListener(EMFFormsContextListener contextListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentContext()
		 */
		@Override
		public ViewModelContext getParentContext() {
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#changeDomainModel(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public void changeDomainModel(EObject newDomainModel) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentVElement()
		 */
		@Override
		public VElement getParentVElement() {
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void registerRootDomainModelChangeListener(RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void unregisterRootDomainModelChangeListener(
			RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

	}
}
