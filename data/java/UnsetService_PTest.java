/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.unset.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecp.common.spi.UniqueSetting;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.unset.UnsetService;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationFactory;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeListener;
import org.eclipse.emf.ecp.view.spi.model.VContainedContainer;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener;
import org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jfaltermeier
 *
 */
public class UnsetService_PTest {

	private Fan fan;
	private Merchandise merchandise;

	private final EStructuralFeature merchandisePriceFeature = BowlingPackage.eINSTANCE.getMerchandise_Price();
	private final EStructuralFeature merchandiseNameFeature = BowlingPackage.eINSTANCE.getMerchandise_Name();
	private final EStructuralFeature fanNameFeature = BowlingPackage.eINSTANCE.getFan_Name();
	private final BigDecimal price = new BigDecimal(19.84);
	private final String mercName = "Wimpel";
	private final String fanName = "Max Morlock";

	private VView view;

	private ViewModelContext context;
	private DefaultRealm realm;

	@Before
	public void before() {
		realm = new DefaultRealm();
		fan = BowlingFactory.eINSTANCE.createFan();
		merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise.setPrice(price);
		merchandise.setName(mercName);
		fan.setFavouriteMerchandise(merchandise);
		fan.setName(fanName);

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(fan.eClass());
	}

	@After
	public void after() {
		if (context != null) {
			context.dispose();
		}
		realm.dispose();
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Public methods
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@Test
	public void testInstantiate() {
		final UnsetService unsetService = new UnsetService();
		final ViewModelContextStub contextStub = new ViewModelContextStub();
		unsetService.instantiate(contextStub);
		assertTrue(contextStub.hasRegisteredViewListener);
		assertFalse(contextStub.hasRegisteredDomainListener);
	}

	@Test(expected = IllegalStateException.class)
	public void testInstantiateWithNullDomainModel() {
		final UnsetService unsetService = new UnsetService();
		final ViewModelContextStub contextStub = new ViewModelContextStub() {
			@Override
			public EObject getDomainModel() {
				return null;
			}
		};
		unsetService.instantiate(contextStub);
	}

	@Test(expected = IllegalStateException.class)
	public void testInstantiateWithNullViewModel() {
		final UnsetService unsetService = new UnsetService();
		final ViewModelContextStub contextStub = new ViewModelContextStub() {
			@Override
			public VElement getViewModel() {
				return null;
			}
		};
		unsetService.instantiate(contextStub);
	}

	@Test
	public void testDispose() {
		final UnsetService unsetService = new UnsetService();
		final ViewModelContextStub contextStub = new ViewModelContextStub();
		unsetService.instantiate(contextStub);
		assertTrue(contextStub.hasRegisteredViewListener);
		assertFalse(contextStub.hasRegisteredDomainListener);
		unsetService.dispose();
		assertFalse(contextStub.hasRegisteredViewListener);
		assertFalse(contextStub.hasRegisteredDomainListener);
	}

	@Test
	public void testGetPriority() {
		assertEquals(5, unsetService().getPriority());
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Init
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@Test
	public void testInitSingleControlInViewAllVisible() {
		addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInViewWithHiddenControl() {
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInViewWithHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInContainerAllVisible() {
		addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan());
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInContainerWithHiddenControl() {
		addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInContainerWithHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitSingleControlInContainerWithHiddenView() {
		addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInViewAllVisible() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInViewWithOneHiddenControl() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInViewWithTwoHiddenControls() {
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInViewWithHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInOneContainerAllVisible() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInOneContainerWithOneHiddenControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInOneContainerWithTwoHiddenControls() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInOneContainerWithHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInOneContainerWithHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersAllVisible() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersWithOneHiddenControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersWithOneHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersWithTwoHiddenControls() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersWithTwoHiddenContainers() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		container2.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitTwoControlsInTwoContainersWithHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Dynamic
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@Test
	public void testChangeSingleControlInViewAllVisibleToHiddenControl() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInViewAllVisibleToHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInViewWithHiddenControlToVisibleControl() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);
		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInViewWithHiddenControlToHiddenView() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInViewWithHiddenViewToHiddenControl() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInViewWithHiddenViewToVisibleView() {
		addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		view.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerAllVisibleToHiddenControl() {
		final VControl control = addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan());
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerAllVisibleToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerAllVisibleToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenControlToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenControlToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenControlToVisibleControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenContainerToHiddenControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenContainerToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenContainerToVisibleContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		container.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenViewToHiddenControl() {
		final VControl control = addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenViewToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeSingleControlInContainerWithHiddenViewToVisibleView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		view.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewAllVisibleToOneHiddenControl() {
		addControlToView(merchandisePriceReferenceFromFan());
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		control.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewAllVisibleToOneHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithOneHiddenControlToTwoVisibleControls() {
		addControlToView(merchandisePriceReferenceFromFan());
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithOneHiddenControlToTwoHiddenControls() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithOneHiddenControlToHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithTwoHiddenControlsToHiddenView() {
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithTwoHiddenControlsToOneVisibleControl() {
		addControlToView(merchandisePriceReferenceFromFan()).setVisible(false);
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithHiddenViewToHiddenControl() {
		addControlToView(merchandisePriceReferenceFromFan());
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInViewWithHiddenViewToVisibleView() {
		addControlToView(merchandisePriceReferenceFromFan());
		addControlToView(merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		view.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerAllVisibleToOneHiddenControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		control.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerAllVisibleToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerAllVisibleToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithOneHiddenControlToTwoVisibleControls() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithOneHiddenControlToTwoHiddenControls() {
		final VContainedContainer container = addVerticalLayoutToView();
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithOneHiddenControlToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithOneHiddenControlToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithTwoHiddenControlsToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithTwoHiddenControlsToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithTwoHiddenControlsToOneVisibleControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan()).setVisible(false);
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenContainerToOneHiddenControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenContainerToHiddenView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenContainerToVisibleContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		container.setVisible(false);
		unsetService();
		container.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenViewToOneHiddenControl() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenViewToHiddenContainer() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		container.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInOneContainerWithHiddenViewToVisibleView() {
		final VContainedContainer container = addVerticalLayoutToView();
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		addControlToContainer(container, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		view.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersAllVisibleToOneHiddenControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		unsetService();
		control.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersAllVisibleToOneHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		unsetService();
		container1.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersAllVisibleToHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenControlToTwoVisibleControls() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenControlToHiddenContainerOfHiddenControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container2.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenControlToTwoHiddenControls() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		final VControl control = addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenControlToHiddenContainerOfVisibleControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container1.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenControlToHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenContainerToHiddenControlInHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		final VControl control = addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenContainerToTwoVisibleContainers() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		container1.setVisible(true);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenContainerToHiddenControlInVisibleContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenContainerToTwoHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		container2.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithOneHiddenContainerToHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenControlsToOneHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		container1.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenControlsToHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(container2, merchandisePriceReferenceFromFan()).setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenControlsToOneVisibleControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan()).setVisible(false);
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		control.setVisible(false);
		unsetService();
		control.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenContainersToOneHiddenControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		container2.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenContainersToHiddenView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		container2.setVisible(false);
		unsetService();
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithTwoHiddenContainersToOneVisibleContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		container1.setVisible(false);
		container2.setVisible(false);
		unsetService();
		container1.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		container1.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithHiddenViewToOneHiddenControl() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		final VControl control = addControlToContainer(container2, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		control.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithHiddenViewToOneHiddenContainer() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		container1.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testChangeTwoControlsInTwoContainersWithHiddenViewToVisibleView() {
		final VContainedContainer container1 = addVerticalLayoutToView();
		final VContainedContainer container2 = addVerticalLayoutToView();
		addControlToContainer(container1, merchandisePriceReferenceFromFan());
		addControlToContainer(container2, merchandisePriceReferenceFromFan());
		view.setVisible(false);
		unsetService();
		view.setVisible(true);

		// we dont change the state of the domain model when shown again. if this changes assertions can be added here
		merchandise.setPrice(price);
		view.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// More specific test
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	@Test
	public void testChangeOtherViewFeatures() {
		final VControl control = addControlToView(merchandisePriceReferenceFromFan());
		unsetService();
		for (final EStructuralFeature feature : control.eClass().getEAllStructuralFeatures()) {
			if (feature == VViewPackage.eINSTANCE.getElement_Visible()) {
				continue;
			}
			if (!feature.isMany()) {
				control.eSet(feature, feature.getDefaultValue());
			}
		}
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
	}

	@Test
	public void testInitDifferentControls() {
		addControlToContainer(addVerticalLayoutToView(), merchandisePriceReferenceFromFan()).setVisible(false);
		addControlToContainer(addVerticalLayoutToView(), merchandiseNameReferenceFromFan());
		unsetService();
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(mercName, merchandise.getName());
		assertTrue(merchandise.eIsSet(merchandiseNameFeature));
	}

	@Test
	public void testChangeDifferentControls() {
		final VControl mercControl = addControlToContainer(addVerticalLayoutToView(),
			merchandisePriceReferenceFromFan());
		final VControl fanControl = addControlToContainer(addVerticalLayoutToView(), fanNameReference());
		unsetService();
		mercControl.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(fanName, fan.getName());
		assertTrue(fan.eIsSet(fanNameFeature));
		fanControl.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(fanNameFeature.getDefaultValue(), fan.getName());
		assertFalse(fan.eIsSet(fanNameFeature));
	}

	@Test
	public void testComplexCategorization() {
		final VCategorizationElement categorizationElement = VCategorizationFactory.eINSTANCE
			.createCategorizationElement();

		final VCategorization categorization1 = VCategorizationFactory.eINSTANCE.createCategorization();
		final VCategorization categorization1a = VCategorizationFactory.eINSTANCE.createCategorization();
		final VCategorization categorization2 = VCategorizationFactory.eINSTANCE.createCategorization();

		final VCategory category11 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl control11 = addControlToCategory(category11, merchandisePriceReferenceFromFan());
		final VCategory category12 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl control12 = addControlToCategory(category12, merchandisePriceReferenceFromFan());

		final VCategory category1a1 = VCategorizationFactory.eINSTANCE.createCategory();
		addControlToCategory(category1a1, merchandisePriceReferenceFromFan());
		final VCategory category1a2 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl control1a2 = addControlToCategory(category1a2, merchandiseNameReferenceFromFan());

		final VCategory category21 = VCategorizationFactory.eINSTANCE.createCategory();
		addControlToCategory(category21, merchandisePriceReferenceFromFan());
		final VCategory category22 = VCategorizationFactory.eINSTANCE.createCategory();
		addControlToCategory(category22, fanNameReference());

		categorization1.getCategorizations().add(category11);
		categorization1.getCategorizations().add(category12);

		categorization1a.getCategorizations().add(category1a1);
		categorization1a.getCategorizations().add(category1a2);

		categorization2.getCategorizations().add(category21);
		categorization2.getCategorizations().add(category22);

		categorization1.getCategorizations().add(categorization1a);
		categorizationElement.getCategorizations().add(categorization1);
		categorizationElement.getCategorizations().add(categorization2);
		view.getChildren().add(categorizationElement);

		unsetService();

		doLogicForComplexCategorizationTest(categorization2, control11, control12, category1a1, control1a2);

	}

	private void doLogicForComplexCategorizationTest(final VCategorization categorization2,
		final VControl control11, final VControl control12, final VCategory category1a1, final VControl control1a2) {

		control11.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(mercName, merchandise.getName());
		assertTrue(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanName, fan.getName());
		assertTrue(fan.eIsSet(fanNameFeature));

		control1a2.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
		assertFalse(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanName, fan.getName());
		assertTrue(fan.eIsSet(fanNameFeature));

		categorization2.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
		assertFalse(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanNameFeature.getDefaultValue(), fan.getName());
		assertFalse(fan.eIsSet(fanNameFeature));

		category1a1.setVisible(false);
		assertEquals(price, merchandise.getPrice());
		assertTrue(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
		assertFalse(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanNameFeature.getDefaultValue(), fan.getName());
		assertFalse(fan.eIsSet(fanNameFeature));

		control12.setVisible(false);
		assertEquals(merchandisePriceFeature.getDefaultValue(), merchandise.getPrice());
		assertFalse(merchandise.eIsSet(merchandisePriceFeature));
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
		assertFalse(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanNameFeature.getDefaultValue(), fan.getName());
		assertFalse(fan.eIsSet(fanNameFeature));
	}

	@Test
	public void testTable() {
		final Merchandise merc = BowlingFactory.eINSTANCE.createMerchandise();
		merc.setName("Foo");
		fan.getFanMerchandise().add(merc);

		final VTableControl table = VTableFactory.eINSTANCE.createTableControl();
		final VTableDomainModelReference tableDomainModelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		tableDomainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FanMerchandise());
		table.setDomainModelReference(tableDomainModelReference);

		final VFeaturePathDomainModelReference nameCol = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		nameCol.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		final VFeaturePathDomainModelReference priceCol = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		priceCol.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Price());
		VTableDomainModelReference.class.cast(table.getDomainModelReference()).getColumnDomainModelReferences()
			.add(nameCol);
		VTableDomainModelReference.class.cast(table.getDomainModelReference()).getColumnDomainModelReferences()
			.add(priceCol);

		view.getChildren().add(table);

		unsetService();
		assertEquals(1, fan.getFanMerchandise().size());
		assertEquals(merc, fan.getFanMerchandise().get(0));
		assertTrue(fan.eIsSet(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));

		table.setVisible(false);

		assertEquals(0, fan.getFanMerchandise().size());
		assertFalse(fan.eIsSet(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));
		assertEquals("Foo", merc.getName());
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Factory methods
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Creates the unset service.
	 *
	 * @return
	 */
	private UnsetService unsetService() {
		final UnsetService unsetService = new UnsetService();
		context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, fan);
		unsetService.instantiate(context);
		return unsetService;
	}

	/**
	 * Adds a control with the given feature path domain model reference as a direct child of the view.
	 *
	 * @param domainModelReference
	 * @return the created control
	 */
	private VControl addControlToView(VFeaturePathDomainModelReference domainModelReference) {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(domainModelReference);
		view.getChildren().add(control);
		return control;
	}

	/**
	 * Adds a control with the given feature path domain model reference as a direct child of the container.
	 *
	 * @param domainModelReference
	 * @return the created control
	 */
	private VControl addControlToContainer(VContainedContainer container,
		VFeaturePathDomainModelReference domainModelReference) {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(domainModelReference);
		container.getChildren().add(control);
		return control;
	}

	private VControl addControlToCategory(VCategory category, VFeaturePathDomainModelReference domainModelReference) {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(domainModelReference);
		category.setComposite(control);
		return control;
	}

	/**
	 * Adds a vertical layout as a direct child of the view.
	 *
	 * @return the created layout
	 */
	private VVerticalLayout addVerticalLayoutToView() {
		final VVerticalLayout layout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(layout);
		return layout;
	}

	/**
	 * References the player name from a league object.
	 *
	 * @return
	 */
	private VFeaturePathDomainModelReference merchandisePriceReferenceFromFan() {
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Price());
		domainModelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		return domainModelReference;
	}

	private VFeaturePathDomainModelReference merchandiseNameReferenceFromFan() {
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		domainModelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		return domainModelReference;
	}

	private VFeaturePathDomainModelReference fanNameReference() {
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		return domainModelReference;
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Helper- & stub classes
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * @author Jonas
	 *
	 */
	private class ViewModelContextStub implements ViewModelContext {

		private boolean hasRegisteredViewListener;
		private boolean hasRegisteredDomainListener;

		@Override
		public void unregisterViewChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredViewListener = false;
		}

		@Override
		public void unregisterDomainChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredDomainListener = false;
		}

		@Override
		public void registerViewChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredViewListener = true;
		}

		@Override
		public void registerDomainChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredDomainListener = true;
		}

		@Override
		public VElement getViewModel() {
			return view;
		}

		@Override
		public EObject getDomainModel() {
			return fan;
		}

		@Override
		public void dispose() {
		}

		@Override
		public <T> boolean hasService(Class<T> serviceType) {
			return false;
		}

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
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(UniqueSetting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VElement> getControlsFor(UniqueSetting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getContextValue(java.lang.String)
		 */
		@Override
		public Object getContextValue(String key) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#putContextValue(java.lang.String,
		 *      java.lang.Object)
		 */
		@Override
		public void putContextValue(String key, Object value) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDisposeListener(org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener)
		 */
		@Override
		public void registerDisposeListener(ViewModelContextDisposeListener listener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#addContextUser(java.lang.Object)
		 */
		@Override
		public void addContextUser(Object user) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#removeContextUser(java.lang.Object)
		 */
		@Override
		public void removeContextUser(Object user) {
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
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void registerEMFFormsContextListener(EMFFormsContextListener contextListener) {
			contextListener.contextInitialised();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void unregisterEMFFormsContextListener(EMFFormsContextListener contextListener) {
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
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#changeDomainModel(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public void changeDomainModel(EObject newDomainModel) {
			// TODO Auto-generated method stub

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
