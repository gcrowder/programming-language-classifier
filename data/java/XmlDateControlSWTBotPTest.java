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
package org.eclipse.emf.ecp.view.ui.editor.test.controls;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.spi.GCCollectable;
import org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emf.edit.provider.ReflectiveItemProviderAdapterFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Referee;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotDateTime;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.junit.AfterClass;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 *
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class XmlDateControlSWTBotPTest extends ECPCommonSWTBotTest {

	private static double memBefore;
	private static double memAfter;

	private final boolean isDomainCollectable;

	private GCCollectable viewCollectable;
	private GCCollectable domainCollectable;
	private final Calendar calendar;

	public XmlDateControlSWTBotPTest(boolean isDomainCollectable) {
		this.isDomainCollectable = isDomainCollectable;

		// Creating date
		// [DE] 02.10.1986 11:11:11:111
		// [US] 10/02/1986 11:11:11:111
		calendar = Calendar.getInstance();
		calendar.set(Calendar.YEAR, 1986);
		calendar.set(Calendar.MONTH, 9);
		calendar.set(Calendar.DAY_OF_MONTH, 2);
		calendar.set(Calendar.HOUR, 11);
		calendar.set(Calendar.MINUTE, 11);
		calendar.set(Calendar.SECOND, 11);
		calendar.set(Calendar.MILLISECOND, 111);
	}

	@AfterClass
	public static void afterClass() {
		final double diff = Math.abs((memBefore - memAfter) / memBefore);
		assertTrue(diff < 0.05);
	}

	@Parameters
	public static Collection<Object[]> data() {
		final List<Object[]> data = new ArrayList<Object[]>();
		for (int i = 0; i < 24; i++) {
			data.add(new Boolean[] { false });
		}
		data.add(new Boolean[] { true });
		return data;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#logic()
	 */
	@Override
	public void logic() {
		final SWTBotText text = bot.text();
		text.setText("01.01.1989");
		final SWTBotButton button = bot.button();
		button.click();
		final SWTBotDateTime dateTime = bot.dateTime();
		dateTime.setDate(calendar.getTime());
	}

	@Override
	public void assertions(double before, double after) {
		XmlDateControlSWTBotPTest.memBefore += before;
		XmlDateControlSWTBotPTest.memAfter += after;

		if (getDomainObject() != null) {
			final XMLGregorianCalendar cal = (XMLGregorianCalendar) getDomainObject().eGet(
				BowlingPackage.eINSTANCE.getReferee_DateOfBirth(), true);
			assertEquals(DatatypeConstants.FIELD_UNDEFINED, cal.getTimezone());
			assertEquals(DatatypeConstants.FIELD_UNDEFINED, cal.getHour());
			assertEquals(DatatypeConstants.FIELD_UNDEFINED, cal.getMinute());
			assertEquals(DatatypeConstants.FIELD_UNDEFINED, cal.getSecond());
			assertEquals(DatatypeConstants.FIELD_UNDEFINED, cal.getMillisecond());
			assertEquals(1986, cal.getYear());
			assertEquals(10, cal.getMonth());
			assertEquals(2, cal.getDay());
			assertTrue("More than four adapter left on domain model element after dispose of ECPSWTView: "
				+ getDomainObject().eAdapters().size()
				+ " adapters. Not all adapters can be removed, but it's maybe time to get suspicious.",
				getDomainObject()
					.eAdapters().size() < 5);
		}

		assertTrue(getSWTViewCollectable().isCollectable());
		unsetSWTViewCollectable();
		unsetDomainObject();
		// if (isDomainCollectable) {
		// }
		// domainCollectable = null;
		assertTrue(viewCollectable.isCollectable());
		viewCollectable = null;
		assertTrue(domainCollectable.isCollectable());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createDomainObject()
	 */
	@Override
	public EObject createDomainObject() {
		Referee ref = (Referee) getDomainObject();

		if (isDomainCollectable) {
			// remove reference to domain object, since gc will be tested
			unsetDomainObject();
		}

		if (ref == null) {
			ref = BowlingFactory.eINSTANCE.createReferee();
			DatatypeFactory datatypeFactory;
			try {
				datatypeFactory = DatatypeFactory.newInstance();
			} catch (final DatatypeConfigurationException ex) {
				throw new RuntimeException(ex);
			}
			final XMLGregorianCalendar cal = datatypeFactory.newXMLGregorianCalendarDate(1960, 7, 7, 60);
			ref.setDateOfBirth(cal);
			final ResourceSet resourceSet = new ResourceSetImpl();
			final Resource resource = resourceSet.createResource(URI.createFileURI("foo.xmi"));
			resource.getContents().add(ref);
			addEditingDomain(resourceSet);
			memBefore = 0d;
			memAfter = 0d;
		}

		if (!isDomainCollectable) {
			setDomainObject(ref);
		}

		domainCollectable = new GCCollectable(ref);
		return ref;
	}

	private void addEditingDomain(ResourceSet resourceSet) {
		AdapterFactory adapterFactory = new ComposedAdapterFactory(
			ComposedAdapterFactory.Descriptor.Registry.INSTANCE);
		adapterFactory = new ComposedAdapterFactory(new AdapterFactory[] { adapterFactory,
			new ReflectiveItemProviderAdapterFactory() });
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(adapterFactory,
			new BasicCommandStack(), resourceSet);
		resourceSet.eAdapters().add(new AdapterFactoryEditingDomain.EditingDomainProvider(domain));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createView()
	 */
	@Override
	public VView createView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getGame());

		final VControl control = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainRef = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainRef.setDomainModelEFeature(BowlingPackage.eINSTANCE.getReferee_DateOfBirth());
		control.setDomainModelReference(domainRef);
		view.getChildren().add(control);

		viewCollectable = new GCCollectable(view);
		return view;
	}

}
