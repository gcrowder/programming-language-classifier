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
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.ecp.view.internal.validation.ECPSubstitutionLabelProvider;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Eugen
 *
 */
public class SubstitutionLabel_PTest {

	private ComposedAdapterFactory adapterFactory;
	private ECPSubstitutionLabelProvider labelProvider;
	private VTableDomainModelReference modelReference;
	private int modelReferenceAdapters;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		adapterFactory = new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE);
		labelProvider = new ECPSubstitutionLabelProvider(adapterFactory);
		modelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		modelReferenceAdapters = modelReference.eAdapters().size();
	}

	@After
	public void tearDown() throws Exception {
		adapterFactory.dispose();
		assertEquals(modelReferenceAdapters, modelReference.eAdapters().size());
	}

	@Test
	public void testFeatureLabelProvider() {
		final String featureLabel = labelProvider.getFeatureLabel(VViewPackage.eINSTANCE.getControl_LabelAlignment());
		assertEquals("Label Alignment", featureLabel);
	}

	@Test
	public void testEObjectLabelProvider() {
		final String eObjectLabel = labelProvider.getObjectLabel(modelReference);
		assertEquals("Table Domain Model Reference", eObjectLabel);
	}

}
