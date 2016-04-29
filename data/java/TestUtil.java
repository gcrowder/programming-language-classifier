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
package org.eclipse.emf.ecp.view.stack.ui.swt.test;

import java.util.Arrays;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackFactory;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackItem;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackLayout;

public final class TestUtil {

	private TestUtil() {
	}

	public static VDomainModelReference createDMR(EStructuralFeature feature, EReference... references) {
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(feature);
		dmr.getDomainModelEReferencePath().addAll(Arrays.asList(references));
		return dmr;
	}

	public static VStackLayout createStack(EStructuralFeature feature, EReference... references) {
		final VStackLayout stackLayout = VStackFactory.eINSTANCE.createStackLayout();
		stackLayout.setDomainModelReference(createDMR(feature, references));
		return stackLayout;
	}

	public static VStackItem createItem(Object value, EStructuralFeature feature) {
		final VStackItem item = VStackFactory.eINSTANCE.createStackItem();
		item.setValue(value);
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(createDMR(feature));
		item.getChildren().add(control);
		return item;
	}

}
