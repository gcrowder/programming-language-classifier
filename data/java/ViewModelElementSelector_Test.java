/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.template.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.label.model.VLabel;
import org.eclipse.emf.ecp.view.spi.label.model.VLabelPackage;
import org.eclipse.emf.ecp.view.spi.label.model.VLabelStyle;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.template.model.VTStyleSelector;
import org.eclipse.emf.ecp.view.template.selector.viewModelElement.model.VTViewModelElementFactory;
import org.eclipse.emf.ecp.view.template.selector.viewModelElement.model.VTViewModelElementSelector;
import org.junit.Before;
import org.junit.Test;

/**
 * @author eugen
 *
 */
public class ViewModelElementSelector_Test {

	private VTViewModelElementSelector viewModelElementSelector;

	@Before
	public void setup() {
		viewModelElementSelector = VTViewModelElementFactory.eINSTANCE.createViewModelElementSelector();
	}

	@Test
	public void testApplicableWrongClassNoSubclasses() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getDiagnostic());
		viewModelElementSelector.setSelectSubclasses(false);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(VTStyleSelector.NOT_APPLICABLE.doubleValue(), specificity,
			0d);
	}

	@Test
	public void testApplicableWrongClassWithSubclasses() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getDiagnostic());
		viewModelElementSelector.setSelectSubclasses(true);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(VTStyleSelector.NOT_APPLICABLE.doubleValue(), specificity,
			0d);
	}

	@Test
	public void testApplicableCorrectClassNoSubclasses() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getElement());
		viewModelElementSelector.setSelectSubclasses(false);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(VTStyleSelector.NOT_APPLICABLE.doubleValue(), specificity,
			0d);
	}

	@Test
	public void testApplicableCorrectClassWithSubclasses() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getElement());
		viewModelElementSelector.setSelectSubclasses(true);
		viewModelElementSelector.setAttribute(VLabelPackage.Literals.LABEL__STYLE);
		viewModelElementSelector.setAttributeValue(VLabelStyle.H0);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		when(label.eGet(VLabelPackage.Literals.LABEL__STYLE)).thenReturn(VLabelStyle.H0);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(6, specificity, 0d);
	}

	@Test
	public void testApplicableCorrectClassWithSubclassesWrongAttributeValue() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getElement());
		viewModelElementSelector.setSelectSubclasses(true);
		viewModelElementSelector.setAttribute(VLabelPackage.Literals.LABEL__STYLE);
		viewModelElementSelector.setAttributeValue(VLabelStyle.H1);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		when(label.eGet(VLabelPackage.Literals.LABEL__STYLE)).thenReturn(VLabelStyle.H0);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(VTStyleSelector.NOT_APPLICABLE, specificity, 0d);
	}

	@Test
	public void testApplicableCorrectClassWithSubclassesWrongAttribute() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getElement());
		viewModelElementSelector.setSelectSubclasses(true);
		viewModelElementSelector.setAttribute(VViewPackage.eINSTANCE.getElement_Name());
		viewModelElementSelector.setAttributeValue(VLabelStyle.H1);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		when(label.eGet(VLabelPackage.Literals.LABEL__STYLE)).thenReturn(VLabelStyle.H0);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(VTStyleSelector.NOT_APPLICABLE, specificity, 0d);
	}

	@Test
	public void testApplicableCorrectClassWithSubclassesNoAttribute() {
		viewModelElementSelector.setClassType(VViewPackage.eINSTANCE.getElement());
		viewModelElementSelector.setSelectSubclasses(true);
		final VLabel label = mock(VLabel.class);
		when(label.eClass()).thenReturn(VLabelPackage.Literals.LABEL);
		when(label.eGet(VLabelPackage.Literals.LABEL__STYLE)).thenReturn(VLabelStyle.H0);
		final double specificity = viewModelElementSelector.isApplicable(
			label, mock(ViewModelContext.class));
		assertEquals(5, specificity, 0d);
	}
}