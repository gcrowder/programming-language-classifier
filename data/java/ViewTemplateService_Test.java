/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource Muenchen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.template.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.isNull;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Set;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.template.model.VTStyle;
import org.eclipse.emf.ecp.view.template.model.VTStyleProperty;
import org.eclipse.emf.ecp.view.template.model.VTStyleSelector;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplate;
import org.junit.Before;
import org.junit.Test;

public class ViewTemplateService_Test {

	private ViewTemplateProviderImpl templateProvider;
	private VTStyle mockedStyle;

	@Before
	public void setup() {
		templateProvider = new ViewTemplateProviderImpl();
	}

	@Test
	public void testNoTemplateModel() {
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(mock(VElement.class),
			mock(ViewModelContext.class));
		assertTrue(styleProperties.isEmpty());
	}

	@Test
	public void testVElementIsNull() {
		mockTemplate(1d);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(null,
			mock(ViewModelContext.class));
		assertTrue(styleProperties.isEmpty());
	}

	@Test
	public void testViewModelContextIsNull() {
		mockTemplate(1d);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(mock(VElement.class), null);
		assertTrue(styleProperties.isEmpty());
	}

	@Test
	public void testIsApplicable() {
		mockTemplate(1d);

		final VControl vElement = mock(VControl.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		when(vElement.getDomainModelReference()).thenReturn(dmr);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(vElement, viewModelContext);

		assertEquals(1, styleProperties.size());
		assertEquals(mockedStyle.getProperties().get(0), styleProperties.iterator().next());

	}

	@Test
	public void testMultipleStyles() {
		mockTemplate(1d);
		final VTStyle mockedStyle2 = mockStyle(1d);
		templateProvider.getViewTemplate().getStyles().add(mockedStyle2);

		final VControl vElement = mock(VControl.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		when(vElement.getDomainModelReference()).thenReturn(dmr);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(vElement, viewModelContext);

		assertEquals(2, styleProperties.size());
		assertTrue(styleProperties.contains(mockedStyle2.getProperties().get(0)));
		assertTrue(styleProperties.contains(templateProvider.getViewTemplate().getStyles().get(0).getProperties()
			.get(0)));

	}

	@Test
	public void testOnlyOneStyleProperty() {
		mockTemplate(1d);
		final VTStyle mockedStyle2 = mockStyle(10);
		templateProvider.getViewTemplate().getStyles().add(mockedStyle2);
		when(
			templateProvider.getViewTemplate().getStyles().get(0).getProperties().get(0)
				.equalStyles(mockedStyle2.getProperties().get(0))).thenReturn(true);
		final VControl vElement = mock(VControl.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		when(vElement.getDomainModelReference()).thenReturn(dmr);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(vElement, viewModelContext);

		assertEquals(1, styleProperties.size());
		assertEquals(mockedStyle2.getProperties().get(0), styleProperties.iterator().next());

	}

	private void mockTemplate(double specificity) {
		mockedStyle = mockStyle(specificity);
		final EList<VTStyle> styles = new BasicEList<VTStyle>();
		styles.add(mockedStyle);
		final VTViewTemplate viewTemplate = mock(VTViewTemplate.class);
		templateProvider.setViewTemplate(viewTemplate);
		when(viewTemplate.getStyles()).thenReturn(styles);
	}

	private VTStyle mockStyle(double specificity) {
		final VTStyle style = mock(VTStyle.class);
		final VTStyleSelector styleSelector = mock(VTStyleSelector.class);
		when(style.getSelector()).thenReturn(styleSelector);

		when(styleSelector.isApplicable(isNull(VElement.class), isNull(ViewModelContext.class))).thenReturn(
			VTStyleSelector.NOT_APPLICABLE);
		when(styleSelector.isApplicable(isNull(VElement.class), notNull(ViewModelContext.class))).thenReturn(
			VTStyleSelector.NOT_APPLICABLE);
		when(styleSelector.isApplicable(notNull(VElement.class), isNull(ViewModelContext.class))).thenReturn(
			VTStyleSelector.NOT_APPLICABLE);
		when(styleSelector.isApplicable(notNull(VElement.class), notNull(ViewModelContext.class))).thenReturn(
			specificity);

		final VTStyleProperty styleProperty = mock(VTStyleProperty.class);
		final EList<VTStyleProperty> properties = new BasicEList<VTStyleProperty>();
		properties.add(styleProperty);

		when(style.getProperties()).thenReturn(properties);

		return style;
	}

	@Test
	public void testIsNotApplicable() {
		mockTemplate(VTStyleSelector.NOT_APPLICABLE);

		final VControl vElement = mock(VControl.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		when(vElement.getDomainModelReference()).thenReturn(dmr);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final Set<VTStyleProperty> styleProperties = templateProvider.getStyleProperties(vElement, viewModelContext);

		assertEquals(0, styleProperties.size());

	}
}
