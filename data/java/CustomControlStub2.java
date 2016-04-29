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
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.emf.ecp.edit.internal.swt.util.SWTControl;
import org.eclipse.emf.ecp.view.spi.custom.model.ECPHardcodedReferences;
import org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * @author Jonas
 *
 */
public class CustomControlStub2 extends ECPAbstractCustomControlSWT implements ECPHardcodedReferences {

	public static final String LABEL_TEXT = "labelText";
	private static Label label;
	private static Composite parent;

	private final Set<VDomainModelReference> features = new LinkedHashSet<VDomainModelReference>();
	public static VDomainModelReference resolvedDomainModelReference;

	public CustomControlStub2() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.model.ECPHardcodedReferences#getNeededDomainModelReferences()
	 */
	@Override
	public Set<VDomainModelReference> getNeededDomainModelReferences() {
		if (features.isEmpty()) {
			final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
			domainModelReference.getDomainModelEReferencePath().add(
				BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
			features.add(domainModelReference);
		}
		return features;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#getGridDescription()
	 */
	@Override
	public SWTGridDescription getGridDescription() {
		// TODO Auto-generated method stub
		return GridDescriptionFactory.INSTANCE.createSimpleGrid(1, 2, null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#renderControl(org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell,
	 *      org.eclipse.swt.widgets.Composite)
	 */
	@SuppressWarnings({ "restriction", "deprecation" })
	@Override
	public Control renderControl(SWTGridCell cell, Composite parent) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		if (cell.getColumn() == 0) {
			setParent(parent);
			setLabel(new Label(parent, SWT.NONE));
			return label;

		}
		else if (cell.getColumn() == 1) {
			resolvedDomainModelReference = getResolvedDomainModelReference(BowlingPackage.eINSTANCE
				.getMerchandise_Name());
			final Composite createControl = getControl(SWTControl.class, resolvedDomainModelReference).createControl(
				parent);
			return createControl;
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.ui.ECPAbstractCustomControl#disposeCustomControl()
	 */
	@Override
	protected void disposeCustomControl() {
		// TODO Auto-generated method stub

	}

	/**
	 * @return the label
	 */
	public static Label getLabel() {
		return label;
	}

	/**
	 * @param label the label to set
	 */
	public static void setLabel(Label label) {
		CustomControlStub2.label = label;
	}

	/**
	 * @return the parent
	 */
	public static Composite getParent() {
		return parent;
	}

	/**
	 * @param parent the parent to set
	 */
	public static void setParent(Composite parent) {
		CustomControlStub2.parent = parent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#handleContentValidation()
	 */
	@Override
	protected void handleContentValidation() {
		// TODO Auto-generated method stub

	}

}
