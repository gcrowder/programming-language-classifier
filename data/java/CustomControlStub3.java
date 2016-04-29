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
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.databinding.EMFProperties;
import org.eclipse.emf.ecp.view.spi.custom.model.ECPCustomControlChangeListener;
import org.eclipse.emf.ecp.view.spi.custom.model.ECPHardcodedReferences;
import org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * @author jfaltermeier
 *
 */
public class CustomControlStub3 extends ECPAbstractCustomControlSWT implements ECPHardcodedReferences {
	// private VFeaturePathDomainModelReference playersReference;
	// private VFeaturePathDomainModelReference nameReference;
	// private VFeaturePathDomainModelReference dateReference;
	private final Set<VDomainModelReference> features = new LinkedHashSet<VDomainModelReference>();

	public static final String CHANGE_NOTICED = "CHANGE!";

	private Label label;

	public CustomControlStub3() {
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
			final VFeaturePathDomainModelReference playersReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			playersReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());

			final VIndexDomainModelReference nameReference = VIndexdmrFactory.eINSTANCE
				.createIndexDomainModelReference();
			nameReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
			nameReference.setIndex(0);
			nameReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getLeague_Players());

			final VIndexDomainModelReference dateReference = VIndexdmrFactory.eINSTANCE
				.createIndexDomainModelReference();
			dateReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_DateOfBirth());
			nameReference.setIndex(0);
			dateReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getLeague_Players());

			features.add(playersReference);
			features.add(nameReference);
			features.add(dateReference);
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
	@Override
	public Control renderControl(SWTGridCell cell, Composite parent) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		if (cell.getColumn() == 0) {
			label = new Label(parent, SWT.NONE);
			label.setText("Players: ");
			return label;
		}
		if (cell.getColumn() == 1) {
			final Composite tableParent = new Composite(parent, SWT.NONE);

			final Composite tableComposite = new Composite(tableParent, SWT.NONE);
			final TableViewer tableViewer = new TableViewer(tableComposite, SWT.BORDER | SWT.FULL_SELECTION
				| SWT.V_SCROLL);
			tableViewer.getControl().setLayoutData(new GridData());
			tableComposite.setLayout(new TableColumnLayout());

			final IValueProperty[] valueProperties = EMFProperties.values(BowlingPackage.Literals.PLAYER__NAME,
				BowlingPackage.Literals.PLAYER__DATE_OF_BIRTH);

			createViewerBinding(getResolvedDomainModelReference(BowlingPackage.eINSTANCE.getLeague_Players()),
				tableViewer,
				valueProperties);

			registerChangeListener(getResolvedDomainModelReference(BowlingPackage.eINSTANCE.getLeague_Players()),
				new LeagueChangeListener());
			return tableParent;

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

	}

	public class LeagueChangeListener implements ECPCustomControlChangeListener {
		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.custom.model.ECPCustomControlChangeListener#notifyChanged()
		 */
		@Override
		public void notifyChanged() {
			label.setText(CHANGE_NOTICED);
		}

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

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#setEditable(boolean)
	 */
	@Override
	protected boolean setEditable(boolean editable) {
		label.setEnabled(editable);
		return super.setEditable(editable);
	}

}
