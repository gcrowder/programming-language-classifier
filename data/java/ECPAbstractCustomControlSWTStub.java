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
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.edit.internal.swt.util.SWTControl;
import org.eclipse.emf.ecp.view.spi.custom.model.ECPHardcodedReferences;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomPackage;
import org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT;
import org.eclipse.emf.ecp.view.spi.model.VDiagnostic;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * @author Jonas
 *
 */
public class ECPAbstractCustomControlSWTStub extends ECPAbstractCustomControlSWT implements ECPHardcodedReferences {

	private static final String TEST_MESSAGE = "TestMessage";
	private static final String TEST_TITEL = "TestTitel";
	private boolean rendered;
	private int lastValidationSeverity;
	private EStructuralFeature lastValidationFeature;
	private boolean disposed;
	private Label label;
	private Composite textControl;
	private final boolean withControl;
	private boolean validationReseted;
	private Label validationLabel;
	private Button button;

	/**
	 * @return the validationLabel
	 */
	public Label getValidationLabel() {
		return validationLabel;
	}

	public ECPAbstractCustomControlSWTStub() {
		this(false);
	}

	public ECPAbstractCustomControlSWTStub(boolean withControl) {
		super();
		this.withControl = withControl;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#handleContentValidation()
	 */
	@Override
	protected void handleContentValidation() {
		if (getCustomControl().getDiagnostic() == null) {
			setValidationReseted(true);
		}
		final VDiagnostic diagnostic = getCustomControl().getDiagnostic();
		if (diagnostic.getDiagnostics().size() == 0) {
			setValidationReseted(true);
		}
		for (final Object diagnosticObject : diagnostic.getDiagnostics()) {
			final Diagnostic diagnostic2 = (Diagnostic) diagnosticObject;
			if (diagnostic2.getSeverity() == Diagnostic.OK) {
				setValidationReseted(true);
			} else {
				setLastValidationSeverity(diagnostic2.getSeverity());
				setLastValidationFeature((EStructuralFeature) diagnostic2.getData().get(1));
			}
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.swt.ECPAbstractCustomControlSWT#getGridDescription()
	 */
	@Override
	public SWTGridDescription getGridDescription() {
		if (!withControl) {
			return GridDescriptionFactory.INSTANCE.createSimpleGrid(1, 2, null);
		}
		return GridDescriptionFactory.INSTANCE.createSimpleGrid(1, 3, null);
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
			label.setText(ECPAbstractCustomControlSWT_PTest.LABELTEXT);
			setRendered(true);
			return label;
		}
		if (cell.getColumn() == 1 && !withControl) {
			button = new Button(parent, SWT.PUSH);
			return getButton();
		}
		if (cell.getColumn() == 1) {
			validationLabel = createValidationIcon(parent);
			return validationLabel;
		}
		if (cell.getColumn() == 2) {
			final VFeaturePathDomainModelReference controlFeature = (VFeaturePathDomainModelReference) getResolvedDomainModelReference(VCustomPackage.eINSTANCE
				.getCustomControl_BundleName());
			setTextControl(getControl(SWTControl.class, controlFeature).createControl(parent));
			return getTextControl();
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
		setDisposed(true);
		if (label != null) {
			label.dispose();
		}
		if (textControl != null) {
			textControl.dispose();
		}
		if (validationLabel != null) {
			validationLabel.dispose();
		}
	}

	/**
	 * @param composite
	 */
	public void createValidationLabelInStub(Composite composite) {
		super.createValidationIcon(composite);

	}

	/**
	 * @return the rendered
	 */
	public boolean isRendered() {
		return rendered;
	}

	/**
	 * @param rendered the rendered to set
	 */
	public void setRendered(boolean rendered) {
		this.rendered = rendered;
	}

	/**
	 * @return the lastValidationSeverity
	 */
	public int getLastValidationSeverity() {
		return lastValidationSeverity;
	}

	/**
	 * @param lastValidationSeverity the lastValidationSeverity to set
	 */
	public void setLastValidationSeverity(int lastValidationSeverity) {
		this.lastValidationSeverity = lastValidationSeverity;
	}

	/**
	 * @return the lastValidationFeature
	 */
	public EStructuralFeature getLastValidationFeature() {
		return lastValidationFeature;
	}

	/**
	 * @param lastValidationFeature the lastValidationFeature to set
	 */
	public void setLastValidationFeature(EStructuralFeature lastValidationFeature) {
		this.lastValidationFeature = lastValidationFeature;
	}

	/**
	 * @return the disposed
	 */
	public boolean isDisposed() {
		return disposed;
	}

	/**
	 * @param disposed the disposed to set
	 */
	public void setDisposed(boolean disposed) {
		this.disposed = disposed;
	}

	/**
	 * @return the textControl
	 */
	public Composite getTextControl() {
		return textControl;
	}

	public Label getLabel() {
		return label;
	}

	/**
	 * @param textControl the textControl to set
	 */
	public void setTextControl(Composite textControl) {
		this.textControl = textControl;
	}

	/**
	 * @return the validationReseted
	 */
	public boolean isValidationReseted() {
		return validationReseted;
	}

	/**
	 * @param validationReseted the validationReseted to set
	 */
	public void setValidationReseted(boolean validationReseted) {
		this.validationReseted = validationReseted;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.spi.custom.model.ECPHardcodedReferences#getNeededDomainModelReferences()
	 */
	@Override
	public Set<VDomainModelReference> getNeededDomainModelReferences() {
		return Collections.emptySet();
	}

	public List<VDomainModelReference> getResolvedReferences() {
		return getResolvedDomainModelReferences();
	}

	public SWTCustomControlHelper getStubSWTHelper() {
		return super.getHelper();
	}

	public Button getButton() {
		return button;
	}

}