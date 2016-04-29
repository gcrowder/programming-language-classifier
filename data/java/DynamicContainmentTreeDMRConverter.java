/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.dynamictree.model.impl;

import org.eclipse.core.databinding.observable.IObserving;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.databinding.IEMFListProperty;
import org.eclipse.emf.databinding.IEMFValueProperty;
import org.eclipse.emf.databinding.internal.EMFValuePropertyDecorator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.DomainModelReferenceConverterEMF;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

/**
 * A {@link DomainModelReferenceConverterEMF} that converts a {@link DynamicContainmentTreeDomainModelReference} to an
 * {@link IEMFListProperty IEMFListProperty} or an {@link IEMFValueProperty}.
 *
 * @author Lucas Koehler
 *
 */
@SuppressWarnings("restriction")
public class DynamicContainmentTreeDMRConverter implements DomainModelReferenceConverterEMF {
	private EMFFormsDatabindingEMF emfFormsDatabinding;
	private ServiceReference<EMFFormsDatabindingEMF> databindingServiceReference;

	/**
	 * Sets the {@link EMFFormsDatabindingEMF}.
	 *
	 * @param emfFormsDatabinding the emfFormsDatabinding to set
	 */
	void setEMFFormsDatabinding(EMFFormsDatabindingEMF emfFormsDatabinding) {
		this.emfFormsDatabinding = emfFormsDatabinding;
	}

	/**
	 * Unsets the {@link EMFFormsDatabindingEMF}.
	 */
	void unsetEMFFormsDatabinding() {
		emfFormsDatabinding = null;
	}

	/**
	 * This method is called by the OSGI framework when this {@link DomainModelReferenceConverterEMF} is activated. It
	 * retrieves the {@link EMFFormsDatabindingEMF EMF Forms databinding service}.
	 *
	 * @param bundleContext The {@link BundleContext} of this classes bundle.
	 */
	protected final void activate(BundleContext bundleContext) {
		databindingServiceReference = bundleContext.getServiceReference(EMFFormsDatabindingEMF.class);
		setEMFFormsDatabinding(bundleContext.getService(databindingServiceReference));

	}

	/**
	 * This method is called by the OSGI framework when this {@link DomainModelReferenceConverterEMF} is deactivated.
	 * It frees the {@link EMFFormsDatabindingEMF EMF Forms databinding service}.
	 *
	 * @param bundleContext The {@link BundleContext} of this classes bundle.
	 */
	protected final void deactivate(BundleContext bundleContext) {
		unsetEMFFormsDatabinding();
		bundleContext.ungetService(databindingServiceReference);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see DomainModelReferenceConverterEMF#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)
	 */
	@Override
	public double isApplicable(VDomainModelReference domainModelReference) {
		if (domainModelReference == null) {
			throw new IllegalArgumentException("The given VDomainModelReference must not be null."); //$NON-NLS-1$
		}
		if (domainModelReference instanceof DynamicContainmentTreeDomainModelReference) {
			return 10d;
		}
		return NOT_APPLICABLE;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see DomainModelReferenceConverterEMF#convertToValueProperty(VDomainModelReference,EObject)
	 */
	@Override
	public IEMFValueProperty convertToValueProperty(VDomainModelReference domainModelReference, EObject object)
		throws DatabindingFailedException {
		final DynamicContainmentTreeDomainModelReference dynamicContainmentTreeReference = getAndCheckDynamicContainmentTreeDMR(
			domainModelReference);

		final int index = getIndex(dynamicContainmentTreeReference);

		final EMFValuePropertyDecorator indexedProperty = getIndexedRootProperty(
			dynamicContainmentTreeReference, index, object);

		final IEMFValueProperty valuePropertyFromBase = emfFormsDatabinding
			.getValueProperty(dynamicContainmentTreeReference.getPathFromBase(), object);

		return indexedProperty.value(valuePropertyFromBase);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emfforms.spi.core.services.databinding.DomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)
	 */
	@Override
	public IEMFListProperty convertToListProperty(VDomainModelReference domainModelReference, EObject object)
		throws DatabindingFailedException {
		final DynamicContainmentTreeDomainModelReference dynamicContainmentTreeReference = getAndCheckDynamicContainmentTreeDMR(
			domainModelReference);

		final int index = getIndex(dynamicContainmentTreeReference);

		final EMFValuePropertyDecorator indexedProperty = getIndexedRootProperty(
			dynamicContainmentTreeReference, index, object);
		final IEMFListProperty listPropertyFromBase = emfFormsDatabinding
			.getListProperty(dynamicContainmentTreeReference.getPathFromBase(), object);

		return indexedProperty.list(listPropertyFromBase);
	}

	private EMFValuePropertyDecorator getIndexedRootProperty(
		final DynamicContainmentTreeDomainModelReference dynamicContainmentTreeReference, final int index,
		EObject object)
			throws DatabindingFailedException, IllegalListTypeException {
		final IValueProperty valuePropertyFromRoot = emfFormsDatabinding
			.getValueProperty(dynamicContainmentTreeReference.getPathFromRoot(), object);
		final EStructuralFeature structuralFeature = (EStructuralFeature) valuePropertyFromRoot.getValueType();
		checkListType(structuralFeature);

		final EMFIndexedValuePropertyDelegator indexedProperty = new EMFIndexedValuePropertyDelegator(
			valuePropertyFromRoot, index);
		return new EMFValuePropertyDecorator(indexedProperty, structuralFeature);
	}

	private DynamicContainmentTreeDomainModelReference getAndCheckDynamicContainmentTreeDMR(
		VDomainModelReference domainModelReference) throws DatabindingFailedException {
		if (domainModelReference == null) {
			throw new IllegalArgumentException("The given VDomainModelReference must not be null."); //$NON-NLS-1$
		}
		if (!DynamicContainmentTreeDomainModelReference.class.isInstance(domainModelReference)) {
			throw new IllegalArgumentException(
				"DomainModelReference must be an instance of DynamicContainmentTreeDomainModelReference."); //$NON-NLS-1$
		}

		final DynamicContainmentTreeDomainModelReference dynamicContainmentTreeReference = DynamicContainmentTreeDomainModelReference.class
			.cast(domainModelReference);
		if (dynamicContainmentTreeReference.getPathFromRoot() == null) {
			throw new DatabindingFailedException(
				"The field pathFromRoot of the given DynamicContainmentTreeDomainModelReference must not be null."); //$NON-NLS-1$
		}
		if (dynamicContainmentTreeReference.getPathFromBase() == null) {
			throw new DatabindingFailedException(
				"The field pathFromBase of the given DynamicContainmentTreeDomainModelReference must not be null."); //$NON-NLS-1$
		}
		return dynamicContainmentTreeReference;
	}

	private int getIndex(DynamicContainmentTreeDomainModelReference reference) throws DatabindingFailedException {
		EObject parent = reference.eContainer();
		while (!DynamicContainmentItem.class.isInstance(parent) && parent != null) {
			parent = parent.eContainer();
		}
		if (parent == null) {
			throw new DatabindingFailedException(
				"The base item index of the DynamicContainmentTreeDomainModelReference could not be resolved because its container is null."); //$NON-NLS-1$
		}
		final DynamicContainmentItem packingItem = (DynamicContainmentItem) parent;
		if (packingItem.getBaseItemIndex() == null) {
			throw new DatabindingFailedException(
				"The base item index of the DynamicContainmentTreeDomainModelReference must not be null."); //$NON-NLS-1$
		}
		return packingItem.getBaseItemIndex();
	}

	/**
	 * Checks whether the given structural feature references a proper list to generate a value or list property.
	 *
	 * @param structuralFeature The feature to check
	 * @throws IllegalListTypeException if the structural feature doesn't reference a proper list.
	 */
	private void checkListType(EStructuralFeature structuralFeature) throws IllegalListTypeException {
		if (!structuralFeature.isMany()) {
			throw new IllegalListTypeException(
				"The DynamicContainmentTreeDomainModelReference's base feature must reference a list."); //$NON-NLS-1$
		}
		if (!EReference.class.isInstance(structuralFeature)) {
			throw new IllegalListTypeException(
				"The DynamicContainmentTreeDomainModelReference's base feature must reference a list of EObjects."); //$NON-NLS-1$
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emfforms.spi.core.services.databinding.emf.DomainModelReferenceConverterEMF#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference,
	 *      org.eclipse.emf.ecore.EObject)
	 */
	@Override
	public Setting getSetting(VDomainModelReference domainModelReference, EObject object)
		throws DatabindingFailedException {
		final IEMFValueProperty valueProperty = convertToValueProperty(domainModelReference, object);
		final IObservableValue observableValue = valueProperty.observe(object);
		final EObject eObject = (EObject) IObserving.class.cast(observableValue).getObserved();
		if (eObject == null) {
			throw new DatabindingFailedException("The observed elements is empty. Probably the list is empty!");
		}
		final EStructuralFeature eStructuralFeature = valueProperty.getStructuralFeature();
		return InternalEObject.class.cast(eObject).eSetting(eStructuralFeature);
	}
}
