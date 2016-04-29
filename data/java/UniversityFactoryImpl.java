/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource - Generated code
 */
package org.eclipse.emf.ecp.test.university.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecp.test.university.Address;
import org.eclipse.emf.ecp.test.university.Assistant;
import org.eclipse.emf.ecp.test.university.Course;
import org.eclipse.emf.ecp.test.university.CourseCatalog;
import org.eclipse.emf.ecp.test.university.Professor;
import org.eclipse.emf.ecp.test.university.Staff;
import org.eclipse.emf.ecp.test.university.UniversityFactory;
import org.eclipse.emf.ecp.test.university.UniversityPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 *
 * @generated
 */
public class UniversityFactoryImpl extends EFactoryImpl implements UniversityFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static UniversityFactory init() {
		try {
			final UniversityFactory theUniversityFactory = (UniversityFactory) EPackage.Registry.INSTANCE
				.getEFactory(UniversityPackage.eNS_URI);
			if (theUniversityFactory != null) {
				return theUniversityFactory;
			}
		} catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new UniversityFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public UniversityFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
		case UniversityPackage.COURSE_CATALOG:
			return createCourseCatalog();
		case UniversityPackage.COURSE:
			return createCourse();
		case UniversityPackage.STAFF:
			return createStaff();
		case UniversityPackage.PROFESSOR:
			return createProfessor();
		case UniversityPackage.ASSISTANT:
			return createAssistant();
		case UniversityPackage.ADDRESS:
			return createAddress();
		default:
			throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public CourseCatalog createCourseCatalog() {
		final CourseCatalogImpl courseCatalog = new CourseCatalogImpl();
		return courseCatalog;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Course createCourse() {
		final CourseImpl course = new CourseImpl();
		return course;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Staff createStaff() {
		final StaffImpl staff = new StaffImpl();
		return staff;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Professor createProfessor() {
		final ProfessorImpl professor = new ProfessorImpl();
		return professor;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Assistant createAssistant() {
		final AssistantImpl assistant = new AssistantImpl();
		return assistant;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public Address createAddress() {
		final AddressImpl address = new AddressImpl();
		return address;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public UniversityPackage getUniversityPackage() {
		return (UniversityPackage) getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static UniversityPackage getPackage() {
		return UniversityPackage.eINSTANCE;
	}

} // UniversityFactoryImpl
