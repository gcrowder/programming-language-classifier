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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EValidator;
import org.eclipse.emf.ecore.impl.EPackageImpl;
import org.eclipse.emf.ecp.test.university.Address;
import org.eclipse.emf.ecp.test.university.Assistant;
import org.eclipse.emf.ecp.test.university.Course;
import org.eclipse.emf.ecp.test.university.CourseCatalog;
import org.eclipse.emf.ecp.test.university.Person;
import org.eclipse.emf.ecp.test.university.Professor;
import org.eclipse.emf.ecp.test.university.Staff;
import org.eclipse.emf.ecp.test.university.UniversityFactory;
import org.eclipse.emf.ecp.test.university.UniversityPackage;
import org.eclipse.emf.ecp.test.university.util.UniversityValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 *
 * @generated
 */
public class UniversityPackageImpl extends EPackageImpl implements UniversityPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass courseCatalogEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass courseEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass staffEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass professorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass assistantEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass personEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private EClass addressEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package
	 * package URI value.
	 * <p>
	 * Note: the correct way to create the package is via the static factory method {@link #init init()}, which also
	 * performs initialization of the package, or returns the registered package, if one already exists. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private UniversityPackageImpl() {
		super(eNS_URI, UniversityFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>
	 * This method is used to initialize {@link UniversityPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc
	 * --> <!-- end-user-doc -->
	 *
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static UniversityPackage init() {
		if (isInited) {
			return (UniversityPackage) EPackage.Registry.INSTANCE.getEPackage(UniversityPackage.eNS_URI);
		}

		// Obtain or create and register package
		final UniversityPackageImpl theUniversityPackage = (UniversityPackageImpl) (EPackage.Registry.INSTANCE
			.get(eNS_URI) instanceof UniversityPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI)
				: new UniversityPackageImpl());

		isInited = true;

		// Create package meta-data objects
		theUniversityPackage.createPackageContents();

		// Initialize created meta-data
		theUniversityPackage.initializePackageContents();

		// Register package validator
		EValidator.Registry.INSTANCE.put(theUniversityPackage,
			new EValidator.Descriptor() {
				public EValidator getEValidator() {
					return UniversityValidator.INSTANCE;
				}
			});

		// Mark meta-data to indicate it can't be changed
		theUniversityPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(UniversityPackage.eNS_URI, theUniversityPackage);
		return theUniversityPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getCourseCatalog() {
		return courseCatalogEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EReference getCourseCatalog_Courses() {
		return (EReference) courseCatalogEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getCourse() {
		return courseEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EAttribute getCourse_Id() {
		return (EAttribute) courseEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EAttribute getCourse_Name() {
		return (EAttribute) courseEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EAttribute getCourse_Etcs() {
		return (EAttribute) courseEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getStaff() {
		return staffEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EReference getStaff_Professors() {
		return (EReference) staffEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EReference getStaff_Assistants() {
		return (EReference) staffEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EAttribute getStaff_Staff() {
		return (EAttribute) staffEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getProfessor() {
		return professorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getAssistant() {
		return assistantEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getPerson() {
		return personEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EAttribute getPerson_Name() {
		return (EAttribute) personEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EReference getPerson_Addresses() {
		return (EReference) personEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public EClass getAddress() {
		return addressEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public UniversityFactory getUniversityFactory() {
		return (UniversityFactory) getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package. This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) {
			return;
		}
		isCreated = true;

		// Create classes and their features
		courseCatalogEClass = createEClass(COURSE_CATALOG);
		createEReference(courseCatalogEClass, COURSE_CATALOG__COURSES);

		courseEClass = createEClass(COURSE);
		createEAttribute(courseEClass, COURSE__ID);
		createEAttribute(courseEClass, COURSE__NAME);
		createEAttribute(courseEClass, COURSE__ETCS);

		staffEClass = createEClass(STAFF);
		createEReference(staffEClass, STAFF__PROFESSORS);
		createEReference(staffEClass, STAFF__ASSISTANTS);
		createEAttribute(staffEClass, STAFF__STAFF);

		professorEClass = createEClass(PROFESSOR);

		assistantEClass = createEClass(ASSISTANT);

		personEClass = createEClass(PERSON);
		createEAttribute(personEClass, PERSON__NAME);
		createEReference(personEClass, PERSON__ADDRESSES);

		addressEClass = createEClass(ADDRESS);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model. This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) {
			return;
		}
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		professorEClass.getESuperTypes().add(getPerson());
		assistantEClass.getESuperTypes().add(getPerson());

		// Initialize classes, features, and operations; add parameters
		initEClass(courseCatalogEClass, CourseCatalog.class, "CourseCatalog", !IS_ABSTRACT, !IS_INTERFACE, //$NON-NLS-1$
			IS_GENERATED_INSTANCE_CLASS);
		initEReference(getCourseCatalog_Courses(), getCourse(), null, "courses", null, 0, -1, CourseCatalog.class, //$NON-NLS-1$
			!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			!IS_DERIVED, IS_ORDERED);

		initEClass(courseEClass, Course.class, "Course", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEAttribute(getCourse_Id(), ecorePackage.getEString(), "id", null, 1, 1, Course.class, !IS_TRANSIENT, //$NON-NLS-1$
			!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getCourse_Name(), ecorePackage.getEString(), "name", null, 1, 1, Course.class, !IS_TRANSIENT, //$NON-NLS-1$
			!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getCourse_Etcs(), ecorePackage.getEInt(), "etcs", null, 1, 1, Course.class, !IS_TRANSIENT, //$NON-NLS-1$
			!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(staffEClass, Staff.class, "Staff", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEReference(getStaff_Professors(), getProfessor(), null, "professors", null, 0, -1, Staff.class, //$NON-NLS-1$
			IS_TRANSIENT, IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			IS_DERIVED, IS_ORDERED);
		initEReference(getStaff_Assistants(), getAssistant(), null, "assistants", null, 0, -1, Staff.class, //$NON-NLS-1$
			IS_TRANSIENT, IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			IS_DERIVED, IS_ORDERED);
		initEAttribute(getStaff_Staff(), ecorePackage.getEFeatureMapEntry(), "staff", null, 0, -1, Staff.class, //$NON-NLS-1$
			!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(professorEClass, Professor.class, "Professor", !IS_ABSTRACT, !IS_INTERFACE, //$NON-NLS-1$
			IS_GENERATED_INSTANCE_CLASS);

		initEClass(assistantEClass, Assistant.class, "Assistant", !IS_ABSTRACT, !IS_INTERFACE, //$NON-NLS-1$
			IS_GENERATED_INSTANCE_CLASS);

		initEClass(personEClass, Person.class, "Person", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$
		initEAttribute(getPerson_Name(), ecorePackage.getEString(), "name", null, 0, 1, Person.class, !IS_TRANSIENT, //$NON-NLS-1$
			!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getPerson_Addresses(), getAddress(), null, "addresses", null, 0, -1, Person.class, //$NON-NLS-1$
			!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
			!IS_DERIVED, IS_ORDERED);

		initEClass(addressEClass, Address.class, "Address", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS); //$NON-NLS-1$

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http://www.eclipse.org/emf/2002/Ecore
		createEcoreAnnotations();
		// http:///org/eclipse/emf/ecore/util/ExtendedMetaData
		createExtendedMetaDataAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http://www.eclipse.org/emf/2002/Ecore</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected void createEcoreAnnotations() {
		final String source = "http://www.eclipse.org/emf/2002/Ecore"; //$NON-NLS-1$
		addAnnotation(courseEClass,
			source,
			new String[] {
				"constraints", "UniqueItemById NameNotEmpty" //$NON-NLS-1$ //$NON-NLS-2$
		});
	}

	/**
	 * Initializes the annotations for <b>http:///org/eclipse/emf/ecore/util/ExtendedMetaData</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected void createExtendedMetaDataAnnotations() {
		final String source = "http:///org/eclipse/emf/ecore/util/ExtendedMetaData"; //$NON-NLS-1$
		addAnnotation(getStaff_Professors(),
			source,
			new String[] {
				"group", "#staff" //$NON-NLS-1$ //$NON-NLS-2$
		});
		addAnnotation(getStaff_Assistants(),
			source,
			new String[] {
				"group", "#staff" //$NON-NLS-1$ //$NON-NLS-2$
		});
		addAnnotation(getStaff_Staff(),
			source,
			new String[] {
				"kind", "group" //$NON-NLS-1$ //$NON-NLS-2$
		});
	}

} // UniversityPackageImpl
