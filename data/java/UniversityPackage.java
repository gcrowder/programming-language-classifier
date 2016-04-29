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
package org.eclipse.emf.ecp.test.university;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each operation of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.test.university.UniversityFactory
 * @model kind="package"
 * @generated
 */
public interface UniversityPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNAME = "university"; //$NON-NLS-1$

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_URI = "http://org/eclipse/emf/ecp/test/university"; //$NON-NLS-1$

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	String eNS_PREFIX = "org.eclipse.emf.ecp.test.university"; //$NON-NLS-1$

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	UniversityPackage eINSTANCE = org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.CourseCatalogImpl
	 * <em>Course Catalog</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.CourseCatalogImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getCourseCatalog()
	 * @generated
	 */
	int COURSE_CATALOG = 0;

	/**
	 * The feature id for the '<em><b>Courses</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE_CATALOG__COURSES = 0;

	/**
	 * The number of structural features of the '<em>Course Catalog</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE_CATALOG_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Course Catalog</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE_CATALOG_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.CourseImpl <em>Course</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.CourseImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getCourse()
	 * @generated
	 */
	int COURSE = 1;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE__ID = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE__NAME = 1;

	/**
	 * The feature id for the '<em><b>Etcs</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE__ETCS = 2;

	/**
	 * The number of structural features of the '<em>Course</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Course</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int COURSE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.StaffImpl <em>Staff</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.StaffImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getStaff()
	 * @generated
	 */
	int STAFF = 2;

	/**
	 * The feature id for the '<em><b>Professors</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int STAFF__PROFESSORS = 0;

	/**
	 * The feature id for the '<em><b>Assistants</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int STAFF__ASSISTANTS = 1;

	/**
	 * The feature id for the '<em><b>Staff</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int STAFF__STAFF = 2;

	/**
	 * The number of structural features of the '<em>Staff</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int STAFF_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Staff</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int STAFF_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.Person <em>Person</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.Person
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getPerson()
	 * @generated
	 */
	int PERSON = 5;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PERSON__NAME = 0;

	/**
	 * The feature id for the '<em><b>Addresses</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PERSON__ADDRESSES = 1;

	/**
	 * The number of structural features of the '<em>Person</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PERSON_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Person</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PERSON_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.ProfessorImpl <em>Professor</em>}'
	 * class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.ProfessorImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getProfessor()
	 * @generated
	 */
	int PROFESSOR = 3;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PROFESSOR__NAME = PERSON__NAME;

	/**
	 * The feature id for the '<em><b>Addresses</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PROFESSOR__ADDRESSES = PERSON__ADDRESSES;

	/**
	 * The number of structural features of the '<em>Professor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PROFESSOR_FEATURE_COUNT = PERSON_FEATURE_COUNT + 0;

	/**
	 * The number of operations of the '<em>Professor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int PROFESSOR_OPERATION_COUNT = PERSON_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.AssistantImpl <em>Assistant</em>}'
	 * class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.AssistantImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getAssistant()
	 * @generated
	 */
	int ASSISTANT = 4;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ASSISTANT__NAME = PERSON__NAME;

	/**
	 * The feature id for the '<em><b>Addresses</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ASSISTANT__ADDRESSES = PERSON__ADDRESSES;

	/**
	 * The number of structural features of the '<em>Assistant</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ASSISTANT_FEATURE_COUNT = PERSON_FEATURE_COUNT + 0;

	/**
	 * The number of operations of the '<em>Assistant</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ASSISTANT_OPERATION_COUNT = PERSON_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.ecp.test.university.impl.AddressImpl <em>Address</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.ecp.test.university.impl.AddressImpl
	 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getAddress()
	 * @generated
	 */
	int ADDRESS = 6;

	/**
	 * The number of structural features of the '<em>Address</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ADDRESS_FEATURE_COUNT = 0;

	/**
	 * The number of operations of the '<em>Address</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 * @ordered
	 */
	int ADDRESS_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.CourseCatalog
	 * <em>Course Catalog</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Course Catalog</em>'.
	 * @see org.eclipse.emf.ecp.test.university.CourseCatalog
	 * @generated
	 */
	EClass getCourseCatalog();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.test.university.CourseCatalog#getCourses <em>Courses</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Courses</em>'.
	 * @see org.eclipse.emf.ecp.test.university.CourseCatalog#getCourses()
	 * @see #getCourseCatalog()
	 * @generated
	 */
	EReference getCourseCatalog_Courses();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Course <em>Course</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Course</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Course
	 * @generated
	 */
	EClass getCourse();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.test.university.Course#getId <em>Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Id</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Course#getId()
	 * @see #getCourse()
	 * @generated
	 */
	EAttribute getCourse_Id();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.test.university.Course#getName
	 * <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Course#getName()
	 * @see #getCourse()
	 * @generated
	 */
	EAttribute getCourse_Name();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.test.university.Course#getEtcs
	 * <em>Etcs</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Etcs</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Course#getEtcs()
	 * @see #getCourse()
	 * @generated
	 */
	EAttribute getCourse_Etcs();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Staff <em>Staff</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Staff</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Staff
	 * @generated
	 */
	EClass getStaff();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.test.university.Staff#getProfessors <em>Professors</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Professors</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Staff#getProfessors()
	 * @see #getStaff()
	 * @generated
	 */
	EReference getStaff_Professors();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.test.university.Staff#getAssistants <em>Assistants</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Assistants</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Staff#getAssistants()
	 * @see #getStaff()
	 * @generated
	 */
	EReference getStaff_Assistants();

	/**
	 * Returns the meta object for the attribute list '{@link org.eclipse.emf.ecp.test.university.Staff#getStaff
	 * <em>Staff</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute list '<em>Staff</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Staff#getStaff()
	 * @see #getStaff()
	 * @generated
	 */
	EAttribute getStaff_Staff();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Professor <em>Professor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Professor</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Professor
	 * @generated
	 */
	EClass getProfessor();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Assistant <em>Assistant</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Assistant</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Assistant
	 * @generated
	 */
	EClass getAssistant();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Person <em>Person</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Person</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Person
	 * @generated
	 */
	EClass getPerson();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.emf.ecp.test.university.Person#getName
	 * <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Person#getName()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_Name();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link org.eclipse.emf.ecp.test.university.Person#getAddresses <em>Addresses</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for the containment reference list '<em>Addresses</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Person#getAddresses()
	 * @see #getPerson()
	 * @generated
	 */
	EReference getPerson_Addresses();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.ecp.test.university.Address <em>Address</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the meta object for class '<em>Address</em>'.
	 * @see org.eclipse.emf.ecp.test.university.Address
	 * @generated
	 */
	EClass getAddress();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	UniversityFactory getUniversityFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each operation of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.CourseCatalogImpl
		 * <em>Course Catalog</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.CourseCatalogImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getCourseCatalog()
		 * @generated
		 */
		EClass COURSE_CATALOG = eINSTANCE.getCourseCatalog();

		/**
		 * The meta object literal for the '<em><b>Courses</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference COURSE_CATALOG__COURSES = eINSTANCE.getCourseCatalog_Courses();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.CourseImpl <em>Course</em>}'
		 * class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.CourseImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getCourse()
		 * @generated
		 */
		EClass COURSE = eINSTANCE.getCourse();

		/**
		 * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute COURSE__ID = eINSTANCE.getCourse_Id();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute COURSE__NAME = eINSTANCE.getCourse_Name();

		/**
		 * The meta object literal for the '<em><b>Etcs</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute COURSE__ETCS = eINSTANCE.getCourse_Etcs();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.StaffImpl <em>Staff</em>}'
		 * class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.StaffImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getStaff()
		 * @generated
		 */
		EClass STAFF = eINSTANCE.getStaff();

		/**
		 * The meta object literal for the '<em><b>Professors</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference STAFF__PROFESSORS = eINSTANCE.getStaff_Professors();

		/**
		 * The meta object literal for the '<em><b>Assistants</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference STAFF__ASSISTANTS = eINSTANCE.getStaff_Assistants();

		/**
		 * The meta object literal for the '<em><b>Staff</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute STAFF__STAFF = eINSTANCE.getStaff_Staff();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.ProfessorImpl
		 * <em>Professor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.ProfessorImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getProfessor()
		 * @generated
		 */
		EClass PROFESSOR = eINSTANCE.getProfessor();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.AssistantImpl
		 * <em>Assistant</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.AssistantImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getAssistant()
		 * @generated
		 */
		EClass ASSISTANT = eINSTANCE.getAssistant();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.Person <em>Person</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.Person
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getPerson()
		 * @generated
		 */
		EClass PERSON = eINSTANCE.getPerson();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EAttribute PERSON__NAME = eINSTANCE.getPerson_Name();

		/**
		 * The meta object literal for the '<em><b>Addresses</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @generated
		 */
		EReference PERSON__ADDRESSES = eINSTANCE.getPerson_Addresses();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.ecp.test.university.impl.AddressImpl <em>Address</em>
		 * }' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 *
		 * @see org.eclipse.emf.ecp.test.university.impl.AddressImpl
		 * @see org.eclipse.emf.ecp.test.university.impl.UniversityPackageImpl#getAddress()
		 * @generated
		 */
		EClass ADDRESS = eINSTANCE.getAddress();

	}

} // UniversityPackage
