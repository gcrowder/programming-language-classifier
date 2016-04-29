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
package org.eclipse.emf.ecp.test.university.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.ResourceLocator;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.EObjectValidator;
import org.eclipse.emf.ecp.test.university.Address;
import org.eclipse.emf.ecp.test.university.Assistant;
import org.eclipse.emf.ecp.test.university.Course;
import org.eclipse.emf.ecp.test.university.CourseCatalog;
import org.eclipse.emf.ecp.test.university.Person;
import org.eclipse.emf.ecp.test.university.Professor;
import org.eclipse.emf.ecp.test.university.Staff;
import org.eclipse.emf.ecp.test.university.UniversityPackage;
import org.eclipse.emf.ecp.view.internal.validation.ValidationNotification;

/**
 * <!-- begin-user-doc -->
 * The <b>Validator</b> for the model.
 * <!-- end-user-doc -->
 *
 * @see org.eclipse.emf.ecp.test.university.UniversityPackage
 * @generated
 */
public class UniversityValidator extends EObjectValidator {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public static final UniversityValidator INSTANCE = new UniversityValidator();

	/**
	 * A constant for the {@link org.eclipse.emf.common.util.Diagnostic#getSource() source} of diagnostic
	 * {@link org.eclipse.emf.common.util.Diagnostic#getCode() codes} from this package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @see org.eclipse.emf.common.util.Diagnostic#getSource()
	 * @see org.eclipse.emf.common.util.Diagnostic#getCode()
	 * @generated
	 */
	public static final String DIAGNOSTIC_SOURCE = "org.eclipse.emf.ecp.test.university"; //$NON-NLS-1$

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	private static final int GENERATED_DIAGNOSTIC_CODE_COUNT = 0;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants in a
	 * derived class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	protected static final int DIAGNOSTIC_CODE_COUNT = GENERATED_DIAGNOSTIC_CODE_COUNT;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public UniversityValidator() {
		super();
	}

	/**
	 * Returns the package of this validator switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected EPackage getEPackage() {
		return UniversityPackage.eINSTANCE;
	}

	/**
	 * Calls <code>validateXXX</code> for the corresponding classifier of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	protected boolean validate(int classifierID, Object value, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		switch (classifierID) {
		case UniversityPackage.COURSE_CATALOG:
			return validateCourseCatalog((CourseCatalog) value, diagnostics, context);
		case UniversityPackage.COURSE:
			return validateCourse((Course) value, diagnostics, context);
		case UniversityPackage.STAFF:
			return validateStaff((Staff) value, diagnostics, context);
		case UniversityPackage.PROFESSOR:
			return validateProfessor((Professor) value, diagnostics, context);
		case UniversityPackage.ASSISTANT:
			return validateAssistant((Assistant) value, diagnostics, context);
		case UniversityPackage.PERSON:
			return validatePerson((Person) value, diagnostics, context);
		case UniversityPackage.ADDRESS:
			return validateAddress((Address) value, diagnostics, context);
		default:
			return true;
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateCourseCatalog(CourseCatalog courseCatalog, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(courseCatalog, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateCourse(Course course, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!validate_NoCircularContainment(course, diagnostics, context)) {
			return false;
		}
		boolean result = validate_EveryMultiplicityConforms(course, diagnostics, context);
		if (result || diagnostics != null) {
			result &= validate_EveryDataValueConforms(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryReferenceIsContained(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryBidirectionalReferenceIsPaired(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryProxyResolves(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_UniqueID(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryKeyUnique(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validate_EveryMapEntryUnique(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateCourse_UniqueItemById(course, diagnostics, context);
		}
		if (result || diagnostics != null) {
			result &= validateCourse_NameNotEmpty(course, diagnostics, context);
		}
		return result;
	}

	/**
	 * Validates the UniqueItemById constraint of '<em>Course</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	// BEGIN COMPLEX CODE
	public boolean validateCourse_UniqueItemById(Course course, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		// method name is generated
		boolean validationResult = true;
		final CourseCatalog courseCatalog = (CourseCatalog) course.eContainer();
		if (courseCatalog == null) {
			return true;
		}

		final EList<Course> existingCourses = courseCatalog.getCourses();
		final Map<String, Set<Course>> contentMap = new LinkedHashMap<String, Set<Course>>();
		final List<Course> duplicates = new ArrayList<Course>();

		for (final Course courseToCheck : existingCourses) {
			final String id = courseToCheck.getId();
			if (!contentMap.containsKey(id)) {
				contentMap.put(id, new LinkedHashSet<Course>());
			}
			contentMap.get(id).add(courseToCheck);
		}

		for (final String id : contentMap.keySet()) {
			if (id != null) {
				if (contentMap.get(id).size() > 1 && id.equals(course.getId())) {
					duplicates.addAll(contentMap.get(id));
				}
			}
		}

		if (!duplicates.isEmpty()) {
			validationResult = false;
		}

		createDiagnostics(course, diagnostics, context, duplicates);

		for (final Course courseToNotify : existingCourses) {
			courseToNotify.eNotify(new ValidationNotification(courseToNotify));
		}

		return validationResult;
	}

	// END COMPLEX CODE

	/**
	 * @param course
	 * @param diagnostics
	 * @param context
	 * @param duplicates
	 */
	private void createDiagnostics(Course course, DiagnosticChain diagnostics, Map<Object, Object> context,
		final List<Course> duplicates) {
		for (final Course courseWithDuplicatedId : duplicates) {
			if (diagnostics != null) {
				if (courseWithDuplicatedId != course) {
					diagnostics
						.add(createDiagnostic(Diagnostic.WARNING,
							DIAGNOSTIC_SOURCE,
							0, "_UI_GenericConstraint_diagnostic", //$NON-NLS-1$
							new Object[] { "The id of a course must be unique" }, //$NON-NLS-1$
							new Object[] { courseWithDuplicatedId, UniversityPackage.eINSTANCE.getCourse_Id() },
							context));
				}
			}
		}
	}

	/**
	 * Validates the NameNotEmpty constraint of '<em>Course</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated NOT
	 */
	// BEGIN COMPLEX CODE
	public boolean validateCourse_NameNotEmpty(Course course, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		// method name is generated
		if (course.getName() != null && course.getName().length() < 1 || course.getId() != null
			&& course.getId().length() < 1) {
			if (diagnostics != null) {
				diagnostics.add(createDiagnostic(Diagnostic.ERROR,
					DIAGNOSTIC_SOURCE,
					0, "_UI_GenericConstraint_diagnostic", //$NON-NLS-1$
					new Object[] { "A course needs a name and an id." }, //$NON-NLS-1$
					new Object[] { course, UniversityPackage.eINSTANCE.getCourse_Name() },
					context));
			}
			return false;
		}
		return true;
	}

	// END COMPLEX CODE

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateStaff(Staff staff, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(staff, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateProfessor(Professor professor, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(professor, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateAssistant(Assistant assistant, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(assistant, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validatePerson(Person person, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(person, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	public boolean validateAddress(Address address, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(address, diagnostics, context);
	}

	/**
	 * Returns the resource locator that will be used to fetch messages for this validator's diagnostics.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 *
	 * @generated
	 */
	@Override
	public ResourceLocator getResourceLocator() {
		// TODO
		// Specialize this to return a resource locator for messages specific to this validator.
		// Ensure that you remove @generated or mark it @generated NOT
		return super.getResourceLocator();
	}

} // UniversityValidator
