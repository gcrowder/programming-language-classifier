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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Course Catalog</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.test.university.CourseCatalog#getCourses <em>Courses</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getCourseCatalog()
 * @model
 * @generated
 */
public interface CourseCatalog extends EObject {
	/**
	 * Returns the value of the '<em><b>Courses</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.test.university.Course}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Courses</em>' reference list isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Courses</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.test.university.UniversityPackage#getCourseCatalog_Courses()
	 * @model containment="true"
	 * @generated
	 */
	EList<Course> getCourses();

} // CourseCatalog
