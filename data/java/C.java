/**
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 */
package org.eclipse.emfforms.core.services.databinding.testmodel.test.model;

import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>C</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getD <em>D</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getEClassToString
 * <em>EClass To String</em>}</li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getEClassToA <em>EClass To A</em>}
 * </li>
 * <li>{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getA <em>A</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getC()
 * @model
 * @generated
 */
public interface C extends EObject {
	/**
	 * Returns the value of the '<em><b>D</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>D</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>D</em>' containment reference.
	 * @see #setD(D)
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getC_D()
	 * @model containment="true"
	 * @generated
	 */
	D getD();

	/**
	 * Sets the value of the '{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getD
	 * <em>D</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>D</em>' containment reference.
	 * @see #getD()
	 * @generated
	 */
	void setD(D value);

	/**
	 * Returns the value of the '<em><b>EClass To String</b></em>' map.
	 * The key is of type {@link org.eclipse.emf.ecore.EClass},
	 * and the value is of type {@link java.lang.String},
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>EClass To String</em>' map isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>EClass To String</em>' map.
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getC_EClassToString()
	 * @model mapType=
	 *        "org.eclipse.emfforms.core.services.databinding.testmodel.test.model.EClassToEStringMap<org.eclipse.emf.ecore.EClass, org.eclipse.emf.ecore.EString>"
	 * @generated
	 */
	EMap<EClass, String> getEClassToString();

	/**
	 * Returns the value of the '<em><b>EClass To A</b></em>' map.
	 * The key is of type {@link org.eclipse.emf.ecore.EClass},
	 * and the value is of type {@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A},
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>EClass To A</em>' map isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>EClass To A</em>' map.
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getC_EClassToA()
	 * @model mapType=
	 *        "org.eclipse.emfforms.core.services.databinding.testmodel.test.model.EClassToAMap<org.eclipse.emf.ecore.EClass, org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A>"
	 * @generated
	 */
	EMap<EClass, A> getEClassToA();

	/**
	 * Returns the value of the '<em><b>A</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>A</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>A</em>' reference.
	 * @see #setA(A)
	 * @see org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage#getC_A()
	 * @model
	 * @generated
	 */
	A getA();

	/**
	 * Sets the value of the '{@link org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C#getA
	 * <em>A</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>A</em>' reference.
	 * @see #getA()
	 * @generated
	 */
	void setA(A value);

} // C
