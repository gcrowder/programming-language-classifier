/**
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 * Lucas Koehler - extension for EnumComboViewerRenderer_PTest
 */
package org.eclipse.emf.ecp.view.core.swt.test.model;

import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Simple Test Object</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getDate <em>Date</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getXmlDate <em>Xml Date</em>}</li>
 * <li>{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getMyEnum <em>My Enum</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestPackage#getSimpleTestObject()
 * @model
 * @generated
 */
public interface SimpleTestObject extends EObject {
	/**
	 * Returns the value of the '<em><b>Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Date</em>' attribute isn't clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Date</em>' attribute.
	 * @see #setDate(Date)
	 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestPackage#getSimpleTestObject_Date()
	 * @model
	 * @generated
	 */
	Date getDate();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getDate
	 * <em>Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Date</em>' attribute.
	 * @see #getDate()
	 * @generated
	 */
	void setDate(Date value);

	/**
	 * Returns the value of the '<em><b>Xml Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Xml Date</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Xml Date</em>' attribute.
	 * @see #setXmlDate(XMLGregorianCalendar)
	 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestPackage#getSimpleTestObject_XmlDate()
	 * @model dataType="org.eclipse.emf.ecp.view.core.swt.test.model.XMLDate"
	 * @generated
	 */
	XMLGregorianCalendar getXmlDate();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getXmlDate
	 * <em>Xml Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Xml Date</em>' attribute.
	 * @see #getXmlDate()
	 * @generated
	 */
	void setXmlDate(XMLGregorianCalendar value);

	/**
	 * Returns the value of the '<em><b>My Enum</b></em>' attribute.
	 * The literals are from the enumeration {@link org.eclipse.emf.ecp.view.core.swt.test.model.TestEnum}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>My Enum</em>' attribute isn't clear, there really should be more of a description
	 * here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>My Enum</em>' attribute.
	 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestEnum
	 * @see #setMyEnum(TestEnum)
	 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestPackage#getSimpleTestObject_MyEnum()
	 * @model dataType="org.eclipse.emf.ecp.view.core.swt.test.model.TestEnum"
	 * @generated
	 */
	TestEnum getMyEnum();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.core.swt.test.model.SimpleTestObject#getMyEnum
	 * <em>My Enum</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @param value the new value of the '<em>My Enum</em>' attribute.
	 * @see org.eclipse.emf.ecp.view.core.swt.test.model.TestEnum
	 * @see #getMyEnum()
	 * @generated
	 */
	void setMyEnum(TestEnum value);

} // SimpleTestObject
