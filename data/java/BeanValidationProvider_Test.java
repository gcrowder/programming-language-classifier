/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 * 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.bean.test;

import static org.junit.Assert.assertEquals;

import java.io.InputStream;
import java.util.List;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.view.internal.validation.bean.DefaultValidationProvider;
import org.eclipse.emf.ecp.view.validation.bean.BeanValidationProvider;
import org.junit.Test;

import car.CarFactory;
import car.CarPackage;

public class BeanValidationProvider_Test {

	@Test
	public void testAttributeOfRootValidation() {
		car.Driver driverEMF = CarFactory.eINSTANCE.createDriver();
		driverEMF.setName("John Doe");
		driverEMF.setAge(17);
		driverEMF.setBirthPlace("NY");
		InputStream is = getClass().getResourceAsStream("/violations.xml");
		BeanValidationProvider bvp = new DefaultValidationProvider(is);
		List<Diagnostic> validate = bvp.validate(driverEMF);
		assertEquals(1, validate.size());
		assertEquals(driverEMF, validate.get(0).getData().get(0));
		assertEquals(CarPackage.eINSTANCE.getDriver_Age(), validate.get(0)
				.getData().get(1));
	}

	@Test
	public void testAttributeOfChildValidation() {
		car.Driver driverEMF = CarFactory.eINSTANCE.createDriver();
		driverEMF.setName("John Doe");
		driverEMF.setAge(17);
		driverEMF.setBirthPlace("NY");
		car.Car carEMF = CarFactory.eINSTANCE.createCar();
		carEMF.setManufacturer("SuperCar");
		carEMF.setSeatCount(2);
		carEMF.setLicensePlate("MM213");
		carEMF.setDriver(driverEMF);

		// car.Person person1EMF = CarFactory.eINSTANCE.createPerson();
		// person1EMF.setName("Bob der Baumeister");
		// person1EMF.setBirthPlace("Ort1");
		// driverEMF.getChildren().add(person1EMF);
		// car.Person person2EMF = CarFactory.eINSTANCE.createPerson();
		// person2EMF.setName("Jane Doe");
		// person2EMF.setBirthPlace("LA");
		// driverEMF.getChildren().add(person2EMF);
		// car.Person person3EMF = CarFactory.eINSTANCE.createPerson();
		// person3EMF.setName("Bernd das Brot");
		// person3EMF.setBirthPlace("Ort2");
		// driverEMF.getChildren().add(person3EMF);

		InputStream is = getClass().getResourceAsStream("/violations.xml");
		BeanValidationProvider bvp = new DefaultValidationProvider(is);
		List<Diagnostic> validate = bvp.validate(carEMF);
		assertEquals(1, validate.size());
		assertEquals(driverEMF, validate.get(0).getData().get(0));
		assertEquals(CarPackage.eINSTANCE.getDriver_Age(), validate.get(0)
				.getData().get(1));
	}
}
