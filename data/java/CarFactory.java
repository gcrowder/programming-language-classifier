/**
 */
package car;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc --> The <b>Factory</b> for the model. It provides a
 * create method for each non-abstract class of the model. <!-- end-user-doc -->
 * 
 * @see car.CarPackage
 * @generated
 */
public interface CarFactory extends EFactory {
	/**
	 * The singleton instance of the factory. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @generated
	 */
	CarFactory eINSTANCE = car.impl.CarFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Person</em>'. <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return a new object of class '<em>Person</em>'.
	 * @generated
	 */
	Person createPerson();

	/**
	 * Returns a new object of class '<em>Driver</em>'. <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return a new object of class '<em>Driver</em>'.
	 * @generated
	 */
	Driver createDriver();

	/**
	 * Returns a new object of class '<em>Car</em>'. <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return a new object of class '<em>Car</em>'.
	 * @generated
	 */
	Car createCar();

	/**
	 * Returns the package supported by this factory. <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * 
	 * @return the package supported by this factory.
	 * @generated
	 */
	CarPackage getCarPackage();

} // CarFactory
