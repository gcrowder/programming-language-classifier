/**
 */
package car;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc --> The <b>Package</b> for the model. It contains
 * accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each operation of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * 
 * @see car.CarFactory
 * @model kind="package"
 * @generated
 */
public interface CarPackage extends EPackage {
	/**
	 * The package name. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNAME = "car";

	/**
	 * The package namespace URI. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_URI = "http://eclipsesource.com/example/model";

	/**
	 * The package namespace name. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	String eNS_PREFIX = "com.eclipsesource.example.model";

	/**
	 * The singleton instance of the package. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @generated
	 */
	CarPackage eINSTANCE = car.impl.CarPackageImpl.init();

	/**
	 * The meta object id for the '{@link car.impl.PersonImpl <em>Person</em>}'
	 * class. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see car.impl.PersonImpl
	 * @see car.impl.CarPackageImpl#getPerson()
	 * @generated
	 */
	int PERSON = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int PERSON__NAME = 0;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference
	 * list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int PERSON__CHILDREN = 1;

	/**
	 * The feature id for the '<em><b>Birth Place</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int PERSON__BIRTH_PLACE = 2;

	/**
	 * The number of structural features of the '<em>Person</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int PERSON_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Person</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int PERSON_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link car.impl.DriverImpl <em>Driver</em>}'
	 * class. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see car.impl.DriverImpl
	 * @see car.impl.CarPackageImpl#getDriver()
	 * @generated
	 */
	int DRIVER = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER__NAME = PERSON__NAME;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference
	 * list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER__CHILDREN = PERSON__CHILDREN;

	/**
	 * The feature id for the '<em><b>Birth Place</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER__BIRTH_PLACE = PERSON__BIRTH_PLACE;

	/**
	 * The feature id for the '<em><b>Age</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER__AGE = PERSON_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Driver</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER_FEATURE_COUNT = PERSON_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Driver</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int DRIVER_OPERATION_COUNT = PERSON_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link car.impl.CarImpl <em>Car</em>}' class.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see car.impl.CarImpl
	 * @see car.impl.CarPackageImpl#getCar()
	 * @generated
	 */
	int CAR = 2;

	/**
	 * The feature id for the '<em><b>Manufacturer</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR__MANUFACTURER = 0;

	/**
	 * The feature id for the '<em><b>License Plate</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR__LICENSE_PLATE = 1;

	/**
	 * The feature id for the '<em><b>Seat Count</b></em>' attribute. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR__SEAT_COUNT = 2;

	/**
	 * The feature id for the '<em><b>Driver</b></em>' containment reference.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR__DRIVER = 3;

	/**
	 * The number of structural features of the '<em>Car</em>' class. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>Car</em>' class. <!-- begin-user-doc
	 * --> <!-- end-user-doc -->
	 * 
	 * @generated
	 * @ordered
	 */
	int CAR_OPERATION_COUNT = 0;

	/**
	 * Returns the meta object for class '{@link car.Person <em>Person</em>}'.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>Person</em>'.
	 * @see car.Person
	 * @generated
	 */
	EClass getPerson();

	/**
	 * Returns the meta object for the attribute '{@link car.Person#getName
	 * <em>Name</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see car.Person#getName()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_Name();

	/**
	 * Returns the meta object for the containment reference list '
	 * {@link car.Person#getChildren <em>Children</em>}'. <!-- begin-user-doc
	 * --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the containment reference list '
	 *         <em>Children</em>'.
	 * @see car.Person#getChildren()
	 * @see #getPerson()
	 * @generated
	 */
	EReference getPerson_Children();

	/**
	 * Returns the meta object for the attribute '
	 * {@link car.Person#getBirthPlace <em>Birth Place</em>}'. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>Birth Place</em>'.
	 * @see car.Person#getBirthPlace()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_BirthPlace();

	/**
	 * Returns the meta object for class '{@link car.Driver <em>Driver</em>}'.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>Driver</em>'.
	 * @see car.Driver
	 * @generated
	 */
	EClass getDriver();

	/**
	 * Returns the meta object for the attribute '{@link car.Driver#getAge
	 * <em>Age</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>Age</em>'.
	 * @see car.Driver#getAge()
	 * @see #getDriver()
	 * @generated
	 */
	EAttribute getDriver_Age();

	/**
	 * Returns the meta object for class '{@link car.Car <em>Car</em>}'. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for class '<em>Car</em>'.
	 * @see car.Car
	 * @generated
	 */
	EClass getCar();

	/**
	 * Returns the meta object for the attribute '
	 * {@link car.Car#getManufacturer <em>Manufacturer</em>}'. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>Manufacturer</em>'.
	 * @see car.Car#getManufacturer()
	 * @see #getCar()
	 * @generated
	 */
	EAttribute getCar_Manufacturer();

	/**
	 * Returns the meta object for the attribute '
	 * {@link car.Car#getLicensePlate <em>License Plate</em>}'. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>License Plate</em>'.
	 * @see car.Car#getLicensePlate()
	 * @see #getCar()
	 * @generated
	 */
	EAttribute getCar_LicensePlate();

	/**
	 * Returns the meta object for the attribute '{@link car.Car#getSeatCount
	 * <em>Seat Count</em>}'. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the meta object for the attribute '<em>Seat Count</em>'.
	 * @see car.Car#getSeatCount()
	 * @see #getCar()
	 * @generated
	 */
	EAttribute getCar_SeatCount();

	/**
	 * Returns the meta object for the containment reference '
	 * {@link car.Car#getDriver <em>Driver</em>}'. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @return the meta object for the containment reference '<em>Driver</em>'.
	 * @see car.Car#getDriver()
	 * @see #getCar()
	 * @generated
	 */
	EReference getCar_Driver();

	/**
	 * Returns the factory that creates the instances of the model. <!--
	 * begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CarFactory getCarFactory();

	/**
	 * <!-- begin-user-doc --> Defines literals for the meta objects that
	 * represent
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
		 * The meta object literal for the '{@link car.impl.PersonImpl
		 * <em>Person</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc
		 * -->
		 * 
		 * @see car.impl.PersonImpl
		 * @see car.impl.CarPackageImpl#getPerson()
		 * @generated
		 */
		EClass PERSON = eINSTANCE.getPerson();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute
		 * feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute PERSON__NAME = eINSTANCE.getPerson_Name();

		/**
		 * The meta object literal for the '<em><b>Children</b></em>'
		 * containment reference list feature. <!-- begin-user-doc --> <!--
		 * end-user-doc -->
		 * 
		 * @generated
		 */
		EReference PERSON__CHILDREN = eINSTANCE.getPerson_Children();

		/**
		 * The meta object literal for the '<em><b>Birth Place</b></em>'
		 * attribute feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute PERSON__BIRTH_PLACE = eINSTANCE.getPerson_BirthPlace();

		/**
		 * The meta object literal for the '{@link car.impl.DriverImpl
		 * <em>Driver</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc
		 * -->
		 * 
		 * @see car.impl.DriverImpl
		 * @see car.impl.CarPackageImpl#getDriver()
		 * @generated
		 */
		EClass DRIVER = eINSTANCE.getDriver();

		/**
		 * The meta object literal for the '<em><b>Age</b></em>' attribute
		 * feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute DRIVER__AGE = eINSTANCE.getDriver_Age();

		/**
		 * The meta object literal for the '{@link car.impl.CarImpl
		 * <em>Car</em>}' class. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @see car.impl.CarImpl
		 * @see car.impl.CarPackageImpl#getCar()
		 * @generated
		 */
		EClass CAR = eINSTANCE.getCar();

		/**
		 * The meta object literal for the '<em><b>Manufacturer</b></em>'
		 * attribute feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute CAR__MANUFACTURER = eINSTANCE.getCar_Manufacturer();

		/**
		 * The meta object literal for the '<em><b>License Plate</b></em>'
		 * attribute feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute CAR__LICENSE_PLATE = eINSTANCE.getCar_LicensePlate();

		/**
		 * The meta object literal for the '<em><b>Seat Count</b></em>'
		 * attribute feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EAttribute CAR__SEAT_COUNT = eINSTANCE.getCar_SeatCount();

		/**
		 * The meta object literal for the '<em><b>Driver</b></em>' containment
		 * reference feature. <!-- begin-user-doc --> <!-- end-user-doc -->
		 * 
		 * @generated
		 */
		EReference CAR__DRIVER = eINSTANCE.getCar_Driver();

	}

} // CarPackage
