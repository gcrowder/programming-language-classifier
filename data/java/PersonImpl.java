/**
 */
package car.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import car.CarPackage;
import car.Person;

/**
 * <!-- begin-user-doc --> An implementation of the model object '
 * <em><b>Person</b></em>'. <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link car.impl.PersonImpl#getName <em>Name</em>}</li>
 * <li>{@link car.impl.PersonImpl#getChildren <em>Children</em>}</li>
 * <li>{@link car.impl.PersonImpl#getBirthPlace <em>Birth Place</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PersonImpl extends MinimalEObjectImpl.Container implements Person {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getChildren() <em>Children</em>}'
	 * containment reference list. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getChildren()
	 * @generated
	 * @ordered
	 */
	protected EList<Person> children;

	/**
	 * The default value of the '{@link #getBirthPlace() <em>Birth Place</em>}'
	 * attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getBirthPlace()
	 * @generated
	 * @ordered
	 */
	protected static final String BIRTH_PLACE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getBirthPlace() <em>Birth Place</em>}'
	 * attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getBirthPlace()
	 * @generated
	 * @ordered
	 */
	protected String birthPlace = BIRTH_PLACE_EDEFAULT;

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected PersonImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CarPackage.Literals.PERSON;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		String oldName = name;
		name = newName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.PERSON__NAME, oldName, name));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public EList<Person> getChildren() {
		if (children == null) {
			children = new EObjectContainmentEList<Person>(Person.class, this,
					CarPackage.PERSON__CHILDREN);
		}
		return children;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String getBirthPlace() {
		return birthPlace;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setBirthPlace(String newBirthPlace) {
		String oldBirthPlace = birthPlace;
		birthPlace = newBirthPlace;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.PERSON__BIRTH_PLACE, oldBirthPlace, birthPlace));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case CarPackage.PERSON__CHILDREN:
			return ((InternalEList<?>) getChildren()).basicRemove(otherEnd,
					msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case CarPackage.PERSON__NAME:
			return getName();
		case CarPackage.PERSON__CHILDREN:
			return getChildren();
		case CarPackage.PERSON__BIRTH_PLACE:
			return getBirthPlace();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case CarPackage.PERSON__NAME:
			setName((String) newValue);
			return;
		case CarPackage.PERSON__CHILDREN:
			getChildren().clear();
			getChildren().addAll((Collection<? extends Person>) newValue);
			return;
		case CarPackage.PERSON__BIRTH_PLACE:
			setBirthPlace((String) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case CarPackage.PERSON__NAME:
			setName(NAME_EDEFAULT);
			return;
		case CarPackage.PERSON__CHILDREN:
			getChildren().clear();
			return;
		case CarPackage.PERSON__BIRTH_PLACE:
			setBirthPlace(BIRTH_PLACE_EDEFAULT);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case CarPackage.PERSON__NAME:
			return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT
					.equals(name);
		case CarPackage.PERSON__CHILDREN:
			return children != null && !children.isEmpty();
		case CarPackage.PERSON__BIRTH_PLACE:
			return BIRTH_PLACE_EDEFAULT == null ? birthPlace != null
					: !BIRTH_PLACE_EDEFAULT.equals(birthPlace);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", birthPlace: ");
		result.append(birthPlace);
		result.append(')');
		return result.toString();
	}

} // PersonImpl
