import random
import statistics as st
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats, integrate
import numpy as np


class Simulation:
    """ Class that simulates 30 cars at given:
    speeds, time (in seconds) and number of trials. """
    def __init__(self, speeds, time, trials, chance_of_slowdown):
        self.speeds = speeds
        self.time = time
        self.trials = trials
        self.chance_of_slowdown = chance_of_slowdown

    def make_cars(self, speed, chance_of_slowdown=.1):
        """ Make 30 car objects with given speed limit
        and give them evenly spaced locations across 1km stretch of road. """
        car_list = []
        for number in range(30):
            car_list.append(Vehicle((number * 33, (number * 33) + 4), max_speed=speed, chance_of_slowdown = self.chance_of_slowdown))
        return car_list

    def get_location(self, speed, time):
        """ Simulate 30 cars going at given speed
        for given number of cycles (time).
        Return list of all car locations. """
        car_list = self.make_cars(speed)

        total_car_location = []
        for number in range(time):
            car_locations = []
            for index, car in enumerate(car_list):
                car_locations.append(car.location)
                try:
                    car.move_car(car_list[index + 1])
                except IndexError:
                    car.move_car(car_list[0])
            total_car_location.append(car_locations)
        return total_car_location

    def get_all_speeds(self, speed, time):
        """ Simulate 30 cars going at given speed
        for given number of cycles (time).
        Return list of all car speeds and average speed. """
        car_list = self.make_cars(speed)

        total_car_speeds = []
        average_speeds = []
        for number in range(time):
            car_speeds = []
            for index, car in enumerate(car_list):
                car_speeds.append(car.speed)
                try:
                    car.move_car(car_list[index + 1])
                except IndexError:
                    car.move_car(car_list[0])
            total_car_speeds.append(car_speeds)
            average_speeds.append(st.mean(car_speeds))
        return total_car_speeds, round(st.mean(average_speeds), 2)

    def full_monte(self):
        """ Run number of trials given at object creation for each speed given.
        Return list of all speeds and a list containing lists of average speeds
        , by each speed limit."""
        speeds_list = []

        average_speeds_by_speed = []
        for speed in self.speeds:
            average_speeds_list = []
            for trial in range(self.trials):
                trial_speeds_list, trial_average_speed = self.get_all_speeds(speed, self.time)
                speeds_list.append(trial_speeds_list)
                average_speeds_list.append(trial_average_speed)
            average_speeds_by_speed.append(average_speeds_list)
        return speeds_list, average_speeds_by_speed

    def plot_traffic(self, total_car_location):
        """ Method to plot the location of cars in space and time. Takes list
        of total car locations. """
        plot_all_locations = []
        for locations in total_car_location:
            iteration_list = []
            for location in locations:
                car_range = list(range(location[0], location[1]+1))
                if len(car_range) == 0:
                    car_range = list(range(location[0], 7001)) + list(range(1, location[1] + 1))
                iteration_list += car_range
            plot_all_locations.append(iteration_list)
        n = 480
        for iteration in plot_all_locations:
            x = iteration
            y = [n] * len(iteration)
            plt.scatter(x, y)
            n -= 1
        plt.xlim(1, 7000)
        plt.ylim(0, 480)
        plt.show()




class Vehicle:
    """ Requirements:
    1 km road
    5m long cars
    120km/h max speed
    2m/s acceleration
    if car would collide, car will stop
    10 percent chance of slowing down 2m/s
    30 cars in simulation, evenly spaced at start.
    """
    def __init__(self, location=(0, 4), speed=0, max_speed=33, chance_of_slowdown=.10):
        self.location = location
        self.speed = speed
        self.acceleration = 2
        self.size = 5
        self.max_speed = max_speed
        self.desired_space = self.speed
        self.last_location = location
        self.chance_of_slowdown = chance_of_slowdown

    def move_car(self, next_car):
        """ Does all the steps necessary to move the car each iteration."""
        space = self.get_space_ahead(next_car)
        self.update_speed(space, next_car)
        self.random_slowdown(self.chance_of_slowdown)
        self.set_location(next_car)

    def get_space_ahead(self, next_car):
        """ Checks how much space is between current car's font bumper
        and next_car's back bumper."""
        if next_car.location[0] < self.location[1]:
            return (7000 - self.location[1]) + next_car.location[0]
        else:
            return next_car.location[0] - self.location[1]

    def set_location(self, next_car):
        """Changes the location of the car based on car's speed."""
        start = self.location[0] + self.speed
        end = self.location[1] + self.speed
        # (993, 998) --> (998, 1003)
        if start > 7000:
            start = start - 7000
        if end > 7000:
            end = end - 7000
        elif end > 1000 and end < 2000:
            self.chance_of_slowdown=.14
        elif end > 2000 and end < 3000:
            self.chance_of_slowdown=.1
        elif end > 3000 and end < 4000:
            self.chance_of_slowdown=.2
        elif end > 4000 and end < 5000:
            self.chance_of_slowdown=.1
        elif end > 5000 and end < 6000:
            self.chance_of_slowdown=.12
        elif end > 6000 and end < 7000:
            self.chance_of_slowdown=.1
        # (998, 1003) --> (998, 3)
        self.last_location = self.location
        self.location = (start, end)

    def update_speed(self, space, next_car):
        """Change the car's speed based on space ahead of car."""
        if space <= 2:
            self.speed = 0
        elif space >= self.speed and self.speed < self.max_speed:
            # if self.speed < self.max_speed:
            self.speed += self.acceleration
            # elif self.speed > self.max_speed:
            #     self.speed = self.max_speed
        else:
            self.speed = next_car.speed

    def random_slowdown(self, chance_of_slowdown):
        """Location dependent chance to slow the car by 2ms every iteration."""
        if random.random() < self.chance_of_slowdown:
            if self.speed > 2:
                self.speed -= 2

# [0, 0, 0, 1, 1, Car, 1, 1, 0, 0, 0, 0, 1, 1, Car, 1, 1, 0, 0]
# car_list = []
# for number in range(30):
#     car_list.append(Vehicle((number * 33, (number * 33) + 4)))
#
# total_car_location = []
# for number in range(120):
#     car_locations = []
#     for index, car in enumerate(car_list):
#         car_locations.append(car.location)
#         try:
#             car.move_car(car_list[index + 1])
#         except IndexError:
#             car.move_car(car_list[0])
#     total_car_location.append(car_locations)
#
# print(total_car_location)


# tron = Simulation([33], 480, 1, .1)
# # speeds_list, average_speeds_list = tron.full_monte()
# #
# # print("Average Speeds: ", average_speeds_list)
# tron.plot_traffic(tron.get_location(33, 480))



# tronss = Simulation(range(20,33), 120, 10, .1)
# speeds_list, average_speeds_list = tronss.full_monte()
# #
# avg_speeds_by_max_speed = []
# for i, speed in enumerate(average_speeds_list):
#     avg_speed = i + 20, st.mean(speed), st.stdev(speed)
#     print(avg_speed)
#     avg_speeds_by_max_speed.append(avg_speed)
#
#
# tron.plot_traffic(tron.get_location(25, 120))
