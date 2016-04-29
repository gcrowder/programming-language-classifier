import csv
import operator
from datetime import datetime
from movie_lib import Movie, User, Rating
import math
import argparse
import sys

# def get_user_id():
#     parser = argparse.ArgumentParser(description="Show top and recommended movies for a given user. Show top movies if no user given.")
#     parser.add_argument("user_id", help="user id of user in dataset",
#                         type=int, default=0)
#     args = parser.parse_args()
#     if args.user_id < 0 or args.user_id > 943:
#         print("That is not a valid user id. Please enter an id from 1-943.")
#         sys.exit(1)
#     return args.user_id

def format_time(date):
    try:
        return datetime.strptime(date, '%d-%b-%Y')
    except ValueError:
        return datetime(2000, 1, 1)

def get_movies():
    movies = {}
    with open('movies', encoding='latin_1') as f:
        reader = csv.DictReader(f, fieldnames=['movie_id', 'movie_title', 'date', 'something_else'], delimiter='|')
        for row in reader:
            #print("Movie ID: ", row['movie_id'], "Release Date: ", row['date'])
            movies.update({int(row['movie_id']) : Movie(int(row['movie_id']), row['movie_title'], format_time(row['date']))})
    return movies

def get_users():
    users = {}
    with open ('data') as f:
        reader = csv.DictReader(f, fieldnames=['user_id', 'movie_id', 'rating', 'something_else'], delimiter='\t')
        for row in reader:
            user_id = int(row['user_id'])
            movie_id = int(row['movie_id'])
            rating = int(row['rating'])
            if user_id in users:
                users[user_id].add_rating(user_id, movie_id, rating)
            else:
                users.update({user_id: User(user_id, movie_id, rating)})
    return users

def make_movie_ratings(movies, users):
    for movie_id, movie in movies.items():
        for user_id, user in users.items():
            if movie_id in user.ratings:
                movies[movie_id].add_rating(movie_id, user_id, user.ratings[movie_id])
    return movies

def prepare_ratings():
    movies = get_movies()
    users = get_users()
    movies = make_movie_ratings(movies, users)
    all_ratings = Rating(movies, users)
    return all_ratings

def most_popular_movies(ratings_object):
    averaged_movie_ratings = []
    for movie_id in ratings_object.movies.keys():
        if len(ratings_object.movies[movie_id].ratings) > 9:
            averaged_movie_ratings.append((movie_id, ratings_object.average_rating(movie_id)))
    descending_averaged_movie_ratings = sorted(averaged_movie_ratings, key=operator.itemgetter(1), reverse=True)
    return descending_averaged_movie_ratings

def show_movies(ratings_object, top_movies):
    count = 0
    for movie_id, rating in top_movies:
        count += 1
        if count < 21:
            print("{}. {}: {}".format(count, ratings_object.movies[movie_id].movie_title, round(rating, 2)))

def top_rated_unseen_movies(ratings_object, user_id):
    averaged_movie_ratings = []
    for movie_id in ratings_object.movies.keys():
        if len(ratings_object.movies[movie_id].ratings) > 9:
            if user_id not in ratings_object.movies[movie_id].ratings:
                averaged_movie_ratings.append((movie_id, ratings_object.average_rating(movie_id)))
    descending_averaged_movie_ratings = sorted(averaged_movie_ratings, key=operator.itemgetter(1), reverse=True)
    return descending_averaged_movie_ratings

def make_lists_for_euclid_distance(ratings_object, user_one, user_two):
    user_one_reviews = ratings_object.users[user_one].ratings.copy()
    user_two_reviews = ratings_object.users[user_two].ratings.copy()

    intersect_user_one = []
    intersect_user_two = []
    movies = []

    for movie, rating in user_one_reviews.items():
        if movie in user_two_reviews:
            intersect_user_one.append(rating)
            intersect_user_two.append(user_two_reviews[movie])
            movies.append(movie)

    return intersect_user_one, intersect_user_two

def euclidean_distance(v, w):
    """Given two lists, give the Euclidean distance between them on a scale
    of 0 to 1. 1 means the two lists are identical.
    """

    # Guard against empty lists.
    if len(v) is 0:
        return 0

    # Note that this is the same as vector subtraction.
    differences = [v[idx] - w[idx] for idx in range(len(v))]
    squares = [diff ** 2 for diff in differences]
    sum_of_squares = sum(squares)

    return 1 / (1 + math.sqrt(sum_of_squares))

def find_close_users(ratings_object, given_user):
    closeness = []
    for user in ratings_object.users:
        list_one, list_two = make_lists_for_euclid_distance(ratings_object, given_user, user)
        difference = euclidean_distance(list_one, list_two)
        closeness.append((user, difference))

    closest_users = sorted(closeness, key=operator.itemgetter(1), reverse=True)
    return closest_users[:10]

def find_top_movies_from_closest_users(ratings_object, given_user, closest_users):
    close_movie_ratings = {}
    for user, distance in closest_users:
        for movie_id, rating in ratings_object.users[user].ratings.items():
            if given_user not in ratings_object.movies[movie_id].ratings:
                if movie_id in close_movie_ratings:
                    close_movie_ratings[movie_id].append(rating * distance)
                else:
                    close_movie_ratings.update({movie_id : [rating * distance]})
    averaged_movie_ratings = []
    for movie_id, ratings in close_movie_ratings.items():
        if len(ratings) > 2:
            averaged_movie_ratings.append((movie_id, (sum(ratings) / len(ratings))))
    descending_averaged_movie_ratings = sorted(averaged_movie_ratings, key=operator.itemgetter(1), reverse=True)
    return descending_averaged_movie_ratings

def main():
    parser = argparse.ArgumentParser(description="Show top and recommended movies for a given user. Show top movies if no user given.")
    parser.add_argument("user_id", nargs='?', help="user id of user in dataset",
                        type=int, default=0)
    args = parser.parse_args()
    if args.user_id < 0 or args.user_id > 943:
        print("That is not a valid user id. Please enter an id from 1-943.")
        sys.exit(1)
    else:
        user_id = args.user_id

    movie_user_ratings = prepare_ratings()
    pop_movies = most_popular_movies(movie_user_ratings)
    print("Top 20 Films Overall: ")
    show_movies(movie_user_ratings, pop_movies)
    if user_id == 0:
        sys.exit(0)
    recommended_movies = top_rated_unseen_movies(movie_user_ratings, user_id)
    print("\n\n")
    print("Top 20 Films Recommended for User: {}".format(user_id))
    show_movies(movie_user_ratings, recommended_movies)
    ten_closest_users = find_close_users(movie_user_ratings, user_id)
    top_close_movies = find_top_movies_from_closest_users(movie_user_ratings, user_id, ten_closest_users)
    print("\n\n")
    print("Top 20 Films Recommended for User {} by the 10 Closest Users: ".format(user_id))
    show_movies(movie_user_ratings, top_close_movies)


if __name__ == '__main__':
    main()
