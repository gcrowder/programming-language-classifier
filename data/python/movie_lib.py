class Movie:
    def __init__(self, movie_id, movie_title, release_date):
        self.movie_id = movie_id
        self.movie_title = movie_title
        self.release_date = release_date
        self.ratings = {}

    def __str__(self):
        return "Movie ID: {} Movie Title {}".format(self.movie_id, self.movie_title)

    def add_rating(self, movie_id, user_id, rating):
        if self.movie_id == movie_id:
            self.ratings.update({user_id: rating})

class User:
    def __init__(self, user_id, movie_id, rating):
        self.user_id = user_id
        self.ratings = {movie_id : rating}
    def __str__(self):
        return "User ID: {} Number of ratings: {}".format(self.user_id, len(self.ratings))

    def add_rating(self, user_id, movie_id, rating):
        if self.user_id == user_id:
            self.ratings.update({movie_id : rating})

class Rating:
    def __init__(self, movies, users):
        self.movies = movies #{movie_id : movie_object}
        self.users = users #{user_id : user object}

    def average_rating(self, movie_id):
        movie = self.movies[movie_id]
        return (sum(movie.ratings.values())) / len(movie.ratings)

    def users_ratings(self, user_id):
        user = self.users[user_id]
        return user.ratings

    def movies_ratings(self, movie_id):
        movie = self.movies[movie_id]
        return movie.ratings

    def get_movie_title(self, movie_id):
        movie = self.movies[movie_id]
        return movie.movie_title
