import json
import sys
import logging
import csv
import codecs
import numpy as np

'''
Take filename as argument. Returns a list of dictionary objects.
'''
def load_and_decode(fi_name):
    obj = []
    try:
        with open(fi_name) as curr_fi:
            for line in curr_fi:
                obj.append(json.loads(line))
    except IOError as e:
        print 'Could not load file {0}: {1}'.format(fi_name, e.strerror)
    except:
        print 'Unhandled exception'
        raise
    return obj

'''
takes list of business dictionary objects and city name. Returns businesses in city
'''
def filter_business_by_city(business_list, city_name):
    obj = []
    for business in business_list:
        try:
            if business['city'] == unicode(city_name):
                obj.append(business)
        except:
            print 'Other error occurred'
            raise
    return obj

'''
takes list of business dictionary objects and category.
returns businesses with certain category
'''
def filter_business_by_category(business_list, category_name):
    obj = []
    for business in business_list:
        if unicode(category_name) in business['categories']:
            obj.append(business)
    return obj

'''
returns dictionary of businesses
'''
def load_business():
    return load_and_decode('yelp_academic_dataset_business.json')

'''
returns dictionary of users
'''
def load_users():
    return load_and_decode('yelp_academic_dataset_user.json')

'''
returns dictionary of all reviews. DON'T CALL UNLESS YOU HAVE A LOT OF MEMORY!
'''
def load_reviews():
    return load_and_decode('yelp_academic_dataset_review.json')

'''
returns dictionary of reviews for restaurants in Charlotte
'''
def load_reviews_charlotte_restaurants():
    return load_and_decode('yelp_academic_dataset_review_charlotte_restaurants.json')

'''
takes a business id and filters entire review dataset by business id
'''
def filter_reviews_by_restaurant(business_id, output_array):
    with open('yelp_academic_dataset_review.json') as review_data:
        for review in review_data:
            if json.loads(review)['business_id'] == business_id:
                output_array.append(json.loads(review))
    return output_array

'''
takes a list of business ids from a dictionary of businesses
'''
def get_business_ids(business_list):
    id_list = []
    for item in business_list:
        id_list.append(item['business_id'])
    return id_list

'''
gets unique users from a list of dictionary review objects
'''
def get_users_from_reviews(review_list):
    user_list = []
    for review in review_list:
        user_list.append(review['user_id'])
    return list(set(user_list))

'''
takes a list of user ids and returns a list of dictionaries containing user objects
'''
def get_user_by_id(user_id_list):
    user_list = []
    with open('../../yelp_raw_data/yelp_academic_dataset_user.json') as user_data:
        for user in user_data:
            if json.loads(user)['user_id'] in user_id_list:
                user_list.append(json.loads(user))
    return user_list

def get_restaurant_by_id(id_number):
    restaurant_list = []
    with open('../../yelp_raw_data/yelp_academic_dataset_business.json') as restaurant_data:
        for restaurant in restaurant_data:
            if json.loads(restaurant)['business_id'] == id_number:
                return json.loads(restaurant)['name']
    return "Not found"

def parse_csv_ratings(csv_file, outfile):
    restaurant_list = []
    with open(csv_file,'r') as ratings_data:
        reader = csv.reader(ratings_data)
        with open(outfile,'w') as out_data:
            writer = csv.writer(out_data)
            for restaurant_id,rating in reader:
                writer.writerow((get_restaurant_by_id(restaurant_id).encode('utf-8'), rating))

def create_charlotte_restaurant_reviews():
    logging.basicConfig(filename='configure_data.log', level=logging.DEBUG)
    businesses = filter_business_by_city(load_business(), 'Charlotte')
    businesses = filter_business_by_category(businesses, 'Restaurants')
    business_ids = get_business_ids(businesses)
    obj = []
    for business_id in business_ids:
        obj = filter_reviews_by_restaurant(business_id, obj)
        logging.info('Done with business id: {0}'.format(str(business_id)))
    with open('yelp_academic_dataset_review_charlotte_restaurants.json','a') as outfile:
        for line in obj:
            json.dump(line, outfile)
            outfile.write('\n')



class UserRestaurantGraph:
    def __init__(self, edge_list=[]):
        self.edge_list = edge_list
        pass

    def add_edge(self, user, restaurant, weight):
        self.edge_list.append((user, restaurant, weight),)

    def populate_graph_with_reviews(self, review_list):
        for review in review_list:
            if int(review['stars']) >= 3:
                self.add_edge(str(review['user_id']), str(review['business_id']), 1)
            # self.add_edge(str(review['user_id']), str(review['business_id']), int(int(review['stars']) >= 3))

    def write_to_file(self, outfile):
        with open(outfile,'w') as writefile:
            writer = csv.writer(writefile)
            for edge in self.edge_list:
                writer.writerow(edge)

    def rand_split(self, fraction):
        edge_nos = np.random.choice(a=len(self.edge_list)-1,size=len(self.edge_list),replace=True)
        curr_edge_list = np.asarray(self.edge_list)[edge_nos]
        pivot_point = int(fraction*len(curr_edge_list))
        chunk_one = curr_edge_list[0:pivot_point]
        chunk_two = curr_edge_list[pivot_point:len(curr_edge_list)]
        graph_one = UserRestaurantGraph(edge_list=chunk_one)
        graph_two = UserRestaurantGraph(edge_list=chunk_two)
        return graph_one, graph_two

def main2():
    parse_csv_ratings('nbi_ratings.csv','nbi_rating_output.csv')
    parse_csv_ratings('cf_ratings.csv','cf_output.csv')

def main():
    # create_charlotte_restaurant_reviews()
    charlotte_restaurant_reviews = load_reviews_charlotte_restaurants()
    graph = UserRestaurantGraph()
    graph.populate_graph_with_reviews(charlotte_restaurant_reviews)
    graph.write_to_file('review_edges.csv')
    graph_train, graph_test = graph.rand_split(0.9)
    graph_train.write_to_file('review_edges_train.csv')
    graph_test.write_to_file('review_edges_test.csv')

if __name__ == "__main__":
    main()