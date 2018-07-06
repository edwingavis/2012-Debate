###Edwin Gavis

import bs4
import nltk
import urllib3
import collections
import csv
import re
import string
from stemming.porter2 import stem

def main():
	nested = get_lists()
	statements = []
	stops = get_stemmed_stops()
	for i in range(len(nested)):
		statements.append(Statement(i, nested[i][0], nested[i][1], stops))
	unigrams, trigrams = count_unigrams_trigrams(statements)
	tri_list = list(trigrams.keys())
	uni_list = list(unigrams.keys())
	write_doc_matrix(statements, unigrams, trigrams)


def get_lists(): 
	filename = 'Debate1.html'
	html = open(filename).read()
	soup = bs4.BeautifulSoup(html, "lxml")
	nested = []
	running = []
	moderator = False
	regcan = re.compile(r'(OBAMA:)|(ROMNEY:)')
	regmod = re.compile(r'(LEHRER:)')
	regfill = re.compile(r'^\(|[A-Z] [A-Z]|(2012)|(\n)')
	regspeaker = re.compile(r'[AY]:')
	for line in soup.find_all("p"):
		text = line.text
		if regcan.search(text):
			moderator = False
		if regmod.search(text):
			moderator = True
		if len(text) == 0 or regfill.search(text) or moderator:
			continue
		if not regspeaker.search(text):
			running[1] += text
		else:
			if len(running) != 0:
				nested.append(running)
			running = text.split(":")
	return nested



def get_stemmed_stops():
	http = urllib3.PoolManager()
	r = http.request('GET', 'http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop')
	cleaned_stops = str(r.data).replace("\\n", " ").split(" ")
	#set b/c order/count doesn't matter and to speed up searches later
	stemmed_stops = set()
	for word in cleaned_stops:
		stemmed_stops.add(word)
	return stemmed_stops



def count_unigrams_trigrams(statements):
	unigrams = collections.defaultdict(int)
	trigrams = collections.defaultdict(int)
	for statement in statements:
		for token in statement.tokens:
				unigrams[token] += 1
		for trigram in statement.trigrams:
				trigrams[trigram] += 1
	return unigrams, trigrams


def write_doc_matrix(statements, unigrams, trigrams):
	writing = ["Statement #", "Speaker"]
	for unigram in unigrams:
		writing.append(unigram)
	for trigram in trigrams:
		#may speed this up w/ concatenation
		tri_string = '.'.join(trigram)
		writing.append(tri_string)
	with open('debate1.csv', 'w', newline='') as csvfile:
		debatewriter = csv.writer(csvfile)
		debatewriter.writerow(writing)
		for statement in statements:
			writing = []
			writing.append(statement.number)
			writing.append(statement.speaker)
			for uni in unigrams:
				writing.append(statement.tokens.count(uni))
			for tri in trigrams:
				writing.append(statement.trigrams.count(tri))
			debatewriter.writerow(writing)


class Statement(object):
    def __init__(self, number, speaker, text, stops):
        '''
        Constructor 
        Inputs:
            number (int): the statement number
            speaker (string): the speaker
            text (string): the text
            stops (list of strings): list of stemmed stop words
        '''
        self.number = number
        self.speaker = speaker
        self.tokens = self.stem_text(self.prepare_text(text), stops)
        self.trigrams = list(nltk.trigrams(self.tokens))

    def prepare_text(self, text):
        depunctuator = str.maketrans('', '', string.punctuation)
        text = text.lower().translate(depunctuator)
        text_tokenized = nltk.tokenize.word_tokenize(text)
        return text_tokenized

    def stem_text(self, text, stops):
        stemmed_tokens = []
        for word in text:
      	    stemmed = stem(word)
      	    if stemmed not in stops: 
      		    stemmed_tokens.append(stemmed)
        return stemmed_tokens


if __name__=="__main__":
	main()
