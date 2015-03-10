# -*- coding: utf-8 -*-
from gensim import corpora, models, similarities
from prettyprint import pp
import MeCab

def tokenize(string):
    tagger = MeCab.Tagger("-Owakati")
    stoplist = set("は の です ます が".split())
    texts = [[t for t in tagger.parse(s).split() if t not in stoplist] for s in string.split("。")]
    all_tokens = sum(texts, [])
    tokens_once = set(word for word in set(all_tokens) if all_tokens.count(word) == 1)
    texts = [[word for word in text if word not in tokens_once] for text in texts]
    dictionary = corpora.Dictionary(texts)
    corpus = [dictionary.doc2bow(text) for text in texts]
    return corpus

def fromfile(filename):
    return open(filename, "r").read()$
