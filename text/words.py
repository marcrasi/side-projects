import argparse
import os
import pickle
import re
import random
from collections import defaultdict


from keras.callbacks import ModelCheckpoint
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.layers import LSTM
from keras.optimizers import RMSprop, Adam
from keras.utils.data_utils import get_file
import numpy as np
import random
import sys
import math


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'action',
        choices=[
            'train',
            'predict',
            'predict_markov',
            'mh',
            'search_sentences',
            'synonyms',
        ])
    return parser.parse_args()


def get_texts():
    URLS = [
        "http://www.gutenberg.org/cache/epub/29132/pg29132.txt",
        "http://www.gutenberg.org/cache/epub/28767/pg28767.txt",
        "http://www.gutenberg.org/cache/epub/30255/pg30255.txt",
        "http://www.gutenberg.org/cache/epub/32154/pg32154.txt",
        "http://www.gutenberg.org/cache/epub/41562/pg41562.txt",
        "http://www.gutenberg.org/cache/epub/32832/pg32832.txt",
        "http://www.gutenberg.org/cache/epub/40964/pg40964.txt",
        "http://www.gutenberg.org/cache/epub/583/pg583.txt",
        "http://www.gutenberg.org/files/155/155-0.txt",
        #"http://www.gutenberg.org/files/1626/1626-0.txt",
        #"http://www.gutenberg.org/files/1438/1438-0.txt",
        #"http://www.gutenberg.org/files/1895/1895-0.txt",
        #"http://www.gutenberg.org/files/1622/1622-0.txt",
    ]

    texts = []
    for index, url in enumerate(URLS):
        path = get_file('book%d.txt' % index, origin=url)
        text = open(path).read()
        texts.append(text)
    return texts

def extract_sentences(text):
    START_MARKER = r"\*\*\* START OF THIS PROJECT GUTENBERG EBOOK [ A-Z]+ \*\*\*"
    END_MARKER = r"\*\*\* END OF THIS PROJECT GUTENBERG EBOOK [ A-Z]+ \*\*\*"
    SPACELIKE_CHARACHTERS = "[-\n]"
    CHARACTER_BLACKLIST = r"[^.!?a-z ]"
    SENTENCE_SEPARATORS = r"[.!?]"
    WORD_SEPARATORS = r" "
    MINIMUM_SENTENCE_LENGTH = 4

    # Extract the body of the text
    start_match = re.search(START_MARKER, text)
    if start_match is None:
        raise Exception('Start marker not found')

    end_match = re.search(END_MARKER, text)
    if end_match is None:
        raise Exception('End marker not found')

    body = text[start_match.end():end_match.start()]

    # Normalize the body
    body = body.lower()
    body = re.sub(SPACELIKE_CHARACHTERS, ' ', body)
    body = re.sub(CHARACTER_BLACKLIST, '', body)

    # Extract the sentences
    sentences = re.split(SENTENCE_SEPARATORS, body)

    # Extract the words
    sentences = [
        [word for word in re.split(WORD_SEPARATORS, sentence) if len(word) > 0]
        for sentence
        in sentences
    ]

    # Filter the sentences
    sentences = [
        sentence
        for sentence
        in sentences
        if len(sentence) >= MINIMUM_SENTENCE_LENGTH
    ]

    return sentences


def compute_vocabulary(sentences):
    vocabulary = {
        'PAD': 0,
        'START': 1,
        'END': 2,
    }
    current_index = 3
    for sentence in sentences:
        for word in sentence:
            if word not in vocabulary:
                vocabulary[word] = current_index
                current_index += 1
    return vocabulary


def print_statistics(sentences):
    print '%d sentences' % len(sentences)
    print '%d words' % sum([len(sentence) for sentence in sentences])
    print '%d distinct words' % len(compute_vocabulary(sentences))


def load_glove(vocabulary):
    CHARACTER_WHITELIST = r"[a-z]"
    EMBEDDING_DIMENSION = 100
    vocabulary = dict(vocabulary)
    glove = np.zeros((EMBEDDING_DIMENSION + 3, len(vocabulary)))

    # Give the control words special embeddings
    for word in ['PAD', 'START', 'END']:
        glove[EMBEDDING_DIMENSION + vocabulary[word], vocabulary[word]] = 1
        del vocabulary[word]

    # Load from glove
    with open('glove.6B.%dd.txt' % EMBEDDING_DIMENSION, 'r') as f:
        for line in f:
            sp = line.split(' ')
            if sp[0] in vocabulary:
                vocabulary_index = vocabulary[sp[0]]
                for embedding_component in range(EMBEDDING_DIMENSION):
                    glove[embedding_component, vocabulary_index] = \
                        float(sp[embedding_component + 1])
                del vocabulary[sp[0]]

    # Give words not in glove a random embedding
    for word in vocabulary:
        vocabulary_index = vocabulary[word]
        for embedding_component in range(EMBEDDING_DIMENSION):
            glove[embedding_component, vocabulary_index] = \
                2.0 * np.random.rand() + 1.0

    return glove


def load_embedding(vocabulary):
    try:
        f = open('embedding.pkl', 'r')
        embedding_data = pickle.load(f)
        f.close()
        # TODO: Check if vocabulary is the same and if not, reload.
        return embedding_data['embedding']
    except IOError:
        embedding = load_glove(vocabulary)
        embedding_data = {
            'embedding': embedding,
            'vocabulary': vocabulary
        }
        f = open('embedding.pkl', 'w')
        pickle.dump(embedding_data, f)
        return embedding


def invert_vocabulary(vocabulary):
    inverse_vocabulary = {}
    for word in vocabulary:
        inverse_vocabulary[vocabulary[word]] = word
    return inverse_vocabulary


def embed(embedding, vocabulary, word):
    return embedding[:, vocabulary[word]]


def embed_sequence(embedding, vocabulary, sequence):
    return ([
        embed(embedding, vocabulary, word)
        for word
        in sequence
    ])


def find_synonyms(embedding, vocabulary, inverse_vocabulary, word):
    normalized_embedding = embedding.copy()
    for i in range(normalized_embedding.shape[1]):
        normalized_embedding[:, i] /= np.linalg.norm(normalized_embedding[:, i])

    embedded_word = embed(embedding, vocabulary, word)
    cosine_similarities = np.dot(np.transpose(normalized_embedding), embedded_word)
    closests = np.argsort(cosine_similarities)[-4:-1]
    synonyms = set([inverse_vocabulary[closest] for closest in closests])
    synonyms.add(word)
    return list(synonyms)


def sample(preds, temperature=1.0):
    # helper function to sample an index from a probability array
    preds = np.asarray(preds).astype('float64')
    preds = np.log(preds) / temperature
    exp_preds = np.exp(preds)
    preds = exp_preds / np.sum(exp_preds)
    probas = np.random.multinomial(1, preds, 1)
    return np.argmax(probas)


def predict_general(generate_p, inverse_vocabulary, temperature):
    sentence = ['PAD'] * (SEQUENCE_LENGTH - 2) + ['START']
    while sentence[-1] != 'END' and len(sentence) < SEQUENCE_LENGTH + 50:
        next_word = inverse_vocabulary[sample(generate_p([sentence])[0], 1.0)]
        sentence.append(next_word)
    return sentence[SEQUENCE_LENGTH - 1 : -1]


def seeded_predict(generate_p, inverse_vocabulary, temperature, seed, length):
    if len(seed) < SEQUENCE_LENGTH - 1:
        seed = ['PAD'] * (SEQUENCE_LENGTH - 2 - len(seed)) + ['START'] + seed
    sentence = list(seed)
    while len(sentence) < len(seed) + length:
        next_word = None
        while next_word is None or next_word == 'END':
            next_word = inverse_vocabulary[sample(generate_p([sentence])[0], 1.0)]
        sentence.append(next_word)
    return sentence[len(seed):]


def generate_p_lstm(model, embedding, vocabulary):
    def generate_p(sentences):
        embedded_sequences = []
        for sentence in sentences:
            sequence = sentence[-(SEQUENCE_LENGTH - 1):]
            embedded_sequences.append(embed_sequence(embedding, vocabulary, sequence))
        return model.predict(np.array(embedded_sequences), verbose=0)
    return generate_p


def generate_p_markov(markov, vocabulary):
    order = markov['order']
    elements = markov['elements']
    def generate_p(sentences):
        predictions = []
        for sentence in sentences:
            element = elements
            for i in range(order):
                element = element[vocabulary[sentence[i - order]]]
            unnormalized = np.array([element[i] for i in range(len(elements))])
            predictions.append(unnormalized / sum(unnormalized))
        return predictions
    return generate_p


def print_some_predictions(generate_p, vocabulary, inverse_vocabulary):
    for temperature in [0.2, 0.5, 1.0]:
        print 'Temperature %.1f' % temperature
        for _ in range(4):
            sentence = predict_general(generate_p, inverse_vocabulary, temperature)
            print ' '.join(sentence)
            print '\t log likelihood: %.2f' % log_likelihood(generate_p, vocabulary, sentence)
        print ''


def train_markov(order, vocabulary, sequences):
    SMOOTHING = 0.1 / len(vocabulary)
    if order == 0:
        elements = defaultdict(lambda: SMOOTHING)
    elif order == 1:
        elements = defaultdict(lambda: defaultdict(lambda: SMOOTHING))
    elif order == 2:
        elements = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: SMOOTHING)))
    elif order == 3:
        elements = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: SMOOTHING))))
    elif order == 4:
        elements = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: SMOOTHING)))))
    elif order == 5:
        elements = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: SMOOTHING))))))
    elif order == 6:
        elements = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: SMOOTHING))))))
    else:
        raise Exception('Order %d Markov chains not implemented', order)
    for sequence in sequences:
        element = elements
        for i in range(order):
            element[vocabulary[sequence[i - order - 1]]] = \
                element[vocabulary[sequence[i - order - 1]]]
            element = element[vocabulary[sequence[i - order - 1]]]
        element[vocabulary[sequence[-1]]] += 1
    return {
        'order': order,
        'elements': elements
    }


def construct_sequences(sentences, min_length=2):
    sequences = []
    for sentence_without_terminators in sentences:
        sentence = ['START'] + sentence_without_terminators + ['END']
        for end in range(min_length, len(sentence) + 1):
            sequences.append(sentence[:end])
    return sequences


def log_likelihood(generate_p, vocabulary, sentence, seed=[]):
    sequences = construct_sequences([seed + sentence])[len(seed):]
    in_sequences = [sequence[:-1] for sequence in sequences]
    out_words = [sequence[-1] for sequence in sequences]
    predictions = generate_p(in_sequences)
    result = 0.0
    for out_word, prediction in zip(out_words, predictions):
        result += np.log(prediction[vocabulary[out_word]])
    return result


def logfactorial(k):
    result = 0.0
    for i in range(1, k + 1):
        result += np.log(i)
    return result


def sentence_length_log_likelihood(mean_length, length):
    return length * np.log(mean_length) - mean_length - logfactorial(length)


def initial_proposal(generate_p, vocabulary, inverse_vocabulary, constraint_words, length):
    TRIES = 10
    candidates = []
    for _ in range(TRIES):
        constraint_word_positions = sorted((np.random.choice(
            range(length), len(constraint_words), replace=False)))
        candidate = []
        for constraint_word, constraint_word_position in zip(constraint_words + ['DUMMY'], constraint_word_positions + [length]):
            candidate += seeded_predict(generate_p, inverse_vocabulary, 1.0, candidate, constraint_word_position - len(candidate))
            if constraint_word != 'DUMMY':
                candidate.append(constraint_word)
        candidates.append(candidate)

    best_log_likelihood = None
    best_candidate = None
    for candidate in candidates:
        candidate_ll = log_likelihood(generate_p, vocabulary, candidate)
        if best_log_likelihood is None or candidate_ll > best_log_likelihood:
            best_log_likelihood = candidate_ll
            best_candidate = candidate

    return best_candidate


def first_occurence(supersequence, query_words):
    first_index = min([
        supersequence.index(query_word) if query_word in supersequence else 1000
        for query_word
        in query_words
    ])
    if first_index == 1000:
        return None
    else:
        return first_index


def is_subsequence(supersequence, subsequence):
    match = True
    previous_word_index = -1
    for query_words in subsequence:
        if not isinstance(query_words, list):
            query_words = [query_words]
        first_index = first_occurence(supersequence, query_words)
        if first_index is None:
            match = False
            break
        elif first_index < previous_word_index:
            match = False
            break
        else:
            previous_word_index = first_index
    return match


def next_proposal(generate_p, inverse_vocabulary, constraint_words, proposal):
    MIN_LENGTH = 5
    MAX_LENGTH = 100
    MAX_CHANGE = 10

    length = len(proposal)

    def transpose():
        bounds = sorted(np.random.choice(range(length + 1), 4, replace=False))
        a = proposal[:bounds[0]]
        b = proposal[bounds[0]:bounds[1]]
        c = proposal[bounds[1]:bounds[2]]
        d = proposal[bounds[2]:bounds[3]]
        e = proposal[bounds[3]:]
        return (a + d + c + b + e, 0.0)

    # This is not symmetric with itself because it might replace a sequence with a different
    # one that is more or less likely.
    def replace():
        to_replace = np.random.choice(range(1, min(MAX_CHANGE, length + 1)))
        start_replace = np.random.choice(range(length + 1 - to_replace))
        a = proposal[:start_replace]
        b = seeded_predict(generate_p, inverse_vocabulary, 1.0, a, to_replace)
        c = proposal[start_replace + to_replace:]

        b_original = proposal[start_replace : start_replace + to_replace]
        ratio_adjustment = log_likelihood(generate_p, vocabulary, b_original, a) - log_likelihood(generate_p, vocabulary, b, a)

        return (a + b + c, ratio_adjustment)

    def add():
        max_add = min(MAX_CHANGE, MAX_LENGTH - length)
        if max_add == 0:
            return ([], 0.0)
        else:
            to_add = np.random.choice(range(1, max_add + 1))
            start_add = np.random.choice(range(length + 1))
            a = proposal[:start_add]
            b = seeded_predict(generate_p, inverse_vocabulary, 1.0, a, to_add)
            c = proposal[start_add:]

            ratio_adjustment = -log_likelihood(generate_p, vocabulary, b, a)

            return (a + b + c, ratio_adjustment)

    def remove():
        max_remove = min(MAX_CHANGE, length - MIN_LENGTH)
        if max_remove == 0:
            return ([], 0.0)
        else:
            to_remove = np.random.choice(range(1, max_remove + 1))
            start_remove = np.random.choice(range(length - to_remove))
            a = proposal[:start_remove]
            b = proposal[start_remove + to_remove:]
            return (a + b, 0.0)

    # THIS ONE IS NOT ACTIVE RIGHT NOW
    def switch_to_synonym():
        constraint_to_switch = random.choice(constraint_words)
        word_index = first_occurence(proposal, constraint_to_switch)
        switch_to = random.choice(constraint_to_switch)
        a = proposal[:word_index]
        b = [switch_to]
        c = proposal[word_index+1:]
        return (a + b + c, 0.0)

    transitions = [transpose, replace, add, remove]
    candidate_proposal = None
    candidate_ratio_adjustment = None
    while candidate_proposal is None or not is_subsequence(candidate_proposal, constraint_words) or candidate_proposal == proposal:
        transition = random.choice(transitions)
        candidate_proposal, candidate_ratio_adjustment = transition()
    return candidate_proposal, candidate_ratio_adjustment

args = parse_args()

# Load stuff
texts = get_texts()
sentences = []
for text in texts:
    sentences.extend(extract_sentences(text))
vocabulary = compute_vocabulary(sentences)
inverse_vocabulary = invert_vocabulary(vocabulary)
print_statistics(sentences)

embedding = load_embedding(vocabulary)

# Constants
SEQUENCE_LENGTH = 20
EMBEDDING_DIMENSION = embedding.shape[0]
LSTM_DIMENSION = 256
OUTPUT_DIMENSION = len(vocabulary)
BEST_CHECKPOINT_FILE = 'weights.best.hdf5'
BATCH_SIZE = 128

print 'SEQUENCE_LENGTH = %d' % SEQUENCE_LENGTH
print 'EMBEDDING_DIMENSION = %d' % EMBEDDING_DIMENSION
print 'LSTM_DIMENSION = %d' % LSTM_DIMENSION
print 'OUTPUT_DIMENSION = %d' % OUTPUT_DIMENSION
print 'BATCH_SIZE = %d' % BATCH_SIZE

# Model
model = Sequential()
model.add(LSTM(LSTM_DIMENSION, input_shape=(SEQUENCE_LENGTH - 1, EMBEDDING_DIMENSION), consume_less='gpu', stateful=True))
model.add(Dense(OUTPUT_DIMENSION))
model.add(Activation('softmax'))

if os.path.isfile(BEST_CHECKPOINT_FILE):
    model.load_weights(BEST_CHECKPOINT_FILE)

best_checkpointer = ModelCheckpoint(BEST_CHECKPOINT_FILE)
checkpointers = [best_checkpointer]

optimizer = Adam()
model.compile(loss='categorical_crossentropy', optimizer=optimizer)

if args.action == 'train':
    sequences = construct_sequences(sentences)

    embedded_sequences = np.array([
        np.array(embed_sequence(embedding, vocabulary, sequence))
        for sequence
        in sequences
    ])

    X = embedded_sequences[:, :SEQUENCE_LENGTH - 1, :]
    y = np.zeros((len(sequences), OUTPUT_DIMENSION))
    for example_index, sequence in enumerate(sequences):
        y[example_index, vocabulary[sequence[SEQUENCE_LENGTH - 1]]] = 1

    for iteration in range(1, 500):
        print 'Iteration %d' % iteration
        model.fit(X, y, batch_size=64, nb_epoch=1, callbacks=checkpointers)
        print_some_predictions(generate_p_lstm(model, embedding, vocabulary), vocabulary, inverse_vocabulary)
        print ''

elif args.action == 'predict':
    print_some_predictions(generate_p_lstm(model, embedding, vocabulary), vocabulary, inverse_vocabulary)

elif args.action == 'predict_markov':
    sequences = construct_sequences(sentences, 3)
    markov = train_markov(2, vocabulary, sequences)
    print_some_predictions(generate_p_markov(markov, vocabulary), vocabulary, inverse_vocabulary)

elif args.action == 'mh':
    MEAN_LENGTH = 5

    query = raw_input('Your query? ')
    query_words = query.split(' ')

    #query_words_with_synonyms = [
    #    find_synonyms(embedding, vocabulary, inverse_vocabulary, query_word)
    #    for query_word
    #    in query_words
    #]
    query_words_with_synonyms = [[x] for x in query_words]

    proposal = initial_proposal(
        generate_p_lstm(model, embedding, vocabulary), vocabulary, inverse_vocabulary, query_words, 10)
    proposal_ll = log_likelihood(
        generate_p_lstm(model, embedding, vocabulary), vocabulary, proposal) + \
        sentence_length_log_likelihood(MEAN_LENGTH, len(proposal))

    iteration = 0
    changed = True
    while True:
        if changed:
            print 'iteration %d: %s (%.2f)' % (iteration, ' '.join(proposal), proposal_ll)
            changed = False

        candidate_proposal, candidate_ratio_adjustment = next_proposal(
            generate_p_lstm(model, embedding, vocabulary), inverse_vocabulary, query_words_with_synonyms, proposal)
        candidate_proposal_ll = log_likelihood(
            generate_p_lstm(model, embedding, vocabulary), vocabulary, candidate_proposal) + \
            sentence_length_log_likelihood(MEAN_LENGTH, len(candidate_proposal))

        p_accept = np.exp(candidate_proposal_ll - proposal_ll + candidate_ratio_adjustment)
        if np.random.rand() < p_accept:
            proposal = candidate_proposal
            proposal_ll = candidate_proposal_ll
            changed = True

        iteration += 1


elif args.action == 'search_sentences':
    query = raw_input('Your query? ')
    query_words = query.split(' ')
    for sentence in sentences:
        if is_subsequence(sentence, query_words):
            print sentence

elif args.action == 'synonyms':
    while True:
        query = raw_input('Synonyms of what? ')
        print find_synonyms(embedding, vocabulary, inverse_vocabulary, query)
