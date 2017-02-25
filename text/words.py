
import re

from keras.utils.data_utils import get_file

def get_texts():
    URLS = [
        "http://www.gutenberg.org/cache/epub/29132/pg29132.txt",
        "http://www.gutenberg.org/cache/epub/28767/pg28767.txt",
        "http://www.gutenberg.org/cache/epub/30255/pg30255.txt",
        "http://www.gutenberg.org/cache/epub/32154/pg32154.txt",
        "http://www.gutenberg.org/cache/epub/41562/pg41562.txt",
        "http://www.gutenberg.org/cache/epub/32832/pg32832.txt",
        "http://www.gutenberg.org/cache/epub/40964/pg40964.txt",
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


def print_statistics(sentences):
    print '%d sentences' % len(sentences)
    print '%d words' % sum([len(sentence) for sentence in sentences])
    print '%d distinct words' % len(set((word for sentence in sentences for word in sentence)))


texts = get_texts()
sentences = []
for text in texts:
    sentences.extend(extract_sentences(text))
print_statistics(sentences)
