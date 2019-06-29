def natural_language_join(phrases):
    assert phrases
    if len(phrases) == 1:
        return phrases[0]
    if len(phrases) == 2:
        return "{} and {}".format(*phrases)
    return "{}, and {}".format(", ".join(phrases[:-1]), phrases[-1])
