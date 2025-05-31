package exceptions

class InvalidPhaseTransitionException extends IllegalStateException(
    "Invalid phase transition attempted."
)