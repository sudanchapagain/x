import emb
import ctypes


class GameState(ctypes.Structure):
    _fields_ = [
        ("health", ctypes.c_int),
        ("score", ctypes.c_int),
    ]


def modify_state(ptr_value):
    capsule = emb.get_state(ptr_value)
    raw_ptr = ctypes.c_void_p.from_address(id(capsule)).value
    state = ctypes.cast(ptr_value, ctypes.POINTER(GameState)).contents

    print(f"python: health={state.health} score={state.score}")

    state.health -= 25
    state.score += 999

    print("python: mutated the state")
