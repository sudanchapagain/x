#include <Python.h>
#include <stdio.h>


typedef struct {
    int health;
    int score;
} GameState;


static PyObject* py_get_state(PyObject *self, PyObject *args) {
    GameState *state;

    if (!PyArg_ParseTuple(args, "k", &state)) {
        return NULL;
    }

    return PyCapsule_New(state, "GameState", NULL);
}


static PyMethodDef EmbMethods[] = {
    {
        "get_state",
        py_get_state,
        METH_VARARGS,
        "get a pointer to C GameState"
    },
    {
        NULL,
        NULL,
        0,
        NULL
    }
};


static struct PyModuleDef embmodule = {
    PyModuleDef_HEAD_INIT,
    "emb",
    NULL,
    -1,
    EmbMethods
};


PyMODINIT_FUNC PyInit_emb(void) {
    return PyModule_Create(&embmodule);
}


int main() {
    GameState state = { .health = 100, .score = 42 };

    printf("C: before python - health=%d score=%d\n", state.health, state.score);

    PyImport_AppendInittab("emb", PyInit_emb);
    Py_Initialize();

    PyObject *pyName = PyUnicode_FromString("script");
    PyObject *pyModule = PyImport_Import(pyName);
    if (!pyModule) {
        PyErr_Print();
        return 1;
    }

    PyObject *func = PyObject_GetAttrString(pyModule, "modify_state");
    PyObject *args = Py_BuildValue("(k)", (unsigned long)&state);
    PyObject *res = PyObject_CallObject(func, args);

    Py_DECREF(args);
    Py_DECREF(func);
    Py_DECREF(pyModule);
    Py_DECREF(pyName);

    printf("C: after python - health=%d score=%d\n", state.health, state.score);

    Py_Finalize();
    return 0;
}
