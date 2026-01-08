from django.urls import path
from . import views

urlpatterns = [
    path("counter/", views.bit_counter),
]
