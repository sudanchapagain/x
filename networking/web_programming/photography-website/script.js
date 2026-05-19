var toggleMenu = document.querySelector(".toggle-menu");
toggleMenu.addEventListener("click", function () {
  if (this.nextElementSibling.classList.contains("expand")) {
    this.nextElementSibling.classList.remove("expand");
  } else {
    this.nextElementSibling.classList.add("expand");
  }
});
