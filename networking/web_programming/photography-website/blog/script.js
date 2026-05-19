document.addEventListener("DOMContentLoaded", (event) => {
  let currentPostIndex = 0;
  const blogPosts = document.querySelectorAll(".blog-post");
  const nextArrow = document.getElementById("next-arrow");
  const prevArrow = document.getElementById("prev-arrow");

  function showPost(index) {
    blogPosts[currentPostIndex].classList.remove("active");
    currentPostIndex = index;
    blogPosts[currentPostIndex].classList.add("active");
  }

  function showNextPost() {
    showPost((currentPostIndex + 1) % blogPosts.length);
  }

  function showPrevPost() {
    showPost((currentPostIndex - 1 + blogPosts.length) % blogPosts.length);
  }

  nextArrow.addEventListener("click", showNextPost);
  prevArrow.addEventListener("click", showPrevPost);

  setInterval(showNextPost, 7000);
});

// NewsLetter Form
function toggleNewsletterForm() {
  var newsletterForm = document.getElementById("newsletter-form");
  if (newsletterForm.style.display === "none") {
    newsletterForm.style.display = "block";
  } else {
    newsletterForm.style.display = "none";
  }
}

function closeNewsletterForm() {
  var newsletterForm = document.getElementById("newsletter-form");
  newsletterForm.style.display = "none";
}
