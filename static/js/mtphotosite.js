'use strict';

document.addEventListener('DOMContentLoaded', function () {

  //// NAVBAR BURGER ////
  // Get all "navbar-burger" elements
  var $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  // Check if there are any navbar burgers
  if ($navbarBurgers.length > 0) {

    // Add a click event on each of them
    $navbarBurgers.forEach(function ($el) {
      $el.addEventListener('click', function () {

        // Get the target from the "data-target" attribute
        var target = $el.dataset.target;
        var $target = document.getElementById(target);

        // Toggle the class on both the "navbar-burger" and the "navbar-menu"
        $el.classList.toggle('is-active');
        $target.classList.toggle('is-active');

      });
    });
  }

  //// PHOTO INFO BUTTON ////
  const button = document.getElementById("toggle-info");
  const info = document.getElementById("photo-info");

  if (button && info) {
    button.addEventListener("click", () => {
      info.classList.toggle("is-hidden");
      const span = button.querySelector("span:last-child");
      if (info.classList.contains("is-hidden")) {
        span.textContent = "show photo info";
      } else {
        span.textContent = "hide photo info";
      }
    });
  }

});
