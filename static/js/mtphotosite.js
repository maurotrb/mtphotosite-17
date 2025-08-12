'use strict';

// Listener with various actions
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
  const infoBtn = document.getElementById("toggle-info");
  const infoSect = document.getElementById("photo-info");

  if (infoBtn && infoSect) {
    // Initially hide the info when JS is enabled
    infoBtn.textContent = "show photo info";
    infoSect.classList.add("is-hidden");

    // Toggle photo info section on/off
    infoBtn.addEventListener("click", () => {
      infoSect.classList.toggle("is-hidden");
      if (infoSect.classList.contains("is-hidden")) {
        infoBtn.textContent = "show photo info";
      } else {
        infoBtn.textContent = "hide photo info";
      }
    });
  }

  //// CONTEXT NAV ////
  const ctxRaw = sessionStorage.getItem('photoContext');
    if (!ctxRaw) {
    // No context — hide nav controls entirely
    document.querySelector('.nav-links')?.remove();
    return;
  }

  const ctx = JSON.parse(ctxRaw);

  fetch(ctx.jsonUrl)
    .then(res => res.json())
    .then(list => {
      const currentUrl = window.location.href;
      const index = list.findIndex(p => p.url === currentUrl);

      if (index === -1) return; // Current photo not found in list

      // Breadcrumb element
      const breadcrumb = document.querySelector('#breadcrumb');

      if (breadcrumb) {
        if (ctx.contextType === 'home') {
          // Home page special case — only 2 levels
          breadcrumb.innerHTML = `
            <ul>
              <li><a href="/">home</a></li>
              <li class="is-active"><a aria-current="page">${list[index].title}</a></li>
            </ul>
          `;
        } else {
          // Galleries / Tags normal 3-level breadcrumb
          breadcrumb.innerHTML = `
            <ul>
              <li><a href="/">home</a></li>
              <li><a href="/${ctx.contextType}/${ctx.contextName}/">${ctx.contextName.replace(/-/g, ' ')}</a></li>
              <li class="is-active"><a aria-current="page">
                ${list[index].title}
              </a></li>
            </ul>
          `;
        }
      }

      // Nav buttons
      const navContainer = document.querySelector('.nav-links');
      navContainer.innerHTML = ''; // clear template

      // Previous button
      if (index > 0) {
        const prevBtn = document.createElement('a');
        prevBtn.href = list[index - 1].url;
        prevBtn.className = 'button is-ghost';
        prevBtn.innerHTML = '<span class="icon"><i class="fas fa-caret-left"></i></span><span>prev</span>';
        navContainer.appendChild(prevBtn);
      }

      // Next button
      if (index < list.length - 1) {
        const nextBtn = document.createElement('a');
        nextBtn.href = list[index + 1].url;
        nextBtn.className = 'button is-ghost';
        nextBtn.innerHTML = '<span>next</span><span class="icon"><i class="fas fa-caret-right"></i></span>';
        navContainer.appendChild(nextBtn);
      }

      // Keyboard navigation
      document.addEventListener('keydown', e => {
        if (e.key === 'ArrowLeft' && index > 0) {
          window.location.href = list[index - 1].url;
        } else if (e.key === 'ArrowRight' && index < list.length - 1) {
          window.location.href = list[index + 1].url;
        }
      });

      // Swipe navigation (mobile/tablet)
      let touchStartX = 0;
      let touchEndX = 0;

      document.addEventListener('touchstart', e => {
        touchStartX = e.changedTouches[0].screenX;
      }, { passive: true });

      document.addEventListener('touchend', e => {
        touchEndX = e.changedTouches[0].screenX;
        handleSwipe();
      }, { passive: true });

      function handleSwipe() {
        const swipeDistance = touchEndX - touchStartX;
        if (Math.abs(swipeDistance) > 50) {
          if (swipeDistance > 0 && index > 0) {
            // Swipe right → previous photo
            window.location.href = list[index - 1].url;
          } else if (swipeDistance < 0 && index < list.length - 1) {
            // Swipe left → next photo
            window.location.href = list[index + 1].url;
          }
        }
      }

    })
    .catch(err => console.error('Error loading context JSON:', err));
});

//// CONTEXT NAV ////
document.querySelectorAll('a.photo-link').forEach(link => {
  link.addEventListener('click', () => {
    const contextType = link.dataset.contextType || 'home';
    const contextName = link.dataset.contextName || '';
    let jsonUrl = '';

    if (contextType === 'home') {
      jsonUrl = '/index.json'; // virtual home/featured list
    } else {
      jsonUrl = `/${contextType}/${contextName}/index.json`;
    }

    sessionStorage.setItem('photoContext', JSON.stringify({
      contextType,
      contextName,
      jsonUrl
    }));
  });
});
