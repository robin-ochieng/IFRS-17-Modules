// Real-time clock script
function updateClock() {
    const clockElement = document.getElementById("dynamic-clock");
    if (clockElement) {
      const now = new Date();
      const timeString = now.toLocaleTimeString([], { 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit' 
      });
      clockElement.textContent = timeString;
    }
  }
  
  // Update the clock every second
  setInterval(updateClock, 1000);


  $(document).on('blur', '.format-number', function() {
    var num = $(this).val().replace(/,/g, '');
    if (!isNaN(num) && num !== '') {
      $(this).val(Number(num).toLocaleString());
    }
  });


  // www/js/custom.js
Shiny.addCustomMessageHandler('scrollToElement', function(id) {
  var el = document.getElementById(id);
  if (el) {
    el.scrollIntoView({ behavior: 'smooth' });
  }
});


$(function () {
  // Hide ANY open popover when the click target is
  //   – NOT the icon itself ('.info-icon'), and
  //   – NOT inside an already-open popover ('.popover')
  $('body').on('click', function (e) {
    if (!$(e.target).closest('.popover, .info-icon').length) {
      $('.info-icon').popover('hide');
    }
  });
});

// this runs when R calls session$sendCustomMessage("printPage",…)
Shiny.addCustomMessageHandler("printPage", function(message) {
  window.print();
});



$(document).ready(function() {
  // Function to adjust menu container position based on sidebar state
  function adjustMenuContainer() {
    var isCollapsed = $('body').hasClass('sidebar-collapse');
    var menuContainer = $('.menu-container');
    var logos = $('.logos');
    
    if (isCollapsed) {
      // Sidebar is collapsed
      menuContainer.css({
        'left': '0',
        'width': '60px',
        'overflow': 'hidden' 
      });
      
      logos.css({
        'left': '0',
        'width': '60px',
        'padding': '10px 5px'
      });
      
      // Hide text in menu items
      $('.menu-container .nav-link > p').css('display', 'none');
      $('.menu-container .nav-link > span:not(.fa):not(.fas):not(.far)').css('display', 'none');
      $('.menu-container .nav-header').css('display', 'none');

      // Adjust menu items for collapsed state
      $('.menu-container .nav-item').css('white-space', 'nowrap');

    } else {
      // Sidebar is expanded
      menuContainer.css({
        'left': '0',
        'width': '260px',
        'overflow': 'visible'
      });
      
      logos.css({
        'left': '0',
        'width': '260px',
        'padding': '20px 10px 10px 10px'
      });
      
      // Show text elements with a slight delay to ensure proper rendering
      setTimeout(function() {
        $('.menu-container .nav-link > p').css('display', 'block');
        $('.menu-container .nav-link > span:not(.fa):not(.fas):not(.far)').css('display', 'inline');
        $('.menu-container .nav-header').css('display', 'block');
        
        // Reset menu items for expanded state
        $('.menu-container .nav-item').css('white-space', 'normal');
      }, 50);
    }
    
    // Force a reflow to ensure changes are applied
    menuContainer[0].offsetHeight;
  }
  
  // Listen for sidebar toggle button clicks
  $('[data-widget="pushmenu"]').on('click', function() {
    setTimeout(adjustMenuContainer, 350);  // Wait for animation
  });
  
  // Also listen for body class changes
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.attributeName === "class") {
        var oldClasses = mutation.oldValue || "";
        var newClasses = mutation.target.className;
        
        // Check if sidebar-collapse class was added or removed
        if (oldClasses.includes('sidebar-collapse') !== newClasses.includes('sidebar-collapse')) {
          setTimeout(adjustMenuContainer, 100);
        }
      }
    });
  });
  
  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['class'],
    attributeOldValue: true
  });
  
  // Initial adjustment
  setTimeout(adjustMenuContainer, 100);
  
  // Handle window resize
  $(window).on('resize', function() {
    setTimeout(adjustMenuContainer, 100);
  });
});

